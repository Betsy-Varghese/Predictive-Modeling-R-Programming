##############################################PACKAGES

#For constructing the matrix and cleaning the data
library(recommenderlab)
library(dplyr)
library(tidytext)
library(reshape2)
library("tm")
library("SnowballC")
library("dbscan")
library("proxy")
library(tidyr)
library(OneR)
library(maditr)
library(Hmisc)

for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

##############################################FUNCTIONS

#Setting working directory for the Functions which already exist
setwd("C:/Users/ngzraryan/Desktop/Recommendation Tools/GP")
source("functions.R")

###Creating MAE evaluation function

MAE <- function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    MAE =  sum( abs(prediction - real) , na.rm = TRUE  / (nrow(prediction) * ncol(prediction)) )
    return(MAE)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

###Creating F1 evaluation function

F1 <- function(prediction, real, threshold=NA, TopN=NA) {
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real))
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      Class_Thres = list(Recall, Precision)
      names(Class_Thres) = c("Recall", "Precision")}
  if (!is.na(TopN)){
    TP = vector(, length = nrow(prediction))
    FP = vector(, length = nrow(prediction))
    FN = vector(, length = nrow(prediction))
    for (i in nrow(prediction)){
      threshold_pred = -sort(-prediction[i, ])[TopN]
      threshold_real = -sort(-real[i, ])[TopN]
      TP[i] = sum(ifelse(prediction[i, ] >= threshold_pred & real[i, ] >= threshold_real, 1, 0), na.rm=T)
      FP[i] = sum(ifelse(prediction[i, ] >= threshold_pred & real[i, ] < threshold_real, 1, 0), na.rm=T)
      FN[i] = sum(ifelse(prediction[i, ] < threshold_pred & real[i, ] >= threshold_real, 1, 0), na.rm=T)
    }
    TP = sum(TP[i])
    FP = sum(FP[i])
    FN = sum(FN[i])
    Recall = TP/(TP+FN)
    Precision = TP/(TP+FP)
    F1_value=2*((Precision*Recall)/(Precision+Recall))
    return(F1_value)
  }}

###Creating cluster based function

ClusterBasedCF <- function(data, N, centers, iter, onlyNew=TRUE){
  
  data2 <- data
  
  # fill with average product rating
  colmeans <- colMeans(data2, na.rm=TRUE)
  
  for (j in colnames(data2)){
    data2[, j] <- ifelse(is.na(data2[ ,j]), colmeans[j], data2[, j])
  }
  
  km <- kmeans(data2, centers=centers, iter.max=iter)
  
  head(km$cluster)
  head(km$centers)
  
  
  # Statistics of the groups
  tab <- table(km$cluster)
  
  # Assign users to groups
  RES <- cbind(data, as.data.frame(km$cluster))
  
  # Calculate average ratings for everi cluster
  aggregation <- aggregate(RES, list(RES$"km$cluster"), mean, na.rm=T)
  aggregation <- aggregation[,-1]
  
  # Make a prediction
  users <- as.data.frame(RES$"km$cluster")
  users <- cbind(users, rownames(RES))
  colnames(users) <- c("km$cluster", 'rn')
  
  
  prediction = merge(users, aggregation, by="km$cluster")
  rownames(prediction) <- prediction$rn
  
  prediction  <- prediction[order(rownames(prediction)), -1:-2]
  
  prediction2 <- matrix(, nrow=nrow(prediction), ncol(prediction), 
                        dimnames=list(rownames(prediction), colnames(prediction)))
  colnames(prediction2) <- colnames(prediction)
  rownames(prediction2) <- rownames(prediction)
  
  for (u in rownames(prediction)){
    if (onlyNew == TRUE){
      unseen <- names(data[u, is.na(data[u,])])
      
      prediction2[u, ] <- as.numeric(t(ifelse(colnames(prediction) %in% unseen, prediction[u, ], as.numeric(NA))))
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
  }
  
  # TopN
  TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), 3))))
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
} 

### Collaborative Filtering (CF) ###

##### User based CF function with correlation
N_UF_BASED <- function(train_da, test_da, N, NN, onlyNew=TRUE){
  
  ### similarity ###
  
  similarity_matrix <- as.matrix(simil(train_da, method="pearson"))
  
  print("similarity calculation done")
  ### Nearest Neighbors ###
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
  }
  
  print("Nearest Neighbor selection done")
  ### Prediction ###
  # Prepare
  prediction <- matrix(, nrow=nrow(test_da), ncol(test_da), 
                       dimnames=list(rownames(test_da), colnames(test_da)))
  prediction2 <- matrix(, nrow=nrow(test_da), ncol(test_da), 
                        dimnames=list(rownames(test_da), colnames(test_da)))
  
  TopN <- matrix(, nrow=nrow(test_da), ncol=N, dimnames=list(rownames(test_da)))
  ### Numerator ###
  for (u in rownames(test_da)){
    similarity_vector <- na.omit(similarity_matrix_NN[u, ])
    
    NN_norm <- train_da[rownames(train_da) %in% names(similarity_vector),]
    
    CM <- colMeans(train_da, na.rm=TRUE)
    for (l in 1:ncol(NN_norm)){
      NN_norm[,l] <- NN_norm[,l] - CM[l]
    }
    NN_norm[is.na(NN_norm)] <- 0
    
    # Numerator
    Num = similarity_vector %*% NN_norm
    
    #Prediction
    prediction[u, ] =  mean(test_da[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
    
    
    if (onlyNew == TRUE){
      unseen <- names(test_da[u, is.na(test_da[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}


##################################################################################################################################

###CONTENT BASED RECOMMENDATION SYSTEM_TOKENIZED

#Loading data for content based RS 
users<-read.table("C:/Users/ngzraryan/Desktop/Recommendation Tools/GP/user_taggedartists.dat", header=TRUE,sep="\t")
tags<-read.table("C:/Users/ngzraryan/Desktop/Recommendation Tools/GP/tags.dat", header = TRUE, sep="\t")

#Checking data structure
length(users$userID)
length(unique(users$userID))

#Merging data sets to have a complete one
userstags<-merge(users,tags, by="tagID", all.users=TRUE)
head(userstags)

#Removing all the punctuations and numbers 
userstags <- userstags %>%
  mutate(tagValue = gsub(x = tagValue, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "_"))


#Tokenizing the tags in words
userstags_token <- userstags %>% unnest_tokens(output = "word", # how should the new column be named?
                                               input = tagValue, # where can we find the text? 
                                               token = "words", # which tokenization scheme should we follow?
                                               drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase


#Spelling correction
correct_spelling <- function(input) {
  output <- case_when(
    # any manual corrections
    input == 'license' ~ 'licence',
    # check and (if required) correct spelling
    !hunspell_check(input, dictionary('en_GB')) ~
      hunspell_suggest(input, dictionary('en_GB')) %>%
      # get first suggestion, or NA if suggestions list is empty
      map(1, .default = NA) %>%
      unlist(),
    TRUE ~ input # if word is correct
  )
  # if input incorrectly spelled but no suggestions, return input word
  ifelse(is.na(output), input, output)
}
userstags_token <- userstags_token %>%  mutate(suggestion = correct_spelling(word))


#Making the long data wide in order to have the tag values as columns
userstags_Product_tok <- dcast(userstags_token,  artistID  ~ tagValue)
userstags_test_tok <- dcast(userstags_token,  userID  ~ artistID)

#Correcting the indexes of the datas
rownames(userstags_test_tok)<-userstags_test_tok$userID
userstags_test_tok$userID<-NULL

rownames(userstags_Product_tok)<-userstags_Product_tok$artistID
userstags_Product_tok$artistID<-NULL

#Setting the needed matrixes for the recommendation system
product_data = userstags_Product_tok
test_data = as(userstags_test_tok, "matrix")

#Predicting
CB<-ContentBased(product_data,test_data,3,11,onlyNew=TRUE)

#Dividing teh data into train and test
train<-userstags_test_tok[1:1000,]
test<-userstags_test_tok[1001:2597,]

#Evaluation metric RSME
RMSE(CB$prediction, test)

#Evaluation metric MAE
MAE(CB$prediction, test)

##################################################################################################################################

###CONTENT BASED RECOMMENDATION SYSTEM_NON-TOKENIZED


#Transferiing long data to wide one
userstags_Product_NT <- dcast(userstags,  artistID  ~ tagValue)
userstags_test <- dcast(userstags,  userID  ~ artistID)


#Correcting the index
rownames(userstags_Product_NT)<-userstags_Product_NT$artistID
userstags_Product_NT$artistID<-NULL

rownames(userstags_test)<-userstags_test$userID
userstags_test$userID<-NULL

#Assigning the matrixes for further calculations
product_data = userstags_Product_NT
test_data = as(userstags_test, "matrix")

#Predicting
CB_NT<-ContentBased(product_data,test_data,3,11,onlyNew=TRUE)

#Splitting the data into train and test for evaluation

train<-userstags_test[1:1000,]
test<-userstags_test[1001:2598,]

#Evaluation
RSME(CB_NT$prediction, test)


##################################################################################################################################

## ITEM BASED CF

#Function
Item_CF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  # Similarity
  
  similarity_matrix = matrix(, ncol=ncol(train_data), nrow=ncol(train_data), dimnames = list(colnames(train_data), colnames(train_data)))
  rowmeans = rowMeans(train_data)
  
  for (i in colnames(train_data)){
    for (j in colnames(train_data)){
      if (j > i){
        sim <- sum((train_data[,i]-mean(train_data[,i], na.rm = TRUE))*(train_data[,j]-mean(train_data[,j], na.rm=TRUE)), na.rm=TRUE)/sqrt(sum((train_data[,i] - mean(train_data[,i], na.rm=TRUE))^2,na.rm = TRUE)*sum((train_data[,i] - mean(train_data[,j], na.rm=TRUE))^2,na.rm = TRUE))
        similarity_matrix[i, j] <- sim
        similarity_matrix[j, i] <- sim
      }
    }
  }
  
  print("Similarity calculation done")
  # Nearest Neighbor
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:ncol(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
    similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
  }
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  
  train_data[is.na(train_data)] <- 0
  
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}

#Set working directory 
setwd("C:/Users/bvarghese/Desktop/IESEG Slides/Recommendation Tools - Stijn Geuens/Data-20200224")
source("functions.R")
source("Cluster_based.R")

#reading the data
artists <- read.csv('artists.dat', sep="\t")
user_tagged_artists <- read.csv('user_taggedartists.dat', sep="\t")
tags <- read.csv('tags.dat', sep="\t")
user_artists <- read.csv('user_artists.dat', sep="\t")

# get unique userID and aritstID to subset user_artist
unique_userID <- unique(user_artists$userID)
unique_artistID <- unique(user_artists$artistID)

# subset user_artist 
user_artists <- subset(user_artists, userID %in% unique_userID)
user_artists <- subset(user_artists, artistID %in% unique_artistID)


# inspecting the weight variable of the user_artist dataset
max(user_artists$weight)
min(user_artists$weight)


# create 10 equal sized groups for the weight variable (categorization)
user_artists$group <- as.double(cut_number(user_artists$weight,10))
user_artists$weight <- NULL
user_artists


# creating the final data for the colaborative filtering
final_cf <- spread(user_artists, artistID, group)
rownames(final_cf) <- final_cf$userID
final_cf$userID <- NULL

min(final_cf)
max(final_cf)
dim(final_cf)

# create matrices
final_cf_matrix <- as(final_cf,"matrix")


### Calling the Cluster Based function 

Results_Cluster <- ClusterBasedCF(final_cf_matrix, 3, 10, 20, onlyNew = TRUE)
prediction <- Results_Cluster$prediction
TopN <- Results_Cluster$topN


### Split train - Test 

train <- final_cf_matrix[1:100, 1:500]
test <- final_cf_matrix[101:150, 1:500]


### Item-Based CF

item_results <- Item_CF(train, test, 3, 10, onlyNew = TRUE)

item_prediction <- as.data.frame(item_results$prediction)
item_TopN <- as.data.frame(item_results$topN)

###Evaluation using MAE
MAE(CB$prediction, test)

################################################################################################################

###USER BASED CF

final <- as(final_cf_matrix, "realRatingMatrix")

## parameter ###
trainPercentage = 0.7

trainRows <- sample(1:nrow(final), nrow(final)*trainPercentage)

train_da <- final[trainRows,]
test_da <- final[-trainRows,]

train_da <- as(train_data,"matrix")
test_da <- as(test_data,"matrix")

#For all users 
ResultsUBCF <- N_UF_BASED(train_da, test_da, N=3, NN=10, onlyNew=TRUE)

ResultsUBCF$prediction
ResultsUBCF$topN



### Reference Links
# https://rpubs.com/jt_rpubs/285729


################################################################################################################
###HYBRID 1

users<-read.table("C:/Users/dwijayaweera/Documents/2nd - Recommendation Tools/GroupAssignmentData/user_taggedartists.dat", header=TRUE)
tags<-read.delim("C:/Users/dwijayaweera/Documents/2nd - Recommendation Tools/GroupAssignmentData/tags.dat", header = TRUE, sep="\t")
user_artists<-read.delim("C:/Users/dwijayaweera/Documents/2nd - Recommendation Tools/GroupAssignmentData/user_artists.dat", header = TRUE, sep="\t")
artists<-read.delim("C:/Users/dwijayaweera/Documents/2nd - Recommendation Tools/GroupAssignmentData/artists.dat", header = TRUE, sep="\t")



usertags<-merge(users,tags, by="tagID")
user_tags_artists<-merge(usertags,user_artists)
user_tags_artists<-merge(user_tags_artists, artists)
user_tags_artists<-as.matrix(user_tags_artists)

#user_tags_artists <- user_tags_artists[with(user_tags_artists, order(-weight)), ]


### Split train - Test ###
set.seed(2)

train_rows = sample(1:nrow(user_tags_artists), 0.7*nrow(user_tags_artists))

train <- as(user_tags_artists, "matrix")[train_rows,]
test <- as(user_tags_artists, "matrix")[-train_rows,]
test1 <- test[1:750, ]
test2 <- test[751:1500, ]



### Compute individual models ###
ContentBasedModel <- ContentBased(product_data, test1, 3, 10, onlyNew=F)
CFItemBasedModel <- ItemBasedCF(user_artists, test1, 3, 10, onlyNew=F)


### Transform results to lists (to be able to use the rowMeans function) ###
ContentBasedModel_list <- as.list(ContentBasedModel$prediction)
CFItemBasedModel_list <- as.list(CFItemBasedModel$prediction)



###Compute Means
hybrid <- rowMeans(cbind(as.numeric(ContentBasedModel_list), as.numeric(CFItemBasedModel_list)), na.rm=T)



### Transform list back to matrix with correct number of dimensions ###
Hybrid_prediction <- matrix(hybrid, nrow=nrow(test1), ncol=ncol(test1))
rownames(Hybrid_prediction) <- rownames(test1)
colnames(Hybrid_prediction) <- colnames(test1)

# RMSE
RMSE(ContentBasedModel$prediction, test1)
RMSE(CFItemBasedModel$prediction, test1)
RMSE(Hybrid_prediction, test1)

# Classification
Classification(ContentBasedModel$prediction, test1, threshold=3)
Classification(CFItemBasedModel$prediction, test1, threshold=3)
Classification(Hybrid_prediction, test1, threshold=3)


################################################################################



###Hybrid2

set.seed(2)
train_rows = sample(1:nrow(final), 0.7*nrow(final))

train_D <- as(final, "matrix")[train_rows,]
test_D <- as(final, "matrix")[-train_rows,]
test1 <- test_D[1:300,]
test2 <- test_D[300:700,]


### Compute individual models ###
CLTFIDF <- ClusterBasedCF(final_cf_matrix, test1, 3, 10, onlyNew=F)
UB <- N_UF_BASED(train_D, test1, 3, 10, onlyNew=F)

### Transform results to lists (to be able to use the rowMeans function) ###
CLTFIDF_list <- as.list(CLTFIDF$prediction)
UB_list <- as.list(UB$prediction)

####################
### Compute Mean ###
####################
hybrid <- rowMeans(cbind(as.numeric(CLTFIDF_list), as.numeric(UB_list)), na.rm=T)

### Transform list back to matrix with correct number of dimensions ###
Hybrid_prediction <- matrix(hybrid, nrow=nrow(test1), ncol=ncol(test1))
rownames(Hybrid_prediction) <- rownames(test1)
colnames(Hybrid_prediction) <- colnames(test1)

### Evaluate ###
# RMSE
RMSE(CLTFIDF$prediction, test1)
RMSE(UB$prediction, test1)
RMSE(Hybrid_prediction, test1)

# Classification
Classification(CLTFIDF$prediction, test1, threshold=5)
Classification(UB$prediction, test1, threshold=5)
Classification(Hybrid_prediction, test1, threshold=5)

#########################
### Linear Regression ###
#########################

# Train a linear Regression
### flatten test1 dataset
test_list <- as.list(test1)

### Transform list and matrices to dataframe
test_df <- data.frame(matrix(unlist(test_list), byrow=T))
CLTFIDF_df <- data.frame(matrix(unlist(CLTFIDF_list), byrow=T))
UB_df <- data.frame(matrix(unlist(UB_list), byrow=T))

### Combine created dataframes
input <- cbind(test_df, CLTFIDF_df, UB_df)
colnames(input) <- c('TARGET', 'CLTFIDF', 'UB')

### Train the linear regression
fit <- lm(TARGET ~ CLTFIDF + UB, data=input)
summary(fit)

### Score Models
CLTFIDF2 <- ClusterBasedCF(final_cf_matrix, test2, 3, 10, onlyNew=F)
UB2 <- N_UF_BASED(train_D, test2, 3, 10, onlyNew=F)

### Matrix to list
test_list2 <- as.list(test2)
CLTFIDF2_list2 <- as.list(CLTFIDF2$prediction)
UB2_list2 <- as.list(UB2$prediction)

### List to dataframe
test_df2 <- data.frame(matrix(unlist(test_list2), byrow=T))
CLTFIDF2_df2 <- data.frame(matrix(unlist(CLTFIDF2_list2), byrow=T))
UB2_df2 <- data.frame(matrix(unlist(UB2_list2), byrow=T))

### combine dataframes to have an input dataset for linear regression
input2 <- cbind(test_df2, CLTFIDF2_df2, UB2_df2)
colnames(input2) <- c('TARGET', 'CLTFIDF2', 'UB2')

### Predict using the model calculated on test2 dataset
Hybrid_lin_reg <- predict(fit, input2)

### Transform the list of results to matrix with the correct dimensions
Hybrid_lin_reg <- matrix(Hybrid_lin_reg, nrow=nrow(test2), ncol=ncol(test2))
rownames(Hybrid_lin_reg) <- rownames(test2)
colnames(Hybrid_lin_reg) <- colnames(test2)

# Evaluation
# RMSE
RMSE(CLTFIDF2$prediction, test2)
RMSE(UB2$prediction, test2)
RMSE(Hybrid_lin_reg, test2)

# Classification
Classification(CLTFIDF2$prediction, test2, threshold=5)
Classification(UB2$prediction, test2, threshold=5)
Classification(Hybrid_lin_reg, test2, threshold=5)

################################################################################

write.csv(user_artists.dat,"C:/Users/kgupta/Desktop/Recommendation tools/user_artists.csv", row.names = FALSE)

#