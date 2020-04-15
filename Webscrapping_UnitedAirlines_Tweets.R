#Installing Required Packages
 
  #1. Install.packages
      install.packages("readxl")
      library(readxl)
 
 #2. Round_df function
     install.packages("forestmangr")
     library(forestmangr)
 
  #1. For plotting of graphs
      install.packages("ggplot2")
      library(ggplot2)

      install.packages("plotly")
      library(plotly)

  #2. For plotting time series of tweets
      install.packages("TSstudio")
      library(TSstudio)
      
  #3. Company Sentiment Analysis (converting tweets to ASCII)
      install.packages("syuzhet")
      library(syuzhet)
      
  #4. For topic modelling (finding out optimum k) 
      install.packages("ldatuning")
      library("ldatuning")
 
  #5. For topic modelling (coherence method for k)
      install.packages("textmineR")
      library(textmineR)
      
  #6. For Network Analysis
      install.packages("devtools")
      library(devtools)
      
      #install_github("dgrtwo/widyr")
      install.packages("widyr")
      library(widyr)
      
      install.packages("tidyr")
      library(tidyr)
      
      install.packages("igraph")
      library(igraph)
      
      install.packages("ggraph")
      library(ggraph)
      
  #7. Packages required for Shiny Dashboard
      install.packages("shinythemes")
      library(shinythemes)
      
      install.packages("RColorBrewer")
      library(RColorBrewer)
      
      install.packages("DT")
      library(DT)
      
      install.packages("shinydashboard")
      library(shinydashboard)
      
      library(shiny)
  
  #8. Libraries required for coherence and cross validation
      library(doParallel)
      library(scales)
      library(foreach)
  
  #7. Other required libraries
      library(dplyr)
      library(rtweet)
      library(tidytext)
      library(tidyverse)
      library(textdata)
      library(textstem)
      library(lubridate)
      library(topicmodels)
      library(stringr)
      library(caTools)
  
  #8. Rtweet Package
      if(!require("rtweet")) install.packages("rtweet"); library("rtweet")  
      
  #9. Topicmodels package 
      if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")
      
  #10. For word cloud
      if (!require("wordcloud")) {
        install.packages("wordcloud",repos="https://cran.rstudio.com/",
                         quiet=TRUE)
        require("wordcloud")
      }
      
  #11. Other Basic Packages
      for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
           if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
           require(i, character.only=TRUE)
      }

#Loading the downloaded data
 
  #1. For tweets where united airlines was mentioned 
      ## We used the search_fullarchive method to download all the user tweets for all 12 months in 2019 
      ## Total tweets collected - 15065
      load("C:/Users/bvarghese/Desktop/IESEG Slides/Social_Media_Matthijs/Group Project/Fulldata_2019_updated.Rdata")
 
  #2. For tweets posted by United Airlines
      ## We used the get_timeline method to obtain tweets for the company in 
      ## However most of these tweets are relpies and not organic tweets
      load("C:/Users/bvarghese/Desktop/IESEG Slides/Social_Media_Matthijs/Group Project/unitedair_tweets3.Rdata")
      
      unitedair_tweets <- unitedair_tweets3
     
##Analysis of company tweets
     
  #1. Removing retweets and replies
     
      # Remove retweets
      unitedair_tweets_organic <- unitedair_tweets[unitedair_tweets$is_retweet==FALSE, ] 
     
      # Remove replies
      unitedair_tweets_organic <- subset(unitedair_tweets_organic, 
                                        is.na(unitedair_tweets_organic$reply_to_status_id)) 
     
  #2. Analysing engagement by looking at favorite_count and retweet_count 
      unitedair_tweets_organic <- unitedair_tweets_organic %>% arrange(-favorite_count)
      unitedair_tweets_organic[1,5]
      unitedair_tweets_organic <- unitedair_tweets_organic %>% arrange(-retweet_count)
      unitedair_tweets_organic[1,5]
     
  #3. Showing the ratio of replies/retweets/organic tweets
     
      # Keeping only the retweets
      unitedair_retweets <- unitedair_tweets[unitedair_tweets$is_retweet==TRUE,]
     
      # Keeping only the replies
      unitedair_replies <- subset(unitedair_tweets, !is.na(unitedair_tweets$reply_to_status_id))
     
      # Creating a data frame containing the number of organic tweets, retweets and replies
      data <- data.frame(
        category=c("Organic", "Retweets", "Replies"),
        count=c(6, 0, 3194)
      )
     
  #4. Preparing data frame for a donut chart
      
      # Adding columns 
      data$fraction = data$count / sum(data$count)
      data$percentage = data$count / sum(data$count) * 100
      data$ymax = cumsum(data$fraction)
      data$ymin = c(0, head(data$ymax, n=-1))
      
      # Rounding the data to two decimal points
      data <- round_df(data, 2)
      
      # Specify what the legend should say
      Type_of_Tweet <- paste(data$category, data$percentage, "%")
      
      #save(data, file = "data.RData")
      
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
        geom_rect() +
        coord_polar(theta="y") + 
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "right")
      
  #5. Showing when the tweets are published
      
      day(as.POSIXlt(unitedair_tweets$created_at, format="%d/%m/%Y"))
      
      colnames(unitedair_tweets)[colnames(unitedair_tweets)=="screen_name"] <- "Twitter_Account"
      unitedair_tweets %>%
      ts_plot(dplyr::group_by(unitedair_tweets, Twitter_Account), "day") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = "Frequency of Tweets from United Airlines",
          subtitle = "Tweet counts aggregated by day",
          caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )
      
   #6. Showing from where the tweets are published
      
       unitedair_app <- unitedair_tweets %>% 
         select(source) %>% 
         group_by(source) %>%
         summarize(count=n())
      
      
       unitedair_app <- subset(unitedair_app, count > 11)
      
      # Preparing data frame for a donut chart
      
      data_source <- data.frame(
        category=unitedair_app$source,
        count=unitedair_app$count
      )
      data_source$fraction = data_source$count / sum(data_source$count)
      data_source$percentage = data_source$count / sum(data_source$count) * 100
      data_source$ymax = cumsum(data_source$fraction)
      data_source$ymin = c(0, head(data_source$ymax, n=-1))
      data_source <- round_df(data_source, 2)
      Source <- paste(data_source$category, data_source$percentage, "%")
      ggplot(data_source, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
        geom_rect() +
        coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "right")
      
   #7. Showing the most frequent words found in the tweets
      
      # Removing special characters
      unitedair_tweets_organic$text <-  gsub("https\\S*", "", unitedair_tweets_organic$text)
      unitedair_tweets_organic$text <-  gsub("@\\S*", "", unitedair_tweets_organic$text) 
      unitedair_tweets_organic$text  <-  gsub("amp", "", unitedair_tweets_organic$text) 
      unitedair_tweets_organic$text  <-  gsub("[\r\n]", "", unitedair_tweets_organic$text)
      unitedair_tweets_organic$text  <-  gsub("[[:punct:]]", "", unitedair_tweets_organic$text)
      unitedair_tweets_organic$text  <-  gsub("[0-9]", "", unitedair_tweets_organic$text)
      unitedair_tweets_organic$text  <-  gsub("We're", "", unitedair_tweets_organic$text)
      unitedair_tweets_organic$text  <-  gsub("we're", "", unitedair_tweets_organic$text)
      unitedair_tweets_organic$text  <-  gsub("we've", "", unitedair_tweets_organic$text)
      
      # Tokenization
      unitedair_tweets_tok <- unitedair_tweets_organic %>% unnest_tokens(output = "word", # how should the new column be named?
                                                                         input = text, # where can we find the text? 
                                                                         token = "words", # which tokenization scheme should we follow?
                                                                         drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase
      
      
      # Removing stopwords
      tweets <- unitedair_tweets_tok %>%
        select(text) %>%
        unnest_tokens(word, text)
      
      
      tweets <- tweets %>% anti_join(get_stopwords())
      
      
      # Plotting the most frequent words found in the tweets
      tweets_frequent <- tweets %>% # gives you a bar chart of the most frequent words found in the tweets
        count(word, sort = TRUE) %>%
        top_n(15) %>%
        mutate(word = reorder(word, n))
      
        ggplot(tweets_frequent, aes(x = word, y = n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip() +
        labs(y = "Count",
             x = "Unique words",
             title = "Most frequent words found in the tweets of United",
             subtitle = "Stop words removed from the list")
      
  #8. Showing the most frequently used hashtags
      
      ##Getting hashtags
      unitedair_tweets_organic$hashtags <- as.character(unitedair_tweets_organic$hashtags)
      unitedair_tweets_organic$hashtags <- gsub("c\\(", "", unitedair_tweets_organic$hashtags)
  
      set.seed(1234)
      wordcloud(unitedair_tweets_organic$hashtags, min.freq=200, scale=c(0.5, 0.5), random.order=FALSE, 
                colors=brewer.pal(8, "Dark2"))
      
  #9. Sentiment analysis
      
      # Converting tweets to ASCII to trackle strange characters
      tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
      
      # Removing retweets, in case needed 
      tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
      
      # Removing mentions, in case needed
      tweets <-gsub("@\\w+","",tweets)
      ew_sentiment<-get_nrc_sentiment((tweets))
      sentimentscores<-data.frame(colSums(ew_sentiment[,]))
      names(sentimentscores) <- "Score"
      sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
      rownames(sentimentscores) <- NULL
      ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
        geom_bar(aes(fill=sentiment),stat = "identity")+
        theme(legend.position="none")+
        xlab("Sentiments")+ylab("Scores")+
        ggtitle("Total sentiment based on scores")+
        theme_minimal()

  
###Analysis for user tweets that mention United Airlines
      
 ##NETWORK ANALYSIS
      
  #1. Remove punctuation and numbers with regular expressions
      united_clean1 <- Fulldata_2019 %>%
        filter(lang == "en") %>%
        mutate(text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
      
      
  #2. Remove http elements manually
      united_clean1$stripped_text <- gsub("http.*","",  united_clean1$text)
      united_clean1$stripped_text <- gsub("https.*","", united_clean1$stripped_text)
      
  #3. Remove punctuation, convert to lowercase, add id for each tweet!
      united_clean1_paired_words <- united_clean1 %>%
      dplyr::select(stripped_text) %>%
      unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)
      
      united_clean1_paired_words %>%
      count(paired_words, sort = TRUE)
      
      united_clean1_separated_words <- united_clean1_paired_words %>%
      separate(paired_words, c("word1", "word2"), sep = " ")
      
      united_clean1_filtered <- united_clean1_separated_words %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
      
  #4. new bigram counts:
      unitedwords_counts <- united_clean1_filtered %>%
      count(word1, word2, sort = TRUE)
      
  #5. Plotting the data
      
      #(plotting graph edges is currently broken)
      network <- unitedwords_counts %>%
                 filter(n >= 85) 
      
      network %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      # geom_edge_link(aes(edge_alpha = n, edge_width = n))
      # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
      geom_node_point(color = "darkslategray4", size = 3) +
      geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
      labs(title = "Word Network: Tweets using the hashtag - United Airlines",
           subtitle = "Twitter data ",
           x = "", y = "")
      
 ##SENTIMENT ANALYSIS
      
  #1. Converting to data frame
      as.data.frame(Fulldata_2019)
      UAdata<- subset(Fulldata_2019, select = c(1,3,5))
      #For now removing the punctuations (may be added in further analysis)
      UAComments <- Fulldata_2019 %>%
        filter(lang == "en")
        mutate(UAdata, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
      
      #Tokenization (+ going to lowercase)
      UATokenized <- UAComments %>% unnest_tokens(output = "word", 
                                                  input = text, 
                                                  token = "words", 
                                                  drop=FALSE,to_lower=TRUE) 
      
      #spelling corrections
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
      UATokenized <- UATokenized %>%  mutate(suggestion = correct_spelling(word))
      
      stem_word=lemmatize_words(UATokenized$word, dictionary = lexicon::hash_lemmas)
      UATokenized<-cbind(stem_word,UATokenized)
      
      # Assigning the needed dictionary
      dict<-get_sentiments('afinn')
      
      # Creating sentiments using afinn dictionary, afinn was chosen to make further calculations easier.
      #UASentiment <- inner_join(UATokenized,dict)
      UAforSentiment <- UATokenized %>% group_by(user_id) %>% 
        mutate(prevword = lag(x=stem_word,n=1)) %>% 
        ungroup()
      
      # check whether the lagged word is a negation word
      # if it is, change the sign
      negationWords <- get_stopwords()[c(81:98,165:167),"word"]
      
      UAforSentiment <- UAforSentiment %>% inner_join(get_sentiments("afinn")) %>%
        mutate(correct_sentiment = ifelse(prevword %in% negationWords$word,-value,value)) %>%
        group_by(user_id) %>%                      
        summarize(Sentiment = sum(correct_sentiment)) 
      
      fullSentiment <- merge(UAforSentiment,UAComments,by="user_id")
      fullSentiment$created_at<-as.Date(fullSentiment$created_at)
      library(lubridate)
      
      fullSentiment$MONTH<-month(as.POSIXlt(fullSentiment$created_at, format="%d/%m/%Y"))
      by_month<-fullSentiment %>% group_by(MONTH) %>% 
        count(MONTH)
      
      by_date <- fullSentiment %>% group_by(created_at) %>% 
        summarise(Sentiment = sum(Sentiment))
      
      #write.csv(by_date, "sentiment.csv")
      
  #2. Plotting tweet's sentiments per day
      ggplot(by_date,aes(x=as.Date(created_at),y=Sentiment, color = Sentiment)) +
        geom_line(size = 1.5) +
        geom_smooth(method = "lm", se = FALSE, lty = 2) +
        expand_limits(y = 0)
      
      Fulldata_2019$created_at<-as.Date(Fulldata_2019$created_at)
      
      Fulldata_2019 <- Fulldata_2019 %>%
        mutate(display_text_width = if_else(is.na(display_text_width), 0, display_text_width))
      
      length <- Fulldata_2019 %>% group_by(created_at) %>% 
        summarise(display_text_width = sum(display_text_width))
      
      genanalysis <- merge(by_date,length,by="created_at")
      
      ggplot(genanalysis, aes(created_at)) + # basic graphical object
        geom_line(aes(y=Sentiment), colour="red") +  # first layer
        geom_line(aes(y=display_text_width), colour="green")
      
      # Matching sentiment words from the 'NRC' sentiment lexicon
      senti = inner_join(UATokenized, get_sentiments("nrc")) %>%
        count(sentiment)
      
      senti$percent = (senti$n/sum(senti$n))*100
      
  #3. Plotting the sentiment summary 
      ggplot(senti, aes(sentiment, percent)) +   
        geom_bar(aes(fill = sentiment), position = 'dodge', stat = 'identity')+ 
        ggtitle("Sentiment analysis based on lexicon: 'NRC'")+
        coord_flip() +
        theme(legend.position = 'none', plot.title = element_text(size=18, face = 'bold'),
              axis.text=element_text(size=16),
              axis.title=element_text(size=14,face="bold"))
      
  #4. Word Cloud
      # Removing stopwords for wordcloud
      UAwordcloud <- UATokenized %>% anti_join(get_stopwords()) 
      
      UAFreq <- UAwordcloud %>% group_by(stem_word) %>% 
        summarize(freq = n()) %>%
        arrange(-freq)                 
      #Deleting United, unitedairlines,airlines
      UAFreq <- UAFreq[-c(1, 2, 4), ]
      #Deleting flight and airline
      UAFreq <- UAFreq[-c(1, 10), ]
      
      wordcloud(UAFreq$stem_word, UAFreq$freq,
                max.words=40,
                scale=c(2,1))
      
  #5. Words used more than 800 times
      
      plot = subset(UAFreq, UAFreq$freq > 800) #creating a subset of words having more than 400 frequency
      str(plot)
      ggplot(data = plot, aes(stem_word, freq)) + geom_bar(stat = 'identity') + ggtitle('Words used more than 800 times')+coord_flip()
      
      #We were interested to see whether the stock prices can be predicted 
      #by the sentiments of tweets. We were planning to use ARIMA modeling
      #but as there are not enough observations as well as the preliminary tests
      # shown that there is no correlation or any relationship between the sentiment
      #and the adjusted close price.
      
      Data <- read_excel("C:/Users/bvarghese/Desktop/IESEG Slides/Social_Media_Matthijs/Group Project/Data.xlsx")
      Datanew <- cbind(Data,by_date)
      
      plot(Data$`Adj Close`, Data$Sentiment, main="Scatterplot",
           xlab="Stock price ", ylab="Sentiments", pch=19)
      
      Data$`Adj Close`
      ggplot(Data, aes(created_at)) +                    # basic graphical object
        geom_line(aes(y=`Adj Close`), colour="red") +  # first layer
        geom_line(aes(y=Sentiment), colour="green")
      
      cor(Data$`Adj Close`, Data$Sentiment, method = c("pearson"))
      cor.test(Data$`Adj Close`, Data$Sentiment, method=c("kendall"))
      cor(Data$`Adj Close`, Data$Sentiment, method = c("spearman"))
      
 ##GENERAL ANALYSIS ON THE USERS DATA
      
  #1. Map Plotting
      install.packages()
      library(rtweet)
      rt <- lat_lng(Fulldata_2019)
      par(mar = c(0, 0, 0, 0))
      if(!require("maps")) install.packages("maps"); library("maps")
      maps::map("world", lwd = .25)
      points(rt$lng, rt$lat, pch = 20, cex = 1,col="red")
      
  #2. Plotting language distribution
      UALang <- Fulldata_2019 %>% group_by(lang) %>% 
        summarize(freq = n()) %>%
        arrange(-freq) 
      
      plot = subset(UALang, UALang$freq > 100) 
      str(plot)
      ggplot(data = plot, aes(lang, freq)) + geom_bar(stat = 'identity') + ggtitle('Languages used more than 100 times')+coord_flip()
      
  #3. Distribution of retweets and original tweets
      counts <- table(Fulldata_2019$is_retweet)
      barplot(counts, main="Tweet Type Distribution", horiz=FALSE, ylim = c(0, 800),
              names.arg=c("Original tweet", "Retweet"),col = rainbow(2))
      
  #4. Followers analysis
      Fol<- subset(Fulldata_2019, select = c(1,78))
      
      Folgrouped<-Fol %>% group_by(user_id) %>% summarise(First_value= first(followers_count)) 
      
      Followers<-merge(UAforSentiment,Folgrouped, by="user_id")
      
      Influencers<-subset(Followers, First_value > 2000)
      Influencers$Dummy_sent<- ifelse(Influencers$Sentiment>0, 1, 0)
      
      counts <- table(Influencers$Dummy_sent)
      barplot(counts, main="Influencers Analysis", horiz=FALSE, ylim = c(0, 800),
              names.arg=c("Negative tweet", "Positive tweet"),col = rainbow(2))
      
  #5. Checking the followers
      Influencers_positive<-subset(Influencers, Dummy_sent = 1)
      
      followersUA <- get_friends("united")
      influencers_friends<-merge(followersUA,Influencers_positive, by="user_id")
      
      slices <- c(4, 716)
      lbls <- c("Friends", "Overall positive")
      pie(slices, labels = lbls, main="Influencers that are friends of UA")
      
      
###Topic Modelling
  #1. Pre-Processing the data
     
      # Filter for English tweets and Remove punctuation and numbers with regular expressions
      united_clean <- Fulldata_2019 %>%
                      filter(lang == "en") %>%
                      mutate(text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
      
      # Tokenization (+ going to lowercase)
      united_clean_tokenized <- united_clean %>% unnest_tokens(output = "word", # how should the new column be named?
                                                               input = text, # where can we find the text? 
                                                               token = "words", collapse = FALSE, # which tokenization scheme should we follow?
                                                               drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase
      
      # Remove some other elements such as # and @ signs if they might occur
      united_clean_tokenized <- filter(united_clean_tokenized, substr(word, 1, 1) != '#', substr(word, 1, 1) != '&', 
                                       substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags
      
      united_try <- united_clean_tokenized %>%
                    filter((word!= "unitedairlines") & (word!= "united") & (word!= "airlines"))
             
      # Lemmatization
      united_try$word <- lemmatize_words(united_try$word, dictionary = lexicon::hash_lemmas)
      #united_try<-cbind(united_try$word,united_try)
      
  #2. LDA Tuning (To find out the optimum value of k)

      # Removing stop words and getting creating the document-term matrix
      united_clean_final <- united_try %>%
        anti_join(stop_words) %>%
        count(status_id,word, sort=TRUE) %>%
        cast_dtm(document = status_id, term = word,
                 value = n, weighting = tm::weightTf)
      
      # Getting scores for the different methods 
      result <- FindTopicsNumber(
        united_clean_final,
        topics = seq(from = 2, to = 30, by = 1),
        metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
        method = "Gibbs",
        control = list(seed = 77),
        mc.cores = 2L,
        verbose = TRUE
      )
      
      # Plotting the results
      FindTopicsNumber_plot(result)
      
  #3. Coherence Method (to find out the optimum value of k)
      
      united_clean_dtm <- united_try %>%
        anti_join(stop_words) %>%       
        count(status_id,word , sort=TRUE) 
      
      #Creating training and Testing Sets
      n <- nrow(Fulldata_2019)
      split <- sample(1:n, round(n * 0.75))
      train_set <- Fulldata_2019[split, ]
      test_set <- Fulldata_2019[-split, ]
      
      # Cleaning the training set
      train_clean <- train_set %>%
        filter(lang == "en") %>%
        mutate(text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
      
      train_tokenized <- train_clean %>% unnest_tokens(output = "word",
                                                       input = text,
                                                       token = "words",
                                                       drop=FALSE,to_lower=TRUE)
      
      train_clean_tokenized <- filter(train_tokenized, substr(word, 1, 1) != '#', substr(word, 1, 1) != '&', 
                                      substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags
      
      train_clean_tokenized <- train_clean_tokenized %>%
        filter(!(nchar(word) == 1) & (word!= "unitedairlines") & (word!= "united") & (word!= "airlines"))
      
      stem_word=lemmatize_words(train_clean_tokenized$word, dictionary = lexicon::hash_lemmas)
      train_clean_tokenized <-cbind(stem_word,train_clean_tokenized)
      
      train_coherence <- train_clean_tokenized %>%
        anti_join(stop_words) %>%   
        count(status_id,stem_word , sort=TRUE)
      
      # Creating a dtm for Coherence method
      dtm <- CreateDtm(train_coherence$stem_word, 
                       doc_names = train_coherence$status_id,
                       ngram_window = c(1, 1))
      
      # Explore the basic frequency
      tf <- TermDocFreq(dtm = dtm)
      original_tf <- tf %>% select(term, term_freq,doc_freq) 
      rownames(original_tf) <- 1:nrow(original_tf)
      
      # Eliminate words appearing less than 20 times or in more than half of the documents
      vocabulary <- tf$term[ tf$term_freq > 20 & tf$doc_freq < nrow(dtm) / 2 ]
      
      k_list <- seq(1, 30, by = 1)
      model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
      if (!dir.exists(model_dir)) dir.create(model_dir)
      model_list <- TmParallelApply(X = k_list, FUN = function(k){
        filename = file.path(model_dir, paste0(k, "_topics.rda"))
        
        if (!file.exists(filename)) {
          set.seed(123)
          m <- FitLdaModel(dtm = dtm, k = k, iterations = 500, calc_likelihood = TRUE)
          m$k <- k
          m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 8)
          save(m, file = filename)
        } else {
          load(filename)
        }
        
        m
      }, export=c("dtm", "model_dir")) # export only needed for Windows machines      
      
      # model tuning and choosing the best model
      coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                                  coherence = sapply(model_list, function(x) mean(x$coherence)), 
                                  stringsAsFactors = FALSE)
      
      #Plotting coherence for all values of k
      ggplot(coherence_mat, aes(x = k, y = coherence)) +
        geom_point() +
        geom_line(group = 1)+
        ggtitle("Best Topic by Coherence Score") + theme_minimal() +
        scale_x_continuous(breaks = seq(1,30,1)) + ylab("Coherence")
      
      # Selecting the model with maximum coherence 
      model_coherence <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
      model_coherence$top_terms <- GetTopTerms(phi = model_coherence$phi, M = 10)
      top_10_terms <- as.data.frame(model_coherence$top_terms)
      
      save(top_10_terms, file = "top_10_terms.Rdata")
      
      # Checking model features
      str(model_coherence)
      
      # Plotting the likelihood of the best selected model
      plot(model_coherence$log_likelihood, type = "l")
      
      # Creating a dendogram to identify the most closely related topics
      model_coherence$topic_linguistic_dist <- CalcHellingerDist(model_coherence$phi)
      model_coherence$hclust <- hclust(as.dist(model_coherence$topic_linguistic_dist), "ward.D")
      model_coherence$hclust$labels <- paste(model_coherence$hclust$labels, model_coherence$labels[ , 1])
      plot(model_coherence$hclust)
      
      # Cleaning the test set
      test_clean <- test_set %>%
        filter(lang == "en") %>%
        mutate(text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
      
      test_tokenized <- test_clean %>% unnest_tokens(output = "word",
                                                     input = text,
                                                     token = "words",
                                                     drop=FALSE,to_lower=TRUE)
      
      test_clean_tokenized <- filter(test_tokenized, substr(word, 1, 1) != '#', substr(word, 1, 1) != '&', 
                                     substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags
      
      test_clean_tokenized <- test_clean_tokenized %>%
        filter(!(nchar(word) == 1) & (word!= "unitedairlines") & (word!= "united") & (word!= "airlines"))
      
      stem_word=lemmatize_words(test_clean_tokenized$word, dictionary = lexicon::hash_lemmas)
      test_clean_tokenized<-cbind(stem_word,test_clean_tokenized)
      
      test_coherence <- test_clean_tokenized %>%
        anti_join(stop_words) %>%       
        count(status_id,stem_word , sort=TRUE)
      
      
  #4. 5-fold validation to find optimum value of k
      
      #Creating train and test Document-Term Matrices
      
      train_final <- train_clean_tokenized %>%
        anti_join(stop_words) %>%   
        count(status_id,stem_word , sort=TRUE) %>%
        cast_dtm(document = status_id, term = stem_word,
                 value = n, weighting = tm::weightTf)
      
      test_final <- test_clean_tokenized %>%
        anti_join(stop_words) %>%       
        count(status_id,stem_word , sort=TRUE) %>%
        cast_dtm(document = status_id, term = stem_word,
                 value = n, weighting = tm::weightTf)
      
      # Defining model parameters
      burnin = 1000
      iter = 500
      keep = 50   
      
      # Finding out the optimum k
      cross_model <- lapply(seq(2,30, by=1), function(k){LDA(train_final, k,  method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep))})
      
      # Getting the log likelihoods of the model to determine optimum k
      cross_model_log <- as.data.frame(as.matrix(lapply(cross_model, logLik)))
      
      # Creating a data frame of the topics and the associated log 
      log_df <- data.frame(topics=c(2:30), LL=as.numeric(as.matrix(cross_model_log)))
      
      # Plotting the results
      ggplot(log_df, aes(x=topics, y=LL)) + 
        xlab("Number of topics") + ylab("Log likelihood of the model") + 
        geom_line() + 
        theme_bw()  
      
      log_df[which.max(log_df$LL),]
      
      # Optimal value was at 29. Create a model for k = 29
      train_final_lda <- LDA(train_final, k = 29, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )
   
      # Check its perplexity parameter
      perplexity(train_final_lda, newdata = train_final)
      
      # Compare it to that of the test set to check performance
      perplexity(train_final_lda, newdata = test_final)
      
      
      # Get the terms per topic: which terms determine the topic?
      train_topics <- tidy(train_final_lda, matrix = "beta")
      
      # Top terms per topic
      train_terms <- train_topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
      
      train_terms <- train_terms %>%
        mutate(term = reorder_within(term, beta, topic)) 
      
        ggplot(train_terms, aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()
      
      # Get the topics per document: which topics determine the documents?
      train_documents <- tidy(train_final_lda, matrix = "gamma")
      
      # Choose, per tweet, the most important topic (the one with the highest weight)
      train_doc_topic <- train_documents %>%
        group_by(document) %>%
        arrange(desc(gamma)) %>%
        slice(1) 
      
      train_doc_topic %>%
        group_by(topic) %>% 
        summarise(nbr_documents = n())
      
      library(ggthemes)
      
      td_beta <- tidy(train_final_lda)
      td_gamma <- tidy(train_final_lda, matrix = "gamma")
      
      top_terms <- td_beta %>%
        arrange(beta) %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        arrange(-beta) %>%
        select(topic, term) %>%
        summarise(terms = list(term)) %>%
        mutate(terms = map(terms, paste, collapse = ", ")) %>% 
        unnest()
      
      gamma_terms <- td_gamma %>%
        group_by(topic) %>%
        summarise(gamma = mean(gamma)) %>%
        arrange(desc(gamma)) %>%
        left_join(top_terms, by = "topic") %>%
        mutate(topic = paste0("Topic ", topic),
               topic = reorder(topic, gamma))
      
      gamma_terms %>%
        top_n(20, gamma) %>%
        ggplot(aes(topic, gamma, label = terms, fill = topic)) +
        geom_col(show.legend = FALSE) +
        geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
                  family = "IBMPlexSans") +
        coord_flip() +
        scale_y_continuous(expand = c(0,0),
                           limits = c(0, 0.09),
                           labels = percent_format()) +
        theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
        theme(plot.title = element_text(size = 16,
                                        family="IBMPlexSans-Bold"),
              plot.subtitle = element_text(size = 13)) +
        labs(x = NULL, y = expression(gamma),
             title = "Top 20 topics by prevalence in the Hacker News corpus",
             subtitle = "With the top words that contribute to each topic")