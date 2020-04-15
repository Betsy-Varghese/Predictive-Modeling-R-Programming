install.packages("haven")
library(haven)

install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

install.packages("tidyverse")
library(tidyverse)

install.packages("readxl")
library(readxl)

install.packages("data.table")
library(data.table)

install.packages("jsonlite")
library(jsonlite)

install.packages("lubridate")
library(lubridate)

install.packages("dummies")
library(dummies)

##Reading in the dataset
Daily_Agg <- read_sas("C:/Users/bvarghese/Documents/GitHub/OpenSourceProgramming/Group Assignment/RawDataIIUserDailyAggregation.sas7bdat")

dataset1 <- read_sas("C:/Users/bvarghese/Documents/GitHub/OpenSourceProgramming/Group Assignment/AnalyticDataInternetGambling.sas7bdat")

pokerchip = read_sas("C:/Users/bvarghese/Documents/GitHub/OpenSourceProgramming/Group Assignment/RawDataIIIPokerChipConversions.sas7bdat")

demographics <- read_sas("C:/Users/bvarghese/Documents/GitHub/OpenSourceProgramming/Group Assignment/RawDataIDemographics.sas7bdat")

##Daily User Aggregation

# 1. Inspecting the data
dim(Daily_Agg)
head(Daily_Agg)

# 2. Working with the date variable
#Changing the date from a character variable to the date format
Daily_Agg$Date <- as.Date(Daily_Agg$Date, format = "%Y%m%d")

#Extracting the Month and Day from the Date and creating two new variables 
Daily_Agg["Month"] <- format(Daily_Agg$Date, "%b")
Daily_Agg["Day"] <- format(Daily_Agg$Date, "%a")

# 3. Renaming Observations

# 4. Creating variables for single and unique rows for each UserID

#Dataframe for all the totals across Feb to Sept
Totals <- Daily_Agg%>%
  group_by(UserID)%>%
  summarize(All_stakes = sum(Stakes), All_winnings = sum(Winnings), All_bets = sum(Bets), 
            min_Stake = min(Stakes), max_Stake = max(Stakes), min_Winning = min(Winnings), max_Winning = max(Winnings))
head(Totals)

#Totals by ProductID
Product_totals <- Daily_Agg%>%
  group_by(UserID, ProductID)%>%
  summarize(total_stakes = sum(Stakes), total_winnings = sum(Winnings), total_bets = sum(Bets))%>%
  pivot_wider(names_from = "ProductID", values_from = c("total_stakes", "total_winnings", "total_bets"))

#Replacing NAs with 0
Product_totals[is.na(Product_totals)] <- 0

head(Product_totals)

#Monthly Totals
Month_totals <- Daily_Agg%>%
  group_by(UserID, Month)%>%
  summarize(stakes = sum(Stakes), winnings = sum(Winnings), bets = sum(Bets))%>%
  mutate(ROI = ((winnings - stakes)/stakes))

#Rounding off to two decimal places
Month_totals$ROI <- round(Month_totals$ROI, digits =2)

#Pivoting the data
Month_totals <- Month_totals%>%
  pivot_wider(names_from = "Month", values_from = c("stakes", "winnings", "bets", "ROI"))

#Replacing NAs with 0
Month_totals[is.na(Month_totals)] <- 0

head(Month_totals)

#Daily Totals
Day_totals <- Daily_Agg%>%
  group_by(UserID, Day)%>%
  summarize(stakes = sum(Stakes), winnings = sum(Winnings), bets = sum(Bets))%>%
  pivot_wider(names_from = "Day", values_from = c("stakes", "winnings", "bets"))

#Replacing NAs with 0
Day_totals[is.na(Day_totals)] <- 0

head(Day_totals)

# 5. Merging the dataframes
M_1 <- merge(Totals, Product_totals, all = TRUE)

M_2 <- merge(M_1, Month_totals, all = TRUE)

M_3 <- merge(M_2, Day_totals, all = TRUE)

head(M_3)

# 6. Additional new variables
M_4 <- M_3%>%
  mutate(Per_change = ((winnings_Sep -winnings_Feb) / winnings_Feb))

M_5 <- M_4%>%
  rename(total_stakes_SportFO = total_stakes_1       , total_stakes_SportLA = total_stakes_2                                                  , total_stakes_CBM = total_stakes_4 ,
         total_stakes_Supertoto= total_stakes_5      , total_stakes_GameVS = total_stakes_6      , total_stakes_Gamebwin = total_stakes_7     , total_stakes_Chartwell = total_stakes_8 , 
         total_winnings_SportFO = total_winnings_1   , total_winnings_SportLA = total_winnings_2                                              , total_winnings_CBM = total_winnings_4 , 
         total_winnings_Supertoto = total_winnings_5 , total_winnings_GameVS = total_winnings_6  , total_winnings_Gamebwin = total_winnings_7 , total_winnings_Chartwell = total_winnings_8 , 
         total_bets_SportFO = total_bets_1           , total_bets_SportLA = total_bets_2                                                      , total_bets_CBM = total_bets_4 , 
         total_bets_Supertoto = total_bets_5         , total_bets_GameVS = total_bets_6          , total_bets_Gamebwin = total_bets_7         , total_bets_Chartwell = total_bets_8 )

##Analytical Internet Gambling Data

# 1. Replacing observations with meaningful values

#merging newly created county and language tables with the exisiting one to replace the codes with names
countrytable <- read_excel("C:/Users/bvarghese/Desktop/IESEG Slides/Open Source Tools_R- Mathijs/Group Project/Countrycodesuncleaned.xlsx")
languagetable <- read_excel("C:/Users/bvarghese/Desktop/IESEG Slides/Open Source Tools_R- Mathijs/Group Project/languages.xlsx")
install.packages("maps")
install.packages("maptools")
install.packages("ggmap")

library(maptools)
library(maps)
library(ggmap)

register_google(key = "AIzaSyAnX8eeXBLkTY94KWyWm7b2eO1SbkJfotk")

#convert country into long and lat 
ll.visited <- geocode(countrytable$name)
countrytable$long <- ll.visited$lon
countrytable$lat <- ll.visited$lat

languagetable$LANGUAGE <- languagetable$Language
countrytable$COUNTRY <- countrytable$number
languagetable$Language<-countrytable$number<- NULL
datasetlanguage <- merge(dataset1,languagetable,by ="LANGUAGE")
datafull <- merge(datasetlanguage,countrytable,by ="COUNTRY")
datafull$countryname <- datafull$name

#dropping the unnecessary language and countrynumber variables
datafull$COUNTRY<-datafull$LANGUAGE<- datafull$Language <- datafull$number <- datafull$name<- NULL

#Renaming columns
colnames(datafull)[1] <- "UserID"
colnames(datafull)[18] <- "Language"
colnames(datafull)[21] <- "Country"
colnames(datafull)[4] <- "Gender"

#now rounding up to two decimal spaces
datafull$FOTotalStakes <- round(datafull$FOTotalStakes, digits =2)
datafull$FOTotalWinnings <-round(datafull$FOTotalWinnings, digits =2)
datafull$LATotalStakes <- round(datafull$LATotalStakes,digits =2)
datafull$LATotalWinnings <- round(datafull$LATotalWinnings, digits = 2)

# 2. Classifying observations into different tiers
#the following code enables to only select the bottom 95% (just insert the variable)
#I've put it in comments because me myself would keep te values of all variables 
#quantile(datafull$LATotalBets, 0.95)
#datafull[datafull$LATotalBets < quantile(datafull$LATotalBets, 0.95), ]

#using rowSums to handle missing values
datafull$Totalstakes <- rowSums(datafull[,c("FOTotalStakes","LATotalStakes")],na.rm=TRUE)
datafull$TotalBets <- rowSums(datafull[,c("FOTotalBets","LATotalBets")],na.rm=TRUE)

#creating new variable: betting tiers (for bets and amount) using dplyr
#table_ages <- subset(infert, select=datafull[,c("Totalstakes")])
#summary(table_ages)
summarydatabets <- summary(datafull$TotalBets)
summarydatastakes <- summary(datafull$Totalstakes)

#getting the 25th and 75th quartiles of both total columns
summarydatabets[2][1]
summarydatabets[5][1]

summarydatastakes[2][1]
summarydatastakes[5][1]

#looping to assess a tier name to each customer
length <- length(datafull$FOTotalBets)
for (x in (1:length)){
  if(datafull$TotalBets[x]<=summarydatabets[2][1])  {
    datafull$TierBets[x] = "lower25"
  }
  else if (datafull$TotalBets[x]<=summarydatabets[5][1]) {
    datafull$TierBets[x] = "middle50"
  }
  else {(datafull$TierBets[x] = "upper25")}
}
#for stakes
for (x in (1:length)){
  if(datafull$Totalstakes[x]<=summarydatastakes[2][1])  {
    datafull$TierStakes[x] = "lower25"
  }
  else if (datafull$Totalstakes[x]<=summarydatastakes[5][1]) {
    datafull$TierStakes[x] = "middle50"
  }
  else {(datafull$TierStakes[x] = "upper25")}
}

head(datafull)

##Pokerchip Conversion Data

#Formating the date
pokerchip$TransDateTime = ymd_hms(pokerchip$TransDateTime)

# Separating date & hours for further analysis
pokerchip <- separate(pokerchip, TransDateTime, into = c('date','hour'), sep = " ")

pokerchip$date = ymd(pokerchip$date)
pokerchip$hour = hms(pokerchip$hour)

#Creating dummies for the TransType
pokerchip$dummy = dummy(pokerchip$TransType)

#Fixing the columns names
pokerchip$TransTypeBuy = pokerchip$dummy[,"TransType24"]
pokerchip$TransTypeSell = pokerchip$dummy[,"TransType124"]
pokerchip$dummy = NULL
pokerchip$TransType = NULL
colnames(pokerchip)

#Sellside Analysis
Sellside = filter(pokerchip, TransTypeSell == 1)
sum(pokerchip$TransTypeSell)
sum(Sellside$TransAmount)
mean(Sellside$TransAmount)

#group by SellSide
Sellside_byid = group_by(Sellside, UserID)
Sellside_byid = summarise(Sellside_byid,
                          poker_meansell = mean(TransAmount, na.rm = TRUE),
                          poker_sumsell = sum(TransAmount, na.rm = TRUE),
                          poker_countsell = poker_sumsell / poker_meansell,
                          poker_sdsell = sd(TransAmount, na.rm = TRUE), 
                          poker_meadiansell = median(TransAmount, na.rm = TRUE))

#BuySide Analysis
Buyside = filter(pokerchip, TransTypeBuy == 1)
sum(pokerchip$TransTypeBuy)
sum(Buyside$TransAmount)
sum(Sellside$TransAmount) - sum(Buyside$TransAmount)
mean(Buyside$TransAmount)

#group by BuySide
Buyside_byid = group_by(Buyside, UserID)
Buyside_byid = summarise(Buyside_byid, 
                         poker_meanbuy = mean(TransAmount, na.rm = TRUE),
                         poker_sumbuy = sum(TransAmount, na.rm = TRUE),
                         poker_countbuy = poker_sumbuy / poker_meanbuy,
                         poker_maxbuy = max(TransAmount),
                         poker_minbuy = min(TransAmount),
                         poker_sdbuy = sd(TransAmount, na.rm = TRUE), 
                         poker_meadianbuy = median(TransAmount, na.rm = TRUE))

# Creating Final table sumarizing the Sell and Buy

Poker_byid = group_by(pokerchip, UserID)
Poker_byid = summarise(Poker_byid)
Buyside_byid
Poker_byid = left_join(Poker_byid,Buyside_byid, by = c("UserID"="UserID"))
Poker_byid = left_join(Poker_byid,Sellside_byid, by = c("UserID"="UserID"))

Poker_byid$poker_result = Poker_byid$poker_sumsell - Poker_byid$poker_sumbuy
Poker_byid$poker_result_percentage = Poker_byid$poker_sumsell / Poker_byid$poker_sumbuy

colnames(Poker_byid)
Poker_byid

# Extract excel for futher tableau analysis

#write.csv(Buyside, "BuySide.csv")
#write.csv(Sellside, "SellSide.csv")

##Demographics Data

#Relacing country and language numbers with the respective names

countrytab <- read_excel("C:/Users/bvarghese/Desktop/IESEG Slides/Open Source Tools_R- Mathijs/Group Project/Countrycodesuncleaned.xlsx")
languagetab <- read_excel("C:/Users/bvarghese/Desktop/IESEG Slides/Open Source Tools_R- Mathijs/Group Project/languages.xlsx")
countrytab$Country <- countrytab$number
countrytab$number<- NULL
demolang <- merge(demographics,languagetab,by ="Language")
demo <- merge(demolang,countrytab,by ="Country")

#Dropping the columns with the country and language codes
demo$Country<-demo$Language<- NULL

#Renaming the newly added columns
colnames(demo)[11] <- "Language"
colnames(demo)[12] <- "Country"

#Viewing the results
head(demo, n=2)

#Changing the date from a character variable to the date format
demo$FirstPay <- as.Date(demo$FirstPay, format = "%Y%m%d")
demo$FirstAct <- as.Date(demo$FirstAct, format = "%Y%m%d")
demo$FirstSp <- as.Date(demo$FirstSp, format = "%Y%m%d")
demo$FirstCa <- as.Date(demo$FirstCa, format = "%Y%m%d")
demo$FirstGa <- as.Date(demo$FirstGa, format = "%Y%m%d")
demo$FirstPo <- as.Date(demo$FirstPo, format = "%Y%m%d")

#Relacing the application id with text
appendix <- read_excel("C:/Users/bvarghese/Desktop/IESEG Slides/Open Source Tools_R- Mathijs/Group Project/Appendix_4.xlsx")
demo_id <- merge(demo, appendix, by = "ApplicationID", all.x = TRUE)

#Dropping the column
demo_id$ApplicationID <- NULL

#Renmaing the newly added column 
colnames(demo_id)[12] <- "Application_Desc"

#Checking the dimensions of the datsets before the merge
dim(M_4)
dim(datafull)
dim(Poker_byid)
dim(demo_id)

## Merging all Datasets

datafull_poker <- merge(datafull, Poker_byid, by = "UserID", all.x = TRUE)
merge1_demo <- merge(demo_id, datafull_poker, by = c("UserID", "Language", "Country", "Gender"), all.x = TRUE)
final_data <- merge(M_5, merge1_demo, by = "UserID", all.x = TRUE)

#Dropping columns with duplicate data
final_data$RegistrationDate <- NULL

#cleaning the final table more 

#using dplyr to round the remaining columns
final_data <- final_data %>% 
  mutate_if(is.numeric, round, digits = 2)
#if the columns contain NA and are numeric converting NA to 0 (dates remain NA)
final_data <- final_data %>% 
mutate_if(is.numeric, ~replace(., is.na(.), 0))
final_data$Gender <- ifelse(final_data$Gender == 0,"Female","Male" )

#add countrycoordinates 

countrytable$Country <- countrytable$name
head(countrytable)
final_data <- merge(final_data,countrytable,by ="Country")
final_data$countryname <- final_data$lat.x <- NULL

#write.csv(final_data, "Final_Data_Table.csv")
