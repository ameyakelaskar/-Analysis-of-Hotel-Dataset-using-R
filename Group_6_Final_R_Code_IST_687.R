################################################ GROUP 6 PROJECT CODE ################################################
#MEMEBERS:
#1) RAHUL JADHAV
#2) CAMERON MITCHELL
#3) TERESA TRAN
#4) QINGYANG LIU
#5) AMEYA KELASKAR
#######################################################################################################################

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(arules)
library(imputeTS)
library(ggplot2)

#----------------------------------------------------------------------------------------------------#

############# CODE FOR PROMOTIONAL IMPROVEMENTS AND CUSTOMER SATISFACTION IMPROVEMENTS  ##############


#----------------------------------------------------------------------------------------------------#

######################################## FOR CITY ANALYSIS ###########################################

#----------------------------------------------------------------------------------------------------#
#### data cleaning_city ####

city <- read_excel("H2-City.xlsx") 

str(city)
summary(city)

# remove the unhelpful columns
dfc <- city %>% mutate(
  Children = as.numeric(Children) + Babies,
  Babies = NULL,
  Agent = NULL,
  DaysInWaitingList = NULL,
  Company = NULL
  
) 

colnames(dfc[3]) <- 'ArrivalDate' 
# change `Arrival Date` into 'ArrivalDate'
summary(dfc)

# tackle NA

dfc$Children <- replace_na(dfc$Children, 0)
summary(dfc$Children)

table(dfc$ReservationStatus[is.na(dfc$`Arrival Date`)])
# in this step we decide to keep the NA rows



#----------------------------------------------------------------------------------------------------#



#### seasonality ####

df_season <- dfc %>% mutate(
  month = month(ReservationStatusDate),
  year = year(ReservationStatusDate),
  # `Arrival Date` = NULL,
  ReservationStatusDate = NULL,
  season = case_when(
    month <= 5 & month >= 3 ~ "Spring",
    month <= 8 & month >= 6 ~ "Summer",
    month <= 11 & month >= 9 ~ "Fall",
    TRUE ~ "Winter"
  )
)

df_season$season <- factor(df_season$season, levels = c("Spring"
                                                        , "Summer"
                                                        , "Fall"
                                                        , "Winter"))
df_season <- df_season[order(df_season$season),]


#----------------------------------------------------------------------------------------------------#



# three types of status by season

ggplot(data = df_season, aes(x = as.factor(season), fill = ReservationStatus)) +
  geom_bar(stat = "count", width = 0.7) +
  theme_minimal()

#Per season, the higest cancellation rate in Winter and highest check-out rate in summer.

ggplot(data = df_season, aes(x = as.factor(month), fill = ReservationStatus)) +
  geom_bar(stat = "count", width = 0.7) +
  theme_minimal()

#Per month, the highest cancellation in January, and highest check-out rate in August.

ggplot(data = df_season, aes(x = as.factor(year), fill = ReservationStatus)) +
  geom_bar(stat = "count", width = 0.7) +
  theme_minimal()

#Year 2016 has the highest both check-out & cancelled.


#----------------------------------------------------------------------------------------------------#



##Focusing on only Bar graphs for status = canceled

# Season Bar Graph
ggplot(data = df_season[df_season$ReservationStatus == 'Canceled',]
       , aes(x = as.factor(season))) +
  geom_bar(stat = "count", width = 0.7
           , fill = "steelblue") +
  theme_minimal()
#highest cancel rate in Winter.


# Season Bar Graph
ggplot(data = df_season[df_season$ReservationStatus == 'Canceled',]
       , aes(x = as.factor(month))) +
  geom_bar(stat = "count", width = 0.7
           , fill = "steelblue") +
  theme_minimal()
#Highest cancel in Jan, second highest in July and Oct.

ggplot(data = df_season[df_season$ReservationStatus == 'Canceled',]
       , aes(x = as.factor(year))) +
  geom_bar(stat = "count", width = 0.7
           , fill = "steelblue") +
  theme_minimal()
#Highest cancel rate in 2016



#----------------------------------------------------------------------------------------------------#



#### customer -- cancellation ####


# no show != cancel --> hotel got the money


df_cus <- dfc %>% mutate(
  IsCanceled = NULL,
  LeadTime = NULL,
  `Arrival Date` = NULL,
  ReservationStatusDate = NULL,
  ReservationStatus = as.factor(ReservationStatus),
  StaysInWeekendNights = StaysInWeekendNights,
  StaysInWeekNights = StaysInWeekNights,
  Adults = Adults,
  Children = Children,
  Meal = as.factor(Meal),
  Country = as.factor(Country),
  MarketSegment = as.factor(MarketSegment),
  DistributionChannel = NULL,
  IsRepeatedGuest = as.factor(IsRepeatedGuest),
  PreviousCancellations = PreviousCancellations,
  PreviousBookingsNotCanceled = PreviousBookingsNotCanceled,
  ReservedRoomType = as.factor(ReservedRoomType),
  AssignedRoomType = as.factor(AssignedRoomType),
  ADR = NULL,
  DepositType = NULL,
  CustomerType = as.factor(CustomerType),
  RequiredCarParkingSpaces = as.factor(RequiredCarParkingSpaces),
  TotalOfSpecialRequests = as.factor(TotalOfSpecialRequests)
)
str(df_cus)
summary(df_cus)


#----------------------------------------------------------------------------------------------------#


# association rules

mt_cus <- as(df_cus,"transactions")
#inspect(mt_cus)
crules <- apriori(mt_cus, parameter = list(supp = 0.05, conf = 0.9, minlen = 4),appearance = list(rhs="ReservationStatus=Canceled"))
# inspect(crules)

# explore the top ten rules
inspect(sort(crules,by="lift")[1:10])
inspect(sort(crules,by="support")[1:10])
inspect(sort(crules,by="confidence")[1:10])

#### ADR -- prediction #####


df_adr <- df_season %>% mutate(
  IsCanceled = NULL,
  LeadTime = NULL,
  `Arrival Date` = NULL,
  year = as.factor(year),
  season = as.factor(season),
  ReservationStatus = as.factor(ReservationStatus),
  StaysInWeekendNights = StaysInWeekendNights,
  StaysInWeekNights = StaysInWeekNights,
  Adults = Adults,
  Children = Children,
  Meal = as.factor(Meal),
  Country = as.factor(Country),
  MarketSegment = as.factor(MarketSegment),
  DistributionChannel = NULL,
  IsRepeatedGuest = as.factor(IsRepeatedGuest),
  PreviousCancellations = PreviousCancellations,
  PreviousBookingsNotCanceled = PreviousBookingsNotCanceled,
  ReservedRoomType = as.factor(ReservedRoomType),
  AssignedRoomType = as.factor(AssignedRoomType),
  ADR = ADR,
  DepositType = NULL,
  CustomerType = as.factor(CustomerType),
  RequiredCarParkingSpaces = as.factor(RequiredCarParkingSpaces),
  TotalOfSpecialRequests = as.factor(TotalOfSpecialRequests)
)

df_adr$ReservationStatusDate = dfc$ReservationStatusDate

str(df_adr)
summary(df_adr)

df_adr$newmonth <- format(df_adr$ReservationStatusDate, format="%Y-%m")
df_adr$newseason <- paste0(df_adr$year, '-', df_adr$season)

dfm <- aggregate(df_adr$ADR, by=list(df_adr$newmonth), FUN=mean)
dfs <- aggregate(df_adr$ADR, by=list(df_adr$newseason), FUN=mean)

dfs$Group.1 <- factor(dfs$Group.1, levels = c("2014-Fall"
                                              , "2015-Spring"
                                              , "2015-Summer"
                                              , "2015-Fall"
                                              , "2015-Winter"
                                              , "2016-Spring"
                                              , "2016-Summer"
                                              , "2016-Fall"
                                              , "2016-Winter"
                                              , "2017-Spring"
                                              , "2017-Summer"
                                              , "2017-Fall"
                                              , "2017-Winter"))
dfs <- dfs[order(dfs$Group.1),]

#----------------------------------------------------------------------------------------------------#


# monthly

ggplot(data = dfm, mapping = aes(x = as.factor(Group.1), y = x, group = 1)) + geom_line()

# season

ggplot(data = dfs, mapping = aes(x = as.factor(Group.1), y = x, group = 1)) + geom_line()


#----------------------------------------------------------------------------------------------------#



# predict by regression

library(caret)
library(leaps)

ADRCity = lm(formula = ADR ~ ReservationStatus + IsRepeatedGuest + PreviousCancellations + PreviousBookingsNotCanceled + TotalOfSpecialRequests, data = df_adr)
summary(ADRCity)

#trend


#----------------------------------------------------------------------------------------------------#

########################################FOR RESORT ANALYSIS#####################################

###################################### data cleaning_resort ####################################
resort <- read_excel("H1-Resort.xlsx")

str(resort)
summary(resort)

# remove the unhelpful columns
dfre <- resort %>% mutate(
  Children = as.numeric(Children) + Babies,
  Babies = NULL,
  Agent = NULL,
  DaysInWaitingList = NULL,
  Company = NULL
  
) 

# colnames(dfc[3]) <- 'ArrivalDate' # change `Arrival Date` into 'ArrivalDate'
summary(dfre)

# tackle NA

dfre$Children <- replace_na(dfre$Children, 0)
summary(dfre$Children)

table(dfre$ReservationStatus[is.na(dfre$`Arrival Date`)])
# in this step we decide to keep the NA rows



#----------------------------------------------------------------------------------------------------#


#### seasonality ####

df_season <- dfre %>% mutate(
  month = month(ReservationStatusDate),
  year = year(ReservationStatusDate),
  # `Arrival Date` = NULL,
  ReservationStatusDate = NULL,
  season = case_when(
    month <= 5 & month >= 3 ~ "Spring",
    month <= 8 & month >= 6 ~ "Summer",
    month <= 11 & month >= 9 ~ "Fall",
    TRUE ~ "Winter"
  )
)

df_season$season <- factor(df_season$season, levels = c("Spring"
                                                        , "Summer"
                                                        , "Fall"
                                                        , "Winter"))
df_season <- df_season[order(df_season$season),]


#----------------------------------------------------------------------------------------------------#


# three types of statue

ggplot(data = df_season, aes(x = as.factor(season), fill = ReservationStatus)) +
  geom_bar(stat = "count", width = 0.7) +
  theme_minimal()

ggplot(data = df_season, aes(x = as.factor(month), fill = ReservationStatus)) +
  geom_bar(stat = "count", width = 0.7) +
  theme_minimal()

ggplot(data = df_season, aes(x = as.factor(year), fill = ReservationStatus)) +
  geom_bar(stat = "count", width = 0.7) +
  theme_minimal()

#Focusing on only status = canceled bar graphs

ggplot(data = df_season[df_season$ReservationStatus == 'Canceled',]
       , aes(x = as.factor(season))) +
  geom_bar(stat = "count", width = 0.7
           , fill = "steelblue") +
  theme_minimal()

ggplot(data = df_season[df_season$ReservationStatus == 'Canceled',]
       , aes(x = as.factor(month))) +
  geom_bar(stat = "count", width = 0.7
           , fill = "steelblue") +
  theme_minimal()

ggplot(data = df_season[df_season$ReservationStatus == 'Canceled',]
       , aes(x = as.factor(year))) +
  geom_bar(stat = "count", width = 0.7
           , fill = "steelblue") +
  theme_minimal()


#----------------------------------------------------------------------------------------------------#


#### customer -- cancellation ####

# no show != cancel --> hotel got the money
df_cus <- dfre %>% mutate(
  IsCanceled = NULL,
  LeadTime = NULL,
  `Arrival Date` = NULL,
  ReservationStatusDate = NULL,
  ReservationStatus = as.factor(ReservationStatus),
  StaysInWeekendNights = StaysInWeekendNights,
  StaysInWeekNights = StaysInWeekNights,
  Adults = Adults,
  Children = Children,
  Meal = as.factor(Meal),
  Country = as.factor(Country),
  MarketSegment = as.factor(MarketSegment),
  DistributionChannel = NULL,
  IsRepeatedGuest = as.factor(IsRepeatedGuest),
  PreviousCancellations = PreviousCancellations,
  PreviousBookingsNotCanceled = PreviousBookingsNotCanceled,
  ReservedRoomType = as.factor(ReservedRoomType),
  AssignedRoomType = as.factor(AssignedRoomType),
  ADR = NULL,
  DepositType = NULL,
  CustomerType = as.factor(CustomerType),
  RequiredCarParkingSpaces = as.factor(RequiredCarParkingSpaces),
  TotalOfSpecialRequests = as.factor(TotalOfSpecialRequests)
)
str(df_cus)
summary(df_cus)


#----------------------------------------------------------------------------------------------------#


# association rules

mt_cus <- as(df_cus,"transactions")
#inspect(mt_cus)
crules <- apriori(mt_cus, parameter = list(supp = 0.05, conf = 0.9, minlen = 4),appearance = list(rhs="ReservationStatus=Canceled"))
# inspect(crules)

# explore the top ten rules
inspect(sort(crules,by="lift")[1:10])
inspect(sort(crules,by="support")[1:10])
inspect(sort(crules,by="confidence")[1:10])


#----------------------------------------------------------------------------------------------------#



#### ADR -- prediction #####

df_adr <- df_season %>% mutate(
  IsCanceled = NULL,
  LeadTime = NULL,
  `Arrival Date` = NULL,
  year = as.factor(year),
  season = as.factor(season),
  ReservationStatus = as.factor(ReservationStatus),
  StaysInWeekendNights = StaysInWeekendNights,
  StaysInWeekNights = StaysInWeekNights,
  Adults = Adults,
  Children = Children,
  Meal = as.factor(Meal),
  Country = as.factor(Country),
  MarketSegment = as.factor(MarketSegment),
  DistributionChannel = NULL,
  IsRepeatedGuest = as.factor(IsRepeatedGuest),
  PreviousCancellations = PreviousCancellations,
  PreviousBookingsNotCanceled = PreviousBookingsNotCanceled,
  ReservedRoomType = as.factor(ReservedRoomType),
  AssignedRoomType = as.factor(AssignedRoomType),
  ADR = ADR,
  DepositType = NULL,
  CustomerType = as.factor(CustomerType),
  RequiredCarParkingSpaces = as.factor(RequiredCarParkingSpaces),
  TotalOfSpecialRequests = as.factor(TotalOfSpecialRequests)
)
df_adr$ReservationStatusDate = dfre$ReservationStatusDate

str(df_adr)
summary(df_adr)

df_adr$newmonth <- format(df_adr$ReservationStatusDate, format="%Y-%m")
df_adr$newseason <- paste0(df_adr$year, '-', df_adr$season)

dfm <- aggregate(df_adr$ADR, by=list(df_adr$newmonth), FUN=mean)
dfs <- aggregate(df_adr$ADR, by=list(df_adr$newseason), FUN=mean)

dfs$Group.1 <- factor(dfs$Group.1, levels = c("2014-Fall"
                                              , "2015-Spring"
                                              , "2015-Summer"
                                              , "2015-Fall"
                                              , "2015-Winter"
                                              , "2016-Spring"
                                              , "2016-Summer"
                                              , "2016-Fall"
                                              , "2016-Winter"
                                              , "2017-Spring"
                                              , "2017-Summer"
                                              , "2017-Fall"
                                              , "2017-Winter"))
dfs <- dfs[order(dfs$Group.1),]


#----------------------------------------------------------------------------------------------------#


# monthly

ggplot(data = dfm, mapping = aes(x = as.factor(Group.1), y = x, group = 1)) + geom_line()

# season

ggplot(data = dfs, mapping = aes(x = as.factor(Group.1), y = x, group = 1)) + geom_line()


#----------------------------------------------------------------------------------------------------#


################################ CODE FOR MARKET IMPROVEMENTS ########################################




#----------------------------------------------------------------------------------------------------#

#CLEANING THE ENVIRONMENT
# Run these three functions to get a clean 
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear user objects from the environment



city <- read_excel("H2-City.xlsx") 
resort <- read_excel("H1-Resort.xlsx") 



#----------------------------------------------------------------------------------------------------#

################################# FOR DATA CLEANING OF RESORT DATA ###################################

#----------------------------------------------------------------------------------------------------#

library(imputeTS)

resort$`Arrival Date` <- gsub("-", "", resort$`Arrival Date`) #remove - between arrival date
resort$ReservationStatusDate <- gsub("-", "", resort$ReservationStatusDate) #remove - between reservation date
resort <- resort[,-23] #remove "company column"
which(colnames(resort)=="RequiredCarParkingSpaces") #find column number
resort <- resort[,-26] #remove "parking"
resort$`Arrival Date` <- as.numeric(resort$`Arrival Date`) #change date to numeric
resort$ReservationStatusDate <- as.numeric(resort$ReservationStatusDate)
sum(is.na(resort)) #check if there's any NAs- no NAs
which(colnames(resort)=="Agent")
resort <- resort[,-22]#remove "Agent"
which(colnames(resort)=="DaysInWaitingList") #remove "DaysInWaitingList"
resort <- resort[,-23]
resort$Children <- resort$Children + resort$Babies #merging babies with children
which(colnames(resort)=="Babies")
resort <- resort[,-10] #remove babies column
resort$AssignedEqualReserved[resort$AssignedRoomType==resort$ReservedRoomType] <- 1
resort$AssignedEqualReserved[resort$AssignedRoomType!=resort$ReservedRoomType] <- 0

#creating dummy variable for if the guests assigned room type was the same as the room type they reserved
resort$AssignedEqualReserved <- as.factor(resort$AssignedEqualReserved)
#making sure the AssignedEqualReserved variable is being treated as a factor variable

#----------------------------------------------------------------------------------------------------#

################################# FOR DATA CLEANING OF CITY DATA ###################################

#----------------------------------------------------------------------------------------------------#




indices_city <- which(is.na(city$`Arrival Date`))
#Keep all the NA values as Arrival Dates can be ignored for cancelled and check out bookings.
# Removing 39270 rows can reduce the data set a lot hence best option is to ignore the NA values for date column.

## Removed the company column
city$Company <- NULL


## NULL string in Agent column
city$Agent<- as.numeric(city$Agent) # Converted to numeric and NULL -> NA

indices_agent<- which(!is.na(city$Agent)) # Index of all non-NA values in Agent
indices_agent

mean(city$Agent[indices_agent]) # Mean before interpolation is 28.14472

city$Agent<- na.interpolation(city$Agent,option = "linear") 
mean(city$Agent) #Mean after interpolation is 28.09549


## Type convert for Children

city$Children<- as.numeric(city$Children)
city$Children<- na_interpolation(city$Children)

##Merge Babies and Children

city$Children<- city$Babies + city$Children

city$Babies <- NULL # remove Babies column

## Conversion of IsRepeatedGuest numeric to categorical

typeof(city$IsRepeatedGuest)
as.factor(city$IsRepeatedGuest) 

## Remove RequiredCarParkingSpaces column

city$RequiredCarParkingSpaces <- NULL

city$AssignedEqualReserved[city$AssignedRoomType==city$ReservedRoomType] <- 1
city$AssignedEqualReserved[city$AssignedRoomType!=city$ReservedRoomType] <- 0
#creating dummy variable for if the guests assigned room type was the same as the room type they reserved
city$AssignedEqualReserved <- as.factor(city$AssignedEqualReserved)
#making sure the AssignedEqualReserved variable is being treated as a factor variable


#----------------------------------------------------------------------------------------------------#

################################# EXPLORATORY ANALYSIS ON RESORT DATA ###################################

#----------------------------------------------------------------------------------------------------#



#boxplots exploring categorical variables relationship with ADR for resort dataset
library(ggplot2)
ggplot(resort, aes(x=ReservationStatus, y=ADR)) + geom_boxplot() 
#boxplot exploring if any sig differences between reservation status types and ADR
#no shows have a smaller ADR overall
ggplot(resort, aes(x=Meal, y=ADR)) + geom_boxplot()
#boxplot exploring if any sig differences between meal types and ADR
#SC meals have smaller ADR
ggplot(resort, aes(x=MarketSegment, y=ADR, fill="red")) + geom_boxplot(fill="green") + ggtitle("Market Segment v. ADR: Resort") + theme(plot.title = element_text(hjust = 0.5))
#boxplot exploring if any sig differences between market segment types and ADR
#complementary and corporate have smaller ADR, Direct and online TA have larger
ggplot(resort, aes(x=DistributionChannel, y=ADR)) + geom_boxplot(fill="red") + ggtitle("Distribution Channel v. ADR: Resort")  + theme(plot.title = element_text(hjust = 0.5))
#boxplot exploring if any sig differences between distribution channel types and ADR
#corporate has smaller ADR
ggplot(resort, aes(x=ReservedRoomType, y=ADR)) + geom_boxplot()
#boxplot exploring if any sig differences between Reserved Room Type types and ADR
ggplot(resort, aes(x=AssignedRoomType, y=ADR)) + geom_boxplot()
#boxplot exploring if any sig differences between Assigned Room Type types and ADR
ggplot(resort, aes(x=DepositType, y=ADR)) + geom_boxplot(fill="light blue") + ggtitle("Deposit Type v. ADR: Resort")  + theme(plot.title = element_text(hjust = 0.5))
#boxplot exploring if any sig differences between Deposit Type types and ADR
#no deposit has slightly higher ADR
ggplot(resort, aes(x=AssignedEqualReserved, y=ADR)) + geom_boxplot(fill="orange") + ggtitle("Room Type v. ADR: Resort")  + theme(plot.title = element_text(hjust = 0.5))
#boxplot exploring if any sig dif between getting reserved room and not getting it
#does appear that ADR is slightly less for those who do not get reserved room



#simple models exploring numerical variables with ADR
canceled <- lm(ADR~ IsCanceled, data=resort)
summary(canceled)
#significant positive relationship
SpecialRequests <- lm(ADR~ TotalOfSpecialRequests, data=resort)
summary(SpecialRequests)
#significant positive relationship
WaitingList <- lm(ADR~ DaysInWaitingList, data=resort)
summary(WaitingList)
#significant, negative relationship
BookingChanges <- lm(ADR~ BookingChanges, data=resort)
summary(BookingChanges)
#significant positive relationship
IsRepeatedGuest <- lm(ADR~ IsRepeatedGuest, data=resort)
summary(IsRepeatedGuest)
#Significant and negative
PreviousCancellations <- lm(ADR~ PreviousCancellations, data=resort)
summary(PreviousCancellations)
#Significant and negative
StaysInWeekendNights <- lm(ADR~ StaysInWeekendNights, data=resort)
summary(StaysInWeekendNights)
#significant positive 
StaysInWeekNights <- lm(ADR~ StaysInWeekNights, data=resort)
summary(StaysInWeekNights)
#significant positive 

#----------------------------------------------------------------------------------------------------#

################################# EXPLORATORY ANALYSIS ON RESORT DATA ###################################

#----------------------------------------------------------------------------------------------------#


#boxplots exploring categorical variables relationship with ADR for city dataset
city %>% filter(ADR < 1000) -> city
#remove one large outlier with an ADR of $5400 in order to assess rest of dataset
library(ggplot2)
ggplot(city, aes(x=ReservationStatus, y=ADR)) + geom_boxplot() 
#boxplot exploring if any sig differences between reservation status types and ADR
#no shows have a smaller ADR overall
ggplot(city, aes(x=Meal, y=ADR)) + geom_boxplot()
#boxplot exploring if any sig differences between meal types and ADR
#FB meals have smaller ADR, but not a lot of purchases of FB meal plan
ggplot(city, aes(x=MarketSegment, y=ADR)) + geom_boxplot() + geom_boxplot(fill="green") + ggtitle("Market Segment v. ADR: City") + theme(plot.title = element_text(hjust = 0.5))
#boxplot exploring if any sig differences between market segment types and ADR
#complementary and corporate have smaller ADR, Direct and online TA have larger
ggplot(city, aes(x=DistributionChannel, y=ADR)) + geom_boxplot(fill="red") + ggtitle("Distribution Channel v. ADR: City")  + theme(plot.title = element_text(hjust = 0.5))
#boxplot exploring if any sig differences between distribution channel types and ADR
#corporate has smaller ADR, direct, GDS, and TA/TO slighly higher
ggplot(city, aes(x=ReservedRoomType, y=ADR)) + geom_boxplot()
#boxplot exploring if any sig differences between Reserved Room Type types and ADR
ggplot(city, aes(x=AssignedRoomType, y=ADR)) + geom_boxplot()
#boxplot exploring if any sig differences between Assigned Room Type types and ADR
ggplot(city, aes(x=DepositType, y=ADR)) + geom_boxplot() + geom_boxplot(fill="light blue") + ggtitle("Deposit Type v. ADR: City")  + theme(plot.title = element_text(hjust = 0.5))
#boxplot exploring if any sig differences between Deposit Type types and ADR
#refundable has slightly higher ADR
ggplot(city, aes(x=AssignedEqualReserved, y=ADR)) + geom_boxplot(fill="orange") + ggtitle("Room Type v. ADR: City")  + theme(plot.title = element_text(hjust = 0.5))
#boxplot exploring if any sig dif between getting reserved room and not getting it
#does appear that ADR is slightly less for those who do not get reserved room



#simple models exploring numerical variables with ADR for city dataset
canceled <- lm(ADR~ IsCanceled, data=city)
summary(canceled)
#significant negative relationship
SpecialRequests <- lm(ADR~ TotalOfSpecialRequests, data=city)
summary(SpecialRequests)
#significant positive relationship
WaitingList <- lm(ADR~ DaysInWaitingList, data=city)
summary(WaitingList)
#significant, negative relationship
BookingChanges <- lm(ADR~ BookingChanges, data=city)
summary(BookingChanges)
#significant positive relationship
IsRepeatedGuest <- lm(ADR~ IsRepeatedGuest, data=city)
summary(IsRepeatedGuest)
#Significant and negative
PreviousCancellations <- lm(ADR~ PreviousCancellations, data=city)
summary(PreviousCancellations)
#Significant and negative
StaysInWeekendNights <- lm(ADR~ StaysInWeekendNights, data=city)
summary(StaysInWeekendNights)
#significant positive 
StaysInWeekNights <- lm(ADR~ StaysInWeekNights, data=city)
summary(StaysInWeekNights)
#significant positive 


#----------------------------------------------------------------------------------------------------#

########################################### END OF CODE #############################################

#----------------------------------------------------------------------------------------------------#


