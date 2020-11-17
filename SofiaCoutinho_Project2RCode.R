#####################################
### Getting and Cleaning the Data ###
#####################################


Disney <- read.csv("https://query.data.world/s/btiz7vu77feguves7tzgd4omhkxseh", header=TRUE, stringsAsFactors=FALSE)


#Changing column names:

names(Disney)[1]<- "MovieTitle" 
names(Disney)[2]<- "ReleaseDate"
names(Disney)[3]<- "Genre"
names(Disney)[4]<- "MPAARating"
names(Disney)[5]<- "TotalGrossRevenue"
names(Disney)[6]<- "InflationAdjustedGrossRevenue"


#Coercing variable types:

Disney$Genre <- as.factor(Disney$Genre)
Disney$Genre <- relevel(Disney$Genre, "Concert/Performance") #Concert/Performance Genre as first level, for reference in regression analysis below

Disney$MPAARating <- as.factor(Disney$MPAARating)

Disney$TotalGrossRevenue <- as.numeric(gsub("[\\$,]", "", Disney$TotalGrossRevenue)) #Make Total Gross Revenue column a numeric variable

Disney$InflationAdjustedGrossRevenue <- as.numeric(gsub("[\\$,]", "", Disney$InflationAdjustedGrossRevenue)) #Make Inflation Adjusted Gross Revenue column a numeric variable

Disney$ReleaseDate <- gsub("[,]", "", Disney$ReleaseDate) #Eliminate commas in Release Date observations
Disney$ReleaseDate <- (strptime(Disney$ReleaseDate, format = "%b %e %Y" )) #Make Release Date column a date variable
Disney$ReleaseDate <- as.POSIXct(Disney$ReleaseDate)


#Removing values:

Disney$TotalGrossRevenue[Disney$TotalGrossRevenue<1]<- NA #Clean any data point with Revenue=0
Disney$InflationAdjustedGrossRevenue[Disney$InflationAdjustedGrossRevenue<1]<- NA ##Clean any data point with Adjusted Revenue=0

Disney$MPAARating[Disney$MPAARating=="Not Rated"]<- NA #Make NA movies that are Not Rated
Disney$MPAARating[Disney$MPAARating==""]<- NA #Make NA Observations that are empty
Disney$Genre[Disney$Genre==""]<- NA #Make NA Observations that are empty

#Overall Structure:

summary(Disney)
str(Disney)
table(is.na(Disney))



######################################
######## Regression Analysis #########
######################################


#Partitioning Data:

p<- .7 #percentage of data

obs_count<- dim(Disney)[1] #number of observations (rows) in the dataframe

training_size <- floor(p * obs_count)

set.seed(1234) #make partition reproducible

train_ind <- sample(obs_count, size = training_size) #create a vector with the shuffled row numbers of the original dataset

TrainingDis <- Disney[train_ind, ] #pulls random rows for training
TestingDis <- Disney[-train_ind, ] #pulls random rows for testing


#Checking dimensions of partitioned data:

dim(TrainingDis)
dim(TestingDis)


#Creating the models:

M1<- lm(TotalGrossRevenue ~ 0 + MPAARating, TrainingDis)
summary(M1)

M2<- lm(TotalGrossRevenue ~ 0 + Genre, TrainingDis)
summary(M2)

M3<- lm(TotalGrossRevenue ~ 0 + MPAARating + Genre, TrainingDis)
summary(M3)

M4<- lm(TotalGrossRevenue ~ 0 + MPAARating + Genre + ReleaseDate, TrainingDis)
summary(M4)

M5<- lm(TotalGrossRevenue ~ 0 + MPAARating + Genre + ReleaseDate + InflationAdjustedGrossRevenue, TrainingDis)
summary(M5)



#Model 1 Error Testing

PRED1_IN <- predict(M1, TrainingDis) #generate predictions on the (in-sample) training data
PRED1_OUT <- predict(M1, TestingDis) #generate predictions on the (out-of-sample) testing data

RMSE1_IN<-sqrt((sum((PRED1_IN-TrainingDis$TotalGrossRevenue)^2, na.rm = TRUE))/length(PRED1_IN))  #computes in-sample error
RMSE1_OUT<-sqrt((sum((PRED1_OUT-TestingDis$TotalGrossRevenue)^2, na.rm=TRUE))/length(PRED1_OUT)) #computes out-of-sample 

RMSE1_IN
RMSE1_OUT



#Model 2 Error Testing

PRED2_IN <- predict(M2, TrainingDis) #generate predictions on the (in-sample) training data
PRED2_OUT <- predict(M2, TestingDis) #generate predictions on the (out-of-sample) testing data

RMSE2_IN<-sqrt((sum((PRED2_IN-TrainingDis$TotalGrossRevenue)^2, na.rm = TRUE))/length(PRED2_IN))  #computes in-sample error
RMSE2_OUT<-sqrt((sum((PRED2_OUT-TestingDis$TotalGrossRevenue)^2, na.rm=TRUE))/length(PRED2_OUT)) #computes out-of-sample 

RMSE2_IN
RMSE2_OUT



#Model 3 Error Testing

PRED3_IN <- predict(M3, TrainingDis) #generate predictions on the (in-sample) training data
PRED3_OUT <- predict(M3, TestingDis) #generate predictions on the (out-of-sample) testing data

RMSE3_IN<-sqrt((sum((PRED3_IN-TrainingDis$TotalGrossRevenue)^2, na.rm = TRUE))/length(PRED3_IN))  #computes in-sample error
RMSE3_OUT<-sqrt((sum((PRED3_OUT-TestingDis$TotalGrossRevenue)^2, na.rm=TRUE))/length(PRED3_OUT)) #computes out-of-sample 

RMSE3_IN
RMSE3_OUT



#Model 4 Error Testing

PRED4_IN <- predict(M4, TrainingDis) #generate predictions on the (in-sample) training data
PRED4_OUT <- predict(M4, TestingDis) #generate predictions on the (out-of-sample) testing data

RMSE4_IN<-sqrt((sum((PRED4_IN-TrainingDis$TotalGrossRevenue)^2, na.rm = TRUE))/length(PRED4_IN))  #computes in-sample error
RMSE4_OUT<-sqrt((sum((PRED4_OUT-TestingDis$TotalGrossRevenue)^2, na.rm=TRUE))/length(PRED4_OUT)) #computes out-of-sample 

RMSE4_IN
RMSE4_OUT



#Model 5 Error Testing

PRED5_IN <- predict(M5, TrainingDis) #generate predictions on the (in-sample) training data
PRED5_OUT <- predict(M5, TestingDis) #generate predictions on the (out-of-sample) testing data

RMSE5_IN<-sqrt((sum((PRED5_IN-TrainingDis$TotalGrossRevenue)^2, na.rm = TRUE))/length(PRED5_IN))  #computes in-sample error
RMSE5_OUT<-sqrt((sum((PRED5_OUT-TestingDis$TotalGrossRevenue)^2, na.rm=TRUE))/length(PRED5_OUT)) #computes out-of-sample 

RMSE5_IN
RMSE5_OUT


#Comparing Model Errors

Error_IN <- c(RMSE1_IN ,RMSE2_IN, RMSE3_IN, RMSE4_IN, RMSE5_IN)
Error_IN

Error_OUT <- c(RMSE1_OUT ,RMSE2_OUT, RMSE3_OUT, RMSE4_OUT, RMSE5_OUT)
Error_OUT
