# AirBnB-Dataset
# Predicting Airbnb new user bookings by Ramya Sreekumar, Sarita Singh, Jared Ellse, Kumar Shourabh and 


#---Load libraries
library(readr)
library(stringr)
library(caret)
library(car)

#---Set working drive
setwd("C:/Users/JE Consulting/Google Drive/Education/SDSU/SDSU - MIS620 Big Data/MIS620 Group Project/AirBNB/Misc")


#Load the complete data and repalce any 'Blanks' with 'NA' when loading the data
alldata<- read.csv("test_users vOrig.csv", header = T,sep = ",", fill = TRUE, na.strings = c("","NA"))


View(alldata) # View the data set

#==================Visualizinging Given Data set================================

# Understanding Gender data information
levels(alldata$gender)  # Gives the categories of Gender attribute

gender.freq <- table(alldata$gender) # Create a temp table to use for barplot creation

#Creates a barplot of the frequencies of Gender in the Given dataset
barplot(gender.freq, main = "Frequency Count of Different Gender Catgories", xlab = " Gender", ylab = "Frequency", border = NA, col = "blue")


# Understanding the Country destination frequency of given data
levels(alldata$country_destination)
destination.freq <- table(alldata$country_destination)
barplot(destination.freq[order(destination.freq,decreasing = T)], main = "Frequency Count of Different Destinations", xlab = "Destination", ylab = "Frequency", border = NA, col = "blue")

# Understanding the Language frequency of given data
levels(alldata$language)
lang.freq <- table(alldata$language)
barplot(lang.freq[order(lang.freq,decreasing = T)], main = "Frequency Count of Different Languages", xlab = "Destination", ylab = "Frequency", border = NA, col = "blue")

# Understanding the Age distibution in the given data
age.freq <- table(alldata$age)
barplot(age.freq[order(age.freq,decreasing = T)], main = "Frequency Count of Different Age", xlab = "Destination", ylab = "Frequency", border = NA, col = "blue")


# Understanding the Signup Method Used distibution in the given data
levels(alldata$signup_method)
signup.freq <- table(alldata$signup_method)
barplot(signup.freq[order(signup.freq,decreasing = T)], main = "Frequency Count of Different Sign Up Method used", xlab = "Destination", ylab = "Frequency", border = NA, col = "blue")

#Understanding the First_device_type used by a new User
levels(alldata$first_device_type)
firstdev.freq <- table(alldata$first_device_type)
barplot(firstdev.freq[order(firstdev.freq,decreasing = T)], main = "Frequency Count of Different First Device Used Method used", xlab = "Destination", ylab = "Frequency", border = NA, col = "blue")

#Understanding the Affiliate Provider used by Users
levels(alldata$affiliate_provider)
affilprovid.freq <- table(alldata$affiliate_provider)
barplot(affilprovid.freq[order(affilprovid.freq,decreasing = T)], main = "Frequency Count of Different Affiliate Provider Used Method used", xlab = "Destination", ylab = "Frequency", border = NA, col = "blue")

#Understanding the Affiliate Provider used by Users
levels(alldata$date_account_created.Year)
dtacctcreatd.freq <- table(alldata$date_account_created.Year)
barplot(dtacctcreatd.freq[order(dtacctcreatd.freq,decreasing = T)], main = "Frequency Count of Different Accounts got created ", xlab = "Destination", ylab = "Frequency", border = "black", col = "yellow")


#Understanding the Affiliate Channel used by Users
levels(alldata$affiliate_channel)
affilchan.freq <- table(alldata$affiliate_channel)
barplot(affilchan.freq[order(affilchan.freq,decreasing = T)], main = "Frequency Count of Different Channel Used ", xlab = "Destination", ylab = "Frequency", border = "black", col = "green")

#Understanding the First Browser used for booking
levels(alldata$first_browser)
firstbrw.freq <- table(alldata$first_browser)
barplot(firstbrw.freq[order(firstbrw.freq,decreasing = T)], main = "Frequency Count of Different First Browser Used ", xlab = "Destination", ylab = "Frequency", border = NA, col = "blue")

#Understanding the Sigup App used for booking
levels(alldata$signup_app)
signapp.freq <- table(alldata$signup_app)
barplot(signapp.freq[order(signapp.freq,decreasing = T)], main = "Frequency Count of Different First Signup App Used ", xlab = "Destination", ylab = "Frequency", border = NA, col = "blue")
#===============================================================================

#---Drop columns that are not relevant for analysis
drops <- c("id", "date_first_booking" )

new_data <- alldata [,!(names(alldata) %in% drops)]

#To split the column "date_account_created" in in year, month and day
dac = as.data.frame(str_split_fixed(new_data$date_account_created, '-', 3))
new_data['dac_year'] = dac[,1]
new_data['dac_month'] = dac[,2]
new_data['dac_day'] = dac[,3]
new_data <- new_data[,-c(which(colnames(new_data) %in% c('date_account_created')))]


# split timestamp_first_active in year, month and day
new_data[,'tfa_year'] = substring(as.character(new_data[,'timestamp_first_active']), 1, 4)
new_data['tfa_month'] = substring(as.character(new_data['timestamp_first_active']), 5, 6)
new_data['tfa_day'] = substring(as.character(new_data['timestamp_first_active']), 7, 8)
new_data = new_data[,-c(which(colnames(new_data) %in% c('timestamp_first_active')))]

#Create the cateory for the age in the data. Also, remove age that are below 15 and above 95
new_data$ageCategory <- cut(new_data$age,seq(15,95,10), right = FALSE, labels = c(1:8))

# Get the data partition which 20% of total data ( but a replica of the ratio of the orginal dataset)
set.seed(123)
index <-createDataPartition(y=new_data$country_destination, p=.2, list=FALSE, times = 1)
sampledata <- new_data[index,]
nrow(sampledata)

# Shows the ratio of the rows in the entire data set based on the country destination
prop.table(table(micedData$country_destination))

# To impute the entire data using "mice" package ( will take a few mins)
set.seed(2)
library(mice)
md.pattern(sampledata)
micedData <- mice(sampledata,m=3,maxit=4,meth='pmm',seed=500)
micedData<- complete(micedData,1)

View(micedData)
summary(sampledata)
summary(micedData) #Summarizes the new imputed data looks like


# To write the Imputed data file back to a .csv file for any time( No NAs present now)
write.table(micedData, "C:/Users/Kumar Shourabh/Desktop/Spring2016/MIS620E_Business Bigdata/Project/AirBNB/Data/ImputedAirbnb.xlsx")


micDt <- read.csv("C:/Users/Kumar Shourabh/Desktop/Spring2016/MIS620E_Business Bigdata/Project/AirBNB/Data/ImputedAirbnb.csv", header = TRUE)
View(micDt)

#Removing theAge column as we have a Age category column for it
micDt$age <- NULL

## To BALANCE THE DATA
library(DMwR)
table(micDt$country_destination)

#micDt$country_destination <- as.factor(micDt$country_destination)

SMOTEdata <- SMOTE(country_destination ~ ., micDt, perc.over = 500, perc.under = 1200)

nrow(SMOTEdata)
prop.table(table(micDt$country_destination))
prop.table(table(SMOTEdata$country_destination))

# Training and Test Data split using the Caret package
set.seed(123)
index1 <-createDataPartition(y=SMOTEdata$country_destination, p=.8, list=FALSE, times = 1)
Train.data <- SMOTEdata[index1,]
Test.data <- SMOTEdata[-index1,]

# CHECK THE COUNT OF COLUMNS
nrow(Train.data)
nrow(Test.data)

summary(Test.data)

#Below code did not upsample the sparse data
#library(dplyr)
#balancedData<- micDt%>%
#  group_by(country_destination) %>%
#  sample_frac(0.1)%>%
# as.data.frame
    
#Using the ROSE package also did not balance the sample dataset
#install.packages("ROSE")
#library(ROSE)
#set.seed(123)
#blData<- ROSE(country_destination ~ ., data = micDt)$data
#prop.table(table(balancedData$country_destination))


#the dummyVars object is used with predict function to create new data frame of dummy variables
#excluding the response factor default (column 17)
dummy.train <- data.frame(predict(dummyVars("~ .", data=Train.data[-11], fullRank=TRUE), newdata=Train.data))
dummy.test <- data.frame(predict(dummyVars("~ .", data=Test.data[-11], fullRank=TRUE), newdata=Test.data))


#add the response factor to the dummy variable training and test sets 
nrow(Train.data)
dummy.train<-cbind(country_destination=Train.data$country_destination,dummy.train)
dummy.test<-cbind(country_destination=Test.data$country_destination,dummy.test)

names(dummy.train) # view the columns in the training data


#Could be used to assign numerical values to the different factors.
#dummy.train[,2] <- ifelse(dummy.train[,2] == "AU", 1, ifelse(dummy.train[,2] == "CA", 2, ifelse(dummy.train[,2] == "DE", 3, ifelse(dummy.train[,2] == "ES", 4, ifelse(dummy.train[,2] == "FR", 5, ifelse(dummy.train[,2] == "GB", 6, ifelse(dummy.train[,2] == "IT", 7, ifelse(dummy.train[,2] == "NDF", 8, ifelse(dummy.train[,2] == "NL", 9, ifelse(dummy.train[,2] == "other", 10, ifelse(dummy.train[,2] == "PT", 11, ifelse(dummy.train[,2] == "US", 12,-1 ))))))))))))



#some parameters to control the sampling during parameter tuning and testing
#ctrl1 <- trainControl(method="repeatedcv", repeats=3,
 #                    classProbs=TRUE,summaryFunction = multiClassSummary, verboseIter = TRUE)
                     

ctrl2 <- trainControl(method="cv",classProbs=TRUE, summaryFunction = multiClassSummary, verboseIter = TRUE)

####--------DECISION TREE----------------
#to see what parameters are to be tuned:
modelLookup("rpart")
m.rpart <- train(country_destination ~ dac_year   + dac_month  + dac_day  + tfa_year + tfa_month   +    tfa_day  + ageCategory+ gender.FEMALE + gender.OTHER, 
           trControl = ctrl2,
           metric = "Accuracy", #using AUC to find best performing parameters
           preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
           data = dummy.train, 
           method = "rpart")

m.rpart <- train(country_destination ~ dac_year   + dac_month  + dac_day  + tfa_year + tfa_month   +    tfa_day  + ageCategory+ gender.FEMALE + gender.OTHER, 
                 trControl = ctrl2,
                 metric = "Accuracy", #using AUC to find best performing parameters
                 preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                 data = dummy.train, 
                 method = "rpart")
m.rpart

p.rpart <- predict(m.rpart,dummy.test)
p.rpart
#Obtaining the COnfusion Matrix usng Decision Tree model
confusionMatrix(p.rpart,dummy.test$country_destination)
length(SMOTEdata$country_destination)
plot(m.rpart)

####--------NEAURAL NETWORK----------------
modelLookup("nnet")
m.nnet <- train(country_destination ~ dac_year   + dac_month  + dac_day  + tfa_year + tfa_month   +    tfa_day  + ageCategory+ gender.FEMALE + gender.OTHER, 
                trControl = ctrl2,
                metric = "Accuracy", #using AUC to find best performing parameters
                preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
                data = dummy.train, 
                method = "nnet")
m.nnet
plot(m.nnet)
p.nnet <- predict(m.nnet,dummy.test)
confusionMatrix(p.nnet,dummy.test$country_destination)
 
##-----------------SVM MODEL-------------------
modelLookup("svmRadial")
m.svm <- train(country_destination ~ dac_year + dac_month + dac_day  + tfa_year + tfa_month + tfa_day  + ageCategory+ gender.FEMALE + gender.OTHER, 
               trControl = ctrl2,
               metric = "Accuracy", #using AUC to find best performing parameters
               preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
               data = dummy.train, 
               method = "svmRadial")
m.svm
plot(m.svm)
p.svm<- predict(m.svm,dummy.test) 
confusionMatrix(p.svm,dummy.test$country_destination)



##-----------------BAGGING ------------------
#some meta-learning examples
##BAGGING - bootstrapping is used to create many training sets and simple models are trained on each and combined
##many small decision trees
install.packages("ipred")
library(ipred)

m.bag <- train(country_destination  ~ dac_year + dac_month + dac_day  + tfa_year + tfa_month + tfa_day  + ageCategory+ gender.FEMALE + gender.OTHER , 
               trControl = ctrl2,
               metric = "Accuracy", #using AUC to find best performing parameters
               preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
               data = dummy.train, 
               method = "treebag")
m.bag
p.bag<- predict(m.bag,dummy.test)
p.bag
confusionMatrix(p.bag,dummy.test$country_destination)



library(randomForest)
m.rf <- train(country_destination ~ dac_year + dac_month + dac_day  + tfa_year + tfa_month + tfa_day  + ageCategory+ gender.FEMALE + gender.OTHER , 
              trControl = ctrl2,
              metric = "Accuracy", #using AUC to find best performing parameters
              preProc = c("range", "nzv"), #scale from 0 to 1 and from columns with zero variance
              data = dummy.train, 
              method = c("rf") )
m.rf
p.rf<- predict(m.rf,dummy.test)
confusionMatrix(p.rf,dummy.test$country_destination)

library(pROC)
rValues <- resamples(list(rpart=m.rpart, nnet=m.nnet, svm=m.svm, bag=m.bag, rf=m.rf))

#Below code generates the Boxplot for the different model evaluation parameters
bwplot(rValues, metric="Accuracy")
bwplot(rValues, metric = "Mean_Sensitivity")
bwplot(rValues, metric = "Mean_ROC")
bwplot(rValues, metric = "Mean_Specificity")
