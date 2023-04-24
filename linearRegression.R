
############converting categorical to numeric
regressionData<-data
regressionData$source_city_factor<-as.numeric(factor(regressionData$source_city))
regressionData$airline_factor<-as.numeric(factor(regressionData$airline))
regressionData$destination_city_factor<-as.numeric(factor(regressionData$destination_city))
regressionData$class_factor<-as.numeric(factor(regressionData$class))
regressionData$arrival_time_factor<-as.numeric(factor(regressionData$arrival_time))
regressionData$departure_time_factor<-as.numeric(factor(regressionData$departure_time))


########splitting the data
install.packages("party")
library(party)
ind<-sample(2,nrow(regressionData),prob=c(0.7,0.3),replace=TRUE)
train.regressionData<-regressionData[ind==1,]
test.regressionData<-regressionData[ind==2,]


##########model
##model<-lm(regressionData$price~ regressionData$airline_factor + regressionData$, data= regressionData )




##############
rm(train.regressionData)
rm(test.regressionData)
rm(regressionData)