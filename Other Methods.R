library(randomForest)
library(caret)
library(e1071)
library(ROSE)
library(party)
library(forecast)

#Reding the data

train.data <- read.csv("TrainData.csv")
test.data <- read.csv("TestData.csv")

train.data <- train.data %>% mutate(Made.Donation.in.March.2007 =
                                      ifelse(train.data$Made.Donation.in.March.2007==1,"Yes","No"))
train.data$Made.Donation.in.March.2007 <- as.factor(train.data$Made.Donation.in.March.2007)
head(train.data$Made.Donation.in.March.2007)

#dividing the train data into 2 datas:

nrow(train.data)
ind <-sample(2,nrow(train.data),replace = TRUE, prob =c(0.8,0.2))
training <- train.data[ind==1,]
testing <- train.data[ind==2,]
nrow(training)
nrow(testing)
head(training)
#str & summary
training$Made.Donation.in.March.2007 <- as.integer(training$Made.Donation.in.March.2007)
str(training)
summary(training)


#correlation graph
par(mfrow = c(2,2))
plot(training$Made.Donation.in.March.2007,training$Number.of.Donations)
plot(training$Made.Donation.in.March.2007,training$Months.since.Last.Donation)
plot(training$Made.Donation.in.March.2007,training$Total.Volume.Donated..c.c..)
plot(training$Made.Donation.in.March.2007,training$Months.since.First.Donation)
############################################################################################
####  Regression:

### Logistic:

lm_model <- glm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                Number.of.Donations + 
                Months.since.First.Donation , data = training)
summary(lm_model)

lm_predict <- predict(lm_model,testing)
head(lm_predict)
with(lm_model ,pchisq(null.deviance - deviance,df.null-df.residual,lower.tail = F))

tab1 <- table(predicted = lm_predict , actual = testing$Made.Donation.in.March.2007)
tab1
confusionMatrix(tab1)

nrow(lm_predict)



#################################################################################################
#####  Decision tree:

ctree_model <- ctree(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                       Number.of.Donations + 
                       Months.since.First.Donation , data = training)
ctree_model
plot(ctree_model)
ctree_predict <- predict(ctree_model,testing)
head(ctree_predict)
tail(ctree_predict)
tab2 <- table(predicted = ctree_predict , actual = testing$Made.Donation.in.March.2007)
tab2
confusionMatrix(tab2)

error_cal2 <- 1 - sum(diag(tab2))/sum(tab2)
error_cal2


ctree_final <- predict(ctree_model,test.data,type = "prob")
head(ctree_final)

#################################################################################################
###### Random forest:

rf_model <- randomForest(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                           Number.of.Donations + 
                           Months.since.First.Donation , data = training)

rf_predict <- predict(rf_model,testing)
head(rf_predict)

tab3 <- table(predicted = rf_predict , actual = testing$Made.Donation.in.March.2007)
tab3
confusionMatrix(tab3)

error_cal3 <- 1 - sum(diag(tab3))/sum(tab3)
error_cal3

rf_final <- predict(rf_model,test.data)
table(rf_final)
rf_data <- cbind(test.data,rf_final[,2])
write.csv(rf_data,file = "RF_Data.csv")

#changing the sample size
#############################################################################################
### over sampling:
over <- ovun.sample(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                      Number.of.Donations + 
                      Months.since.First.Donation , data = training, method = "over", N=702)$data
over_model <- randomForest(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                             Number.of.Donations + 
                             Months.since.First.Donation , data = over)

over_predict <- predict(over_model,testing)
head(over_predict)

tab4 <- table(predicted = over_predict , actual = testing$Made.Donation.in.March.2007)
tab4
confusionMatrix(tab4)

error_cal4 <- 1 - sum(diag(tab4))/sum(tab4)
error_cal4

###################################################################################################
### under sampling:
under <- ovun.sample(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                      Number.of.Donations + 
                      Months.since.First.Donation , data = training, method = "under", N=218)$data
under_model <- randomForest(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                             Number.of.Donations + 
                             Months.since.First.Donation , data = under)

under_predict <- predict(under_model,testing)
head(under_predict)

tab5 <- table(predicted = under_predict , actual = testing$Made.Donation.in.March.2007)
tab5
confusionMatrix(tab5)
error_cal5 <- 1 - sum(diag(tab5))/sum(tab5)
error_cal5


#################################################################################################
### both sampling:
both <- ovun.sample(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                      Number.of.Donations + 
                      Months.since.First.Donation , data = training, method = "both", N=920)$data
both_model <- randomForest(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                             Number.of.Donations + 
                             Months.since.First.Donation , data = both)

both_predict <- predict(both_model,testing)
head(both_predict)

tab6 <- table(predicted = both_predict , actual = testing$Made.Donation.in.March.2007)
tab6
confusionMatrix(tab6)

error_cal6 <- 1 - sum(diag(tab6))/sum(tab6)
error_cal6

####################################################################################################
###Synthatic data
rose <- ROSE(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
               Number.of.Donations + 
               Months.since.First.Donation , data = training,
             N=500 )$data
rose_model <- randomForest(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                             Number.of.Donations + 
                             Months.since.First.Donation , data = rose)

rose_predict <- predict(rose_model,testing)
head(rose_predict)

tab7 <- table(predicted = rose_predict , actual = testing$Made.Donation.in.March.2007)
tab7
confusionMatrix(tab7)
error_cal7 <- 1 - sum(diag(tab7))/sum(tab7)
error_cal7




