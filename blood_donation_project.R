library(randomForest)
library(caret)
library(e1071)
library(party)
library(forecast)
library(dplyr)
library(plyr)
library(rpart)
library(rsq)

train.data <- read.csv("TrainData.csv")
test.data <- read.csv("TestData.csv")

# logestic model with the original training data:

log.model1 <- glm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                Number.of.Donations + 
                Months.since.First.Donation , data = train.data,family = "binomial")
summary(log.model1)

log.model3 <- glm(log(Made.Donation.in.March.2007) ~ log(Months.since.Last.Donation) +
                    log(Number.of.Donations) + 
                    log(Months.since.First.Donation) , data = train.data,family = "binomial")

# evaluate model fit in comparison with null model
modelChi <- log.model1$null.deviance - log.model1$deviance
chidf <- log.model1$df.null - log.model1$df.residual

# compute probability associated with observed chi square difference (p-value)
chisq.prob <- 1-pchisq(modelChi, chidf)

# R-squared value of Generalized Linear Models
rsq(model.log1)

# compute BIC
model1.BIC <- log.model1$deviance +
  2*length(log.model1$coefficients)*log(length(log.model1$fitted.values) )

#Computing the accuracy of the model by creating confusion matrix:

train.predict1 <- predict(log.model1,train.data , type = "response")
p1 <- ifelse(train.predict1 > 0.5,1,0)
tab1 <- table(predicted = p1 , actual = train.data$Made.Donation.in.March.2007)
tab1
confusionMatrix(tab1)


#Goodness of fit test:
with(log.model1 ,pchisq(null.deviance - deviance,df.null-df.residual,lower.tail = F))


# compute P(upgrade) given that extra cards are not purchased
P.upgrade.1 <- 1/(1+exp(-(model.log1$coefficients[1] +
                          model.log1$coefficients[2]*0)))
P.upgrade.1

# compute P(no upgrade) given that extra cards are NOT purchased
P.no.upgrade.1 <- 1-P.upgrade.1
P.no.upgrade.1


# compute odds of upgrade given that extra cards are NOT purchased
odds.1 <- P.upgrade.1/P.no.upgrade.1
odds.1

# compute P(upgrade) given that extra cards ARE purchased
P.upgrade.2 <- 1/(1+exp(-(model.log1$coefficients[1] +
                          model.log1$coefficients[2]*1)))
P.upgrade.2
# compute P(no upgrade) given that extra cards ARE purchased
P.no.upgrade.2 <- 1-P.upgrade.2
P.no.upgrade.2
# compute odds of upgrade given that extra cards ARE purchased
odds.2 <- P.upgrade.2/P.no.upgrade.2
odds.2
# compute odds ratio
OR.1 <- odds.2/odds.1
OR.1

plot(,train.predict1)

########################################################################################
# Logestic regression for training data removing the outliers:

summary(train.data)

col1 <- 14 + 1.5 * IQR(train.data$Months.since.Last.Donation)
col2 <- 7 + 1.5 * IQR(train.data$Number.of.Donations)
col3 <- 49 + 1.5 * IQR(train.data$Months.since.First.Donation)

train.data1 = subset(train.data,train.data$Months.since.Last.Donation < col1 )
train.data1 = subset(train.data1,train.data1$Number.of.Donations < col2) 
train.data1 = subset(train.data1,train.data1$Months.since.First.Donation < col3) 

summary(train.data1)


log.model2 <- glm(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                Number.of.Donations + 
                Months.since.First.Donation , data = train.data1 ,family = "binomial")
summary(log.model2)


# evaluate model fit in comparison with null model for model2.
modelChi2 <- log.model2$null.deviance - log.model2$deviance
chidf2 <- log.model2$df.null - log.model2$df.residual

# compute probability associated with observed chi square difference (p-value)
chisq.prob2 <- 1-pchisq(modelChi2, chidf2)

# R-squared value of Generalized Linear Models
rsq(log.model2)

# compute BIC
model2.BIC <- log.model2$deviance +
  2*length(log.model2$coefficients)*log(length(log.model2$fitted.values) )

#Computing the accuracy of the model by creating confusion matrix:

train.predict2 <- predict(log.model2,train.data , type = "response")
p2 <- ifelse(train.predict2 > 0.5,1,0)
tab2 <- table(predicted = p2 , actual = train.data$Made.Donation.in.March.2007)
tab2

confusionMatrix(tab2)

#Goodness of fit test:
with(log.model2 ,pchisq(null.deviance - deviance,df.null-df.residual,lower.tail = F))



### Predicting the test data and writing in a file:

fpredict <- predict(log.model2,test.data,type = "response")
dataa <- cbind(test.data$X ,fpredict)
write.csv(dataa,file = "LR2.CSV")


###########################################################################################


ctree_model1 <- rpart(Made.Donation.in.March.2007 ~ Months.since.Last.Donation +
                       Number.of.Donations + 
                       Months.since.First.Donation , data = train.data1)
summary(ctree_model1)

ctree_predict <- predict(ctree_model1,train.data)
head(ctree_predict)

p2 <- ifelse(ctree_predict > 0.5,1,0)

tab2 <- table(predicted = p2 , actual = train.data$Made.Donation.in.March.2007)
tab2
confusionMatrix(tab2)

tree_predict <- predict(ctree_model1, test.data)
dataa1 <- cbind(test.data$X,tree_predict)
write.csv(dataa1,file="tree.csv")




###################################################################################################
#evaluate the model fit:
modelChi <- model.log1$null.deviance - model.log1$deviance
chidf <- model.log1$df.null - model.log1$df.residual


chisq.prob <- 1-pchisq(modelChi, chidf)
chisq.prob

# compute AIC and BIC
card.AIC <- model.log1$deviance + 2*length(model.log1$coefficients)
card.BIC <- model.log1$deviance +
  2*length(model.log1$coefficients)*log(length(model.log1$fitted.values))

rsq(model.log1)
(model.log1$null.deviance - model.log1$deviance)/model.log1$null.deviance

