setwd("C:/Users/KJohn/OneDrive/Desktop/Northwestern/MSDS 454 Advanced Modeling Techniques/Assignment 3")

spam.df <- readRDS('spam_base_v2.RData')
spam.df <- data.frame(spam.df)
str(spam.df)

# Split data and drop non-modeling variables;
drop.list  <- c('u','train','test');
spam.train <- subset(spam.df,train==1)[, !(names(spam.df) %in% drop.list)];
spam.test  <- subset(spam.df,test==1)[, !(names(spam.df) %in% drop.list)];

dim(spam.train)
dim(spam.test)
names(spam.train)


##########################
##Model 2: Random Forest##
##########################

library(randomForest)
library(pROC)
library(caret)

model.2 <- randomForest(as.factor(spam) ~., data=spam.train)
varImpPlot(model.2, sort=TRUE, n.var=10, pch=15, main="Random Forest")

model.2$confusion
roc.1 <- roc(response=spam.train$spam, predictor=as.numeric(model.2$predicted))
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))


model.2.test <- predict(model.2, newdata=spam.test)
confusionMatrix(factor(model.2.test), factor(spam.test$spam))
roc.2 <- roc(response=spam.test$spam, predictor=as.numeric(model.2.test))
print(roc.2)
