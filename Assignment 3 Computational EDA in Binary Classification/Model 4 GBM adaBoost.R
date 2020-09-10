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


##############################
##Model 4: GBM with adaBoost##
##############################

library(gbm)
library(pROC)

model.4 <- gbm(spam ~., data=spam.train, distribution='adaboost')
gbm.score.2 <- exp(model.4$fit)/(1+exp(model.4$fit))

roc.2 <- roc(response=spam.train$spam, predictor=gbm.score.2)
print(roc.2)
plot(roc.2)
text(0.6,0.6,paste('AUC=',round(auc(roc.2),3),sep=''))

roc.specs.2 <- coords(roc=roc.2,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)
gbm.class.2 <- ifelse(gbm.score.2>roc.specs.2$threshold,1,0);

# Let's create a proper confusion matrix
t <- table(spam.train$spam,gbm.class.2);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r


##Variable Importance##

inf.4 <- summary(model.4, 10)
inf.df4 <- data.frame(inf.4)
inf.ft4 <- head(inf.4,10)
formattable(inf.ft4)


##Test Model##

model.4.test <- predict(model.4, newdata=spam.test, n.trees=100, single.tree=FALSE)
gbm.score.2 <- exp(model.4.test)/(1+exp(model.4.test))

roc.2 <- roc(response=spam.test$spam, predictor=gbm.score.2)
print(roc.2)
plot(roc.2)
text(0.6,0.6,paste('AUC=',round(auc(roc.2),3),sep=''))

roc.specs.2 <- coords(roc=roc.2,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)

gbm.class.2 <- ifelse(gbm.score.2>roc.specs.2$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(spam.test$spam,gbm.class.2);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

