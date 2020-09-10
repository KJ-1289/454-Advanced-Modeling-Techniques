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
##Model 1: Decision Tree##
##########################

library(rpart)
library(rattle)
library(lattice)
par(mar=c(1,1,1,1))
model.1 <- rpart(as.factor(spam) ~., data=spam.train)
fancyRpartPlot(model.1)

# Variable importance plot
x <- head(model.1$variable.importance, 10)
dotplot(x,xlab='Variable Importance', main='Decision Tree',pch=15)

##Confusion Matrix##
model.1.train <- predict(model.1, newdata=spam.train, type=c("vector"))
model.1.train <- ifelse(model.1.train==1, 0, 1)

roc.1 <- roc(response=spam.train$spam, predictor=model.1.train)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

roc.specs.1 <- coords(roc=roc.1,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)

gbm.class.1 <- ifelse(model.1.train>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(spam.train$spam,model.1.train);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

##Test Set##
model.1.test <- predict(model.1, newdata=spam.test, type=c("vector"))
model.1.test <- ifelse(model.1.test==1, 0, 1)

roc.1 <- roc(response=spam.test$spam, predictor=model.1.test)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

roc.specs.1 <- coords(roc=roc.1,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)

gbm.class.1 <- ifelse(model.1.test>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(spam.test$spam,model.1.test);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

