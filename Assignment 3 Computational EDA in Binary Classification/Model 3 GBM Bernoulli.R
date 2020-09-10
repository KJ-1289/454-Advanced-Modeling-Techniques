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


############################################
##Model 3: GBM with Bernoulli Distribution##
############################################

library(gbm)
library(pROC)

model.3 <- gbm(spam ~., data=spam.train, distribution='bernoulli')

gbm.score.1 <- exp(model.3$fit)/(1+exp(model.3$fit))

roc.1 <- roc(response=spam.train$spam, predictor=gbm.score.1)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

roc.specs.1 <- coords(roc=roc.1,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)

gbm.class.1 <- ifelse(gbm.score.1>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(spam.train$spam,gbm.class.1);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

# Is GBM returning a least squares fit?
y.hat.1 <- as.numeric(model.3$fit>0);
t <- table(spam.train$spam,y.hat.1);
r <- apply(t,MARGIN=1,FUN=sum);
t/r

##Variable Importance Plot##
library(formattable)
inf.1 <- summary(model.3)
inf.df <- data.frame(inf.1)
inf.df1 <- head(inf.df, 10)
formattable(inf.df1)

##Test Model##

model.3.test <- predict(model.3, newdata=spam.test, n.trees=100, single.tree=FALSE)
gbm.score.2 <- exp(model.3.test)/(1+exp(model.3.test))

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

