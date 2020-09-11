setwd("C:/Users/KJohn/OneDrive/Desktop/Northwestern/MSDS 454 Advanced Modeling Techniques/Assignment 5")
options(scipen=999)

spam.df <- data.frame(readRDS('spam_base_v2.RData'))
str(spam.df)

# Split data and drop non-modeling variables;
drop.list  <- c('u','train','test');
spam.train <- subset(spam.df,train==1)[, !(names(spam.df) %in% drop.list)];
spam.test  <- subset(spam.df,test==1)[, !(names(spam.df) %in% drop.list)];

dim(spam.train)
dim(spam.test)
names(spam.train)


########################################################################
##Computation EDA Model 1: Logistic Regression with Backward Selection##
########################################################################

library(formattable)
library(MASS)
library(pROC)
library(stargazer)

upper.lm <- glm(spam ~ .,data=spam.train)
backward.glm <- stepAIC(object=upper.lm,direction=c('backward'))
stargazer(backward.glm, type="html", out = "BackwardLogistic.doc",
          title="Logistic Regression with Backward Selection")

roc.1 <- roc(response=spam.train$spam, predictor=backward.glm$fitted.values)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

roc.specs.1 <- coords(roc=roc.1,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)

lr.class <- ifelse(backward.lm$fitted.values>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(spam.train$spam,lr.class);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

Metric <- c("Accuracy", "Precision", "Recall", "Specificity", "F1")
Score <- c(0.911, 0.910, 0.911, 0.910, 0.910)
metrics.df <- data.frame(Metric, Score)
formattable(metrics.df)

###########################################
##Computation EDA Model 2: One RUle Model##
###########################################

library(OneR)
data <- optbin(spam.train)
oner.model <- OneR(data, verbose=TRUE)
summary(oner.model)
predict <- predict(oner.model, spam.train)
eval_model(predict, spam.train)

prediction <- as.numeric(predict)
roc.1 <- roc(response=spam.train$spam, predictor=prediction)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

Metric <- c("Accuracy", "Precision", "Recall", "Specificity", "F1")
Score <- c(0.822, 0.780, 0.896, 0.748, 0.834)
metrics.df <- data.frame(Metric, Score)
formattable(metrics.df)

####################################################################
##Computation EDA Model 3: XGBoost with max_depth=4 and nrounds=10##
####################################################################

library(xgboost)

train.matrix <- as.matrix(spam.train[,-58]);

xgboost.eda <- xgboost(data=train.matrix, label=spam.train$spam, max_depth=4,
                   nrounds=10, objective='binary:logistic');

# Plot variable importance;
importance.10 <- xgb.importance(feature_names=colnames(train.matrix),model=xgboost.eda)
xgb.plot.importance(importance.10, rel_to_first=TRUE, xlab='Relative Importance',
                    main='XGBoost: max_depth=4 nrounds=10')

xg.10.score <- predict(xgboost.eda, newdata=train.matrix);
roc.xg10 <- roc(response=spam.train$spam, predictor=xg.10.score)
print(roc.xg10)

# Plot ROC Curve and add AUC to plot
plot(roc.xg10)
text(0.6,0.6,paste('AUC=',round(auc(roc.xg10),3),sep=''))

roc.specs.xg10 <- coords(roc=roc.xg10,x=c('best'),
                        input=c('threshold','specificity','sensitivity'),
                        ret=c('threshold','specificity','sensitivity'),
                        as.list=TRUE
)

xg.10.class <- ifelse(xg.10.score>roc.specs.xg10$threshold,1,0);

# Let's create a proper confusion matrix
t <- table(spam.train$spam,xg.10.class);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

Metric <- c("Accuracy", "Precision", "Recall", "Specificity", "F1")
Score <- c(0.950, 0.959, 0.939, 0.960, 0.949)
metrics.df <- data.frame(Metric, Score)
formattable(metrics.df)

