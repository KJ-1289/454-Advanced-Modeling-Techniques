setwd("C:/Users/KJohn/OneDrive/Desktop/Northwestern/MSDS 454 Advanced Modeling Techniques/Assignment 3")

library(formattable)

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


####################################
##Model 6: XGBoost 1000 Iterations##
####################################

library(xgboost)

# Input to xgboost() must be a matrix;
# Need to drop the response variable from the data frame before we 
# convert it to a matrix.
train.matrix <- as.matrix(spam.train[,-58]);

model.6 <- xgboost(data=train.matrix, label=spam.train$spam, max_depth=4,
                   nrounds=1000, objective='binary:logistic');

# Plot variable importance;
importance.1000 <- xgb.importance(feature_names=colnames(train.matrix),model=model.5)
xgb.plot.importance(importance.1000, rel_to_first=TRUE, xlab='Relative Importance',
                    main='XGBoost Model')

# Plot top 10 variables;
xgb.plot.importance(importance.1000[1:10], rel_to_first=TRUE, xlab='Relative Importance',
                    main='XGBoost Model: 1000 Iterations')

xg.1000.score <- predict(model.6, newdata=train.matrix);
roc.1000 <- roc(response=spam.train$spam, predictor=xg.1000.score)
print(roc.1000)

# Plot ROC Curve and add AUC to plot
plot(roc.1000)
text(0.6,0.6,paste('AUC=',round(auc(roc.1000),3),sep=''))



roc.specs.1000 <- coords(roc=roc.1000,x=c('best'),
                        input=c('threshold','specificity','sensitivity'),
                        ret=c('threshold','specificity','sensitivity'),
                        as.list=TRUE
)

xg.1000.class <- ifelse(xg.1000.score>roc.specs.1000$threshold,1,0);

# Let's create a proper confusion matrix
t <- table(spam.train$spam,xg.1000.class);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r


##Out of Sample Results
test.matrix <- as.matrix(spam.test[,-58]);

xg.500.predict <- predict(model.5,newdata=test.matrix);
xg.1000.predict <- predict(model.6,newdata=test.matrix);

# Out-Of-Sample Accuracy for xg.500;
xg.500.pclass <- ifelse(xg.500.predict>roc.specs.500$threshold,1,0);

# Let's create a proper confusion matrix
t <- table(spam.test$spam,xg.500.pclass);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r


# Out-Of-Sample Accuracy for xg.1000;
xg.1000.pclass <- ifelse(xg.1000.predict>roc.specs.1000$threshold,1,0);

# Let's create a proper confusion matrix
t <- table(spam.test$spam,xg.1000.pclass);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

#AUC
roc.500 <- roc(response=spam.test$spam, predictor=xg.500.predict)
round(auc(roc.500),3)
roc.1000 <- roc(response=spam.test$spam, predictor=xg.1000.predict)
round(auc(roc.1000),3)
