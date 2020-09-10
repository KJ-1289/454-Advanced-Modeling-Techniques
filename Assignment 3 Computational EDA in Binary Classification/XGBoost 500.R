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


###################################
##Model 5: XGBoost 500 Iterations##
###################################

library(xgboost)

# Input to xgboost() must be a matrix;
# Need to drop the response variable from the data frame before we 
# convert it to a matrix.
train.matrix <- as.matrix(spam.train[,-58]);

model.5 <- xgboost(data=train.matrix, label=spam.train$spam, max_depth=4,
                   nrounds=500, objective='binary:logistic');

# Plot variable importance;
importance.500 <- xgb.importance(feature_names=colnames(train.matrix),model=model.5)
xgb.plot.importance(importance.500, rel_to_first=TRUE, xlab='Relative Importance',
                    main='XGBoost Model: 500 Iterations')

# Plot top 10 variables;
xgb.plot.importance(importance.10[1:10], rel_to_first=TRUE, xlab='Relative Importance',
                    main='XGBoost Model: 500 Iterations')

xg.500.score <- predict(model.5, newdata=train.matrix);
roc.500 <- roc(response=spam.train$spam, predictor=xg.500.score)
print(roc.500)

# Plot ROC Curve and add AUC to plot
plot(roc.500)
text(0.6,0.6,paste('AUC=',round(auc(roc.500),3),sep=''))



roc.specs.500 <- coords(roc=roc.500,x=c('best'),
                       input=c('threshold','specificity','sensitivity'),
                       ret=c('threshold','specificity','sensitivity'),
                       as.list=TRUE
)

xg.500.class <- ifelse(xg.500.score>roc.specs.500$threshold,1,0);

# Let's create a proper confusion matrix
t <- table(spam.train$spam,xg.500.class);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r
