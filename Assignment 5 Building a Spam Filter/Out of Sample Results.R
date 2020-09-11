########################
##Out-of-Sample Models##
########################

##Backward Selection Logistic Regression

bwlr.test <- predict(backward.glm, newdata=spam.test)

roc.1 <- roc(response=spam.test$spam, predictor=bwlr.test)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

lr.class <- ifelse(bwlr.test>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(spam.test$spam,lr.class);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r


##One Rule Model

oner.test <- predict(oner.model, newdata=spam.test)
eval_model(oner.test, spam.test)

oner.testn <- as.numeric(oner.test)
roc.1 <- roc(response=spam.test$spam, predictor=oner.test)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))


##XG Boost

test.matrix <- as.matrix(spam.test[,-58])
xgb.test <- predict(xgboost.eda, newdata=test.matrix)

roc.1 <- roc(response=spam.test$spam, predictor=xgb.test)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

xgb.class <- ifelse(xgb.test>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(spam.test$spam,xgb.class)
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r


##Logistic Regression Top 10 Variables

tenlr.test <- predict(std.glm, newdata=spam.test)

roc.1 <- roc(response=spam.test$spam, predictor=tenlr.test)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

lr.class <- ifelse(tenlr.test>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(spam.test$spam,lr.class);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r
