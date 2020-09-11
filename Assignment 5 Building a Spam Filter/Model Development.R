#################################
##Final Model 1: One Rule Model##
#################################

library(OneR)
data <- optbin(binned.df)
oner.model <- OneR(data, verbose=TRUE)
summary(oner.model)
prediction <- predict(oner.model, data)
eval_model(prediction, spam.train.top10)

prediction <- as.numeric(predict)
roc.1 <- roc(response=spam.train.top10$spam, predictor=prediction)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

Metric <- c("Accuracy", "Precision", "Recall", "Specificity", "F1")
Score <- c(0.819, 0.788, 0.872, 0.766, 0.828)
metrics.df <- data.frame(Metric, Score)
formattable(metrics.df)

###############################################
##Final Model 2: Standard Logistic Regression##
###############################################

std.glm <- glm(spam ~ .,data=spam.train.top10)
stargazer(std.glm, type="html", out = "StandardLogistic.doc",
          title="Standard Logistic Regresion")

roc.1 <- roc(response=spam.train.top10$spam, predictor=std.glm$fitted.values)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

roc.specs.1 <- coords(roc=roc.1,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)

lr.class <- ifelse(std.glm$fitted.values>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(spam.train$spam,lr.class);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

Metric <- c("Accuracy", "Precision", "Recall", "Specificity", "F1")
Score <- c(0.869, 0.886, 0.846, 0.891, 0.865)
metrics.df <- data.frame(Metric, Score)
formattable(metrics.df)

##################################################
##Final Model 3: Transformed Logistic Regression##
##################################################

bin.glm <- glm(spam ~ .,data=binned.df)
stargazer(bin.glm, type="html", out = "StandardLogistic.doc",
          title="Logistic Regresion with Transformed Variables")

roc.1 <- roc(response=binned.df$spam, predictor=bin.glm$fitted.values)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

roc.specs.1 <- coords(roc=roc.1,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)

lr.class <- ifelse(bin.glm$fitted.values>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(binned.df$spam,lr.class);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

Metric <- c("Accuracy", "Precision", "Recall", "Specificity", "F1")
Score <- c(0.918, 0.938, 0.894, 0.941, 0.916)
metrics.df <- data.frame(Metric, Score)
formattable(metrics.df)


##############################
##Final Model 4: Naive Bayes##
##############################

library(naivebayes)

predictor.df <- subset(binned.df, select = c("char_freq_xpoint.binned", "word_freq_remove.binned",
                                          "char_freq_dollar.binned", "word_freq_george.binned",
                                          "word_freq_hp.binned", "capital_run_length_longest.binned",
                                          "word_freq_free.binned", "word_freq_our.binned",
                                          "capital_run_length_total.binned", "word_freq_edu.binned"))
nb.model <- naive_bayes(x=predictor.df,y=factor(binned.df$spam), laplace=3)

predicted.spam <- predict(nb.model)
mean(predicted.spam==binned.df$spam)

predicted.num <- as.numeric(predicted.spam)
roc.1 <- roc(response=binned.df$spam, predictor=predicted.num)
print(roc.1)
plot(roc.1)
text(0.6,0.6,paste('AUC=',round(auc(roc.1),3),sep=''))

roc.specs.1 <- coords(roc=roc.1,x=c('best'),
                      input=c('threshold','specificity','sensitivity'),
                      ret=c('threshold','specificity','sensitivity'),
                      as.list=TRUE
)

nb.class <- ifelse(predicted.num>roc.specs.1$threshold,1,0);

# Let's create a proper confusion matrix
# table(actual,predicted)
t <- table(binned.df$spam,nb.class);
# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r

Metric <- c("Accuracy", "Precision", "Recall", "Specificity", "F1")
Score <- c(0.905, 0.871, 0.934, 0.879, 0.901)
metrics.df <- data.frame(Metric, Score)
formattable(metrics.df)
