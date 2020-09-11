####################
##In-Sample Scores##
####################

Metric <- c("Accuracy", "Precision", "Recall", "Specificity", "F1", "AUC")
LR.Backward <- c(0.911, 0.910, 0.911, 0.910, 0.910, 0.956)
One.Rule <- c(0.822, 0.780, 0.896, 0.748, 0.834, 0.732)
XG.Boost <- c(0.950, 0.959, 0.939, 0.960, 0.949, 0.987)
LR.Top.10 <- c(0.869, 0.886, 0.846, 0.891, 0.865, 0.936)
metrics.df <- data.frame(Metric, LR.Full, One.Rule, XG.Boost, LR.Top.10)
formattable(metrics.df)

########################
##Out-of-Sample Scores##
########################

lr.full.test <- predict()
