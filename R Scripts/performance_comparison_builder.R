# This script is supposed to be run to compare the performances of multiple models
# You must execute this script after every script and you have stored
# the p values given by the performance metrics in those scripts into a variable called Pvalue
# This script also assumes that you have created a variable named Name and put an appropriate name for the model

Accuracy <- (cm[1,1] + cm[2,2]) / sum(cm)
Precision <- cm[2,2] / sum(cm[2,])
Recall <- cm[2,2] / sum(cm[,2])
Sensitivity <- Recall
Specificity <- cm[1,1] / sum(cm[1,])
if (exists('models.df') && is.data.frame(get('models.df'))) {
  df <- data.frame(Name, Accuracy, Precision, Recall, Sensitivity, Specificity, Pvalue)
  models.df <- rbind(models.df, df)
} else {
  models.df <- data.frame(Name, Accuracy, Precision, Recall, Sensitivity, Specificity, Pvalue)
}
