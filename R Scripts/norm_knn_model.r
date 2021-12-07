library(class)
library(caret)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) }

data <- read.csv(file.choose(), header = TRUE)
data <- data[1:748, c(1:2,4:5)]
data_n <- as.data.frame(lapply(data[,c(1,2,3,4)], normalize))
colnames(data_n) <- c("Recency", "Frequency", "Time", "donatedMar07")

set.seed(1234)
n.points <- 748
sampling.rate <- 0.8
num.test.set.labels <- n.points * (1 - sampling.rate)
training <- sample(1:n.points, sampling.rate * n.points, replace = FALSE)
train <- subset(data_n[training, ], select = c(Recency, Frequency, Time))
testing <- setdiff(1:n.points, training)
test <- subset(data_n[testing, ], select = c(Recency, Frequency, Time))
train_target <- data_n$donatedMar07[training]
test_target <- data_n$donatedMar07[testing]

minmissclassify <- 1
finalk <- 1

for(k in 1:20) {
  predicted_labels <- knn(train = train, test = test, cl = train_target, k)
  num_incorrect_labels <- sum(predicted_labels != test_target)
  misclassification_rate <- num_incorrect_labels / 150
  cat(k, "\t", misclassification_rate, "\n")
  if (misclassification_rate < minmissclassify) {
    minmissclassify <- misclassification_rate
    finalk <- k
  }
}
model <- knn(train = train, test = test, cl = train_target, k = finalk)
cm <- table(test_target, model)
confusionMatrix(cm)