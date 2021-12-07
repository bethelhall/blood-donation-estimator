library(class)

data <- read.csv(file.choose(), header = TRUE)
colnames(data) <- c("Recency", "Frequency", "Monetary", "Time", "donatedMar07")

set.seed(1234)
n.points <- 748
sampling.rate <- 0.8
num.test.set.labels <- n.points * (1 - sampling.rate)
training <- sample(1:n.points, sampling.rate * n.points, replace = FALSE)
train <- subset(data[training, ], select = c(Recency, Frequency, Monetary, Time))
testing <- setdiff(1:n.points, training)
test <- subset(data[testing, ], select = c(Recency, Frequency, Monetary, Time))
train_target <- data$donatedMar07[training]
test_target <- data$donatedMar07[testing]

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