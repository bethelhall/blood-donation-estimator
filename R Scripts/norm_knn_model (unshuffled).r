library(class)

normalize <- function(x) {
return((x - min(x)) / (max(x) - min(x))) }

data <- read.csv(file.choose(), header = TRUE)
data <- data[1:748, c(1:2,4:5)]
data_n <- as.data.frame(lapply(data[,c(1,2,3,4)], normalize))

train <- data_n[1:598,]
test <- data_n[599:748,]
train_target <- data_n[1:598, 4]
test_target <- data_n[599:748, 4]

minmissclassify <- 1
finalk <- 1
for(k in 1:20) {
    print(k)
    predicted_labels <- knn(train = train, test = test, cl = train_target, k)
    num_incorrect_labels <- sum(predicted_labels != test_target)
    misclassification_rate <- num_incorrect_labels / 150
    print(misclassification_rate)
    cat(k, "\t", misclassification_rate, "\n")
    if (misclassification_rate < minmissclassify) {
      minmissclassify <- misclassification_rate
      finalk <- k
    }
}

model <- knn(train = train, test = test, cl = train_target, k = finalk)
cm <- table(test_target, model)
confusionMatrix(cm)