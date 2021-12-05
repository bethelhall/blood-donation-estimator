library(class)

data <- read.csv(file.choose(), header = TRUE)
data <- data[1:748, c(1:2,4:5)]

train <- data[1:598,]
test <- data[599:748,]
train_target <- data[1:598, 4]
test_target <- data[599:748, 4]

for(k in 17:37) {
    print(k)
    predicted_labels <- knn(train = train, test = test, cl = train_target, k)
    num_incorrect_labels <- sum(predicted_labels != test_target)
    misclassification_rate <- num_incorrect_labels / 150
    print(misclassification_rate)
}

# model <- knn(train = train, test = test, cl = train_target, k = best k value from for loop)
# table(test_target, model) to show confusion matrix
