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

for(k in 17:37) {
    print(k)
    predicted_labels <- knn(train = train, test = test, cl = train_target, k)
    num_incorrect_labels <- sum(predicted_labels != test_target)
    misclassification_rate <- num_incorrect_labels / 150
    print(misclassification_rate)
}