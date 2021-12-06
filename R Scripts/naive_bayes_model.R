# Library for splitting data
library(caTools)

# Uses library with Naive Bayes algorithm
library(e1071)

# Loading data and changing column names to more something more easily addressable
data <- read.table(file.choose(), header = TRUE, sep = ",")
colnames(data) <- c("Recency", "Frequency", "Monetary", "Time", "donatedMar07")

# Cleaning and normalizing data and then splitting into test and training subsets
normalize <- function(x) {return((x - min(x)) / (max(x) - min(x))) }
data <- subset(data, select = -c(Monetary))
data_n <- as.data.frame(lapply(data[,c(1,2,3,4)], normalize))

# Splitting test and training data
set.seed(1234)
spl <- sample.split(data_n$donatedMar07, SplitRatio = 0.8)
train <- subset(data_n, spl == TRUE)
test <- subset(data_n, spl == FALSE)
attach(data_n)

# Creating model
nb_default <- naiveBayes(donatedMar07 ~ ., data = train)
nb_default

# Using model to make predictions
pred <- predict(nb_default, newdata = test)

# Evaluating prediction models with confusion matrix from caret library
library(caret)
cm <- table(predict = pred, donatedMar07 = test$donatedMar07 )
confusionMatrix(cm)