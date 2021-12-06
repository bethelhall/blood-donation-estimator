# Loading data and changing column names to more something more easily addresable
data <- read.table(file.choose(), header = TRUE, sep = ",")
colnames(data) <- c("Recency", "Frequency", "Monetary", "Time", "donatedMar07")

# Including library to make splitting the test and training data easier
library(caTools)

# Splitting test and training data
spl <- sample.split(data$donatedMar07, SplitRatio = 0.8)
train <- subset(data, spl == TRUE)
test <- subset(data, spl == FALSE)

# Creating models
model <- glm(donatedMar07 ~ ., data = train, family = binomial(link = "logit"))

# Using models to make predictions
train$predict <- predict(model, newdata = train, type = "response")
test$predict <- predict(model, newdata = test, type = "response")

# Evaluating prediction models with confusion matrix
ctab.test <- table(predict = test$predict>0.5, donatedMar07=test$donatedMar07)
ctab.test

# Model summaries
summary(model)

# Pseudo R-squared
df.null <- dim(train) [[1]] - 1
df.model <- dim(train) [[1]] - length(model$coefficients)
df.null
df.model
loglikelihood <- function(y, py) {sum(y * log(py) + (1-y)*log(1-py))}
pnull <- mean(as.numeric(train$donatedMar07))
null.dev <- -2*loglikelihood(as.numeric(train$donatedMar07), pnull)
pred <- predict(model, newdata = train, type = "response")
resid.dev <- -2*loglikelihood(as.numeric(train$donatedMar07), pred)
delDev <- null.dev - resid.dev
deldf <- df.null - df.model
p <- pchisq(delDev, deldf, lower.tail = F)
p
pr2 <- 1 - (resid.dev/null.dev)
print(pr2)