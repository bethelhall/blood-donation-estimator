# Loading data and changing column names to more something more easily addresable
data <- read.table(file.choose(), header = TRUE, sep = ",")
colnames(data) <- c("Recency", "Frequency", "Monetary", "Time", "donatedMar07")

# Including library to make splitting the test and training data easier
library(caTools)

# Making new dataset without Monetary attribute to make model without that attribute
noblooddata <- subset(data, select = -c(Monetary))

# Function to normalize data
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
normmodel <- glm(donatedMar07 ~ ., data = train, family = binomial(link = "logit"))

# Using model to make prediction
train$predict <- predict(normmodel, newdata = train, type = "response")
test$predict <- predict(normmodel, newdata = test, type = "response")

# Evaluating prediction models with confusion matrix
cm <- table(predict = test$predict>0.5, donatedMar07 = test$donatedMar07)
cm

summary(normmodel)

# Pseudo-R sqaured of model
df.null <- dim(train) [[1]] - 1
df.model <- dim(train) [[1]] - length(normmodel$coefficients)
df.null
df.model
loglikelihood <- function(y, py) {sum(y * log(py) + (1-y)*log(1-py))}
pnull <- mean(as.numeric(train$donatedMar07))
null.dev <- -2*loglikelihood(as.numeric(train$donatedMar07), pnull)
pred <- predict(normmodel, newdata = train, type = "response")
resid.dev <- -2*loglikelihood(as.numeric(train$donatedMar07), pred)
delDev <- null.dev - resid.dev
deldf <- df.null - df.model
p <- pchisq(delDev, deldf, lower.tail = F)
p
pr2 <- 1 - (resid.dev/null.dev)
print(pr2)
detach(data_n)