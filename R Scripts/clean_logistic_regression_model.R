# Loading data and changing column names to more something more easily addresable
data <- read.table(file.choose(), header = TRUE, sep = ",")
colnames(data) <- c("Recency", "Frequency", "Monetary", "Time", "donatedMar07")

# Including library to make splitting the test and training data easier
library(caTools)

# Making new dataset without Monetary attribute to make model without that attribute
noblooddata <- subset(data, select = -c(Monetary))

# Splitting test and training data
nobloodspl <- sample.split(noblooddata$donatedMar07, SplitRatio = 0.8)
nobloodtrain <- subset(noblooddata, nobloodspl == TRUE)
nobloodtest <- subset(noblooddata, nobloodspl == FALSE)

# Creating models
nobloodmodel <- glm(donatedMar07 ~ ., data = nobloodtrain, family = binomial(link = "logit"))

# Using models to make predictions
nobloodtrain$predict <- predict(nobloodmodel, newdata = nobloodtrain, type = "response")
nobloodtest$predict <- predict(nobloodmodel, newdata = nobloodtest, type = "response")
                                                                                                                              
# Evaluating prediction models with confusion matrix
ctab.nobloodtest <- table(predict = nobloodtest$predict>0.5, donatedMar07=nobloodtest$donatedMar07)
ctab.nobloodtest

# Model summaries
summary(nobloodmodel)

# Pseudo R-squared
df.null <- dim(nobloodtrain) [[1]] - 1
df.model <- dim(nobloodtrain) [[1]] - length(nobloodmodel$coefficients)
df.null
df.model
loglikelihood <- function(y, py) {sum(y * log(py) + (1-y)*log(1-py))}
pnull <- mean(as.numeric(nobloodtrain$donatedMar07))
null.dev <- -2*loglikelihood(as.numeric(nobloodtrain$donatedMar07), pnull)
pred <- predict(nobloodmodel, newdata = nobloodtrain, type = "response")
resid.dev <- -2*loglikelihood(as.numeric(nobloodtrain$donatedMar07), pred)
delDev <- null.dev - resid.dev
deldf <- df.null - df.model
p <- pchisq(delDev, deldf, lower.tail = F)
p
pr2 <- 1 - (resid.dev/null.dev)
print(pr2)