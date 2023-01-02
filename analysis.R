data <- read.table("BDSA601_dataset.txt", header = TRUE)
data$Month <- factor(data$Month,
    levels = c("Feb", "Mar", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE
)

data$Operating_Systems <- factor(data$Operating_Systems)
data$Browser <- factor(data$Browser)
data$Region <- factor(data$Region)
data$Traffic_Type <- factor(data$Traffic_Type)
data$Visitor_Type <- factor(data$Visitor_Type)
data$Weekend <- factor(data$Weekend)
data$Shopping_Intention <- factor(data$Shopping_Intention)

attach(data)

install.packages("summarytools")
library(summarytools)
view(dfSummary(data))




par(mfrow = c(2, 5))
hist(Shopping_Pages, col = "lightgrey")
hist(Shopping_Duration, col = "lightgrey")
hist(Informational_Pages, col = "lightgrey")
hist(Informational_Duration, col = "lightgrey")
hist(Product_Related_Pages, col = "lightgrey")
hist(Product_Related_Duration, col = "lightgrey")
hist(Bounce_Rates, col = "lightgrey")
hist(Exit_Rates, col = "lightgrey")
hist(Page_Values, col = "lightgrey")
hist(Special_Day, col = "lightgrey")

library(corrplot)

correlation <- cor(data[, 1:10])

corrplot(correlation, method = "number", type = "lower", diag = TRUE)

# plot Bounce Rates versus Exit Rates
par(mfrow = c(1, 1))
plot(data$Bounce_Rates[data$Shopping_Intention == FALSE], data$Exit_Rates[data$Shopping_Intention == FALSE], xlab = "Bounce Rate", ylab = "Exit Rates", col = "black")
points(data$Bounce_Rates[data$Shopping_Intention == TRUE], data$Exit_Rates[data$Shopping_Intention == TRUE], col = "red")
legend("bottomright", c("Negative Class", "Positive Class"), text.col = 1:2)

# categorical
# table summary
table(Month)
table(Operating_Systems)
table(Browser)
table(Region)
table(Traffic_Type)
table(Visitor_Type)
table(Weekend)
table(Shopping_Intention)

# graph
par(mfrow = c(2, 4))
plot(Month, main = "Month", col = 1:10)
plot(Operating_Systems, main = "Operating_Systems", col = 1:08)
plot(Browser, main = "Browser", col = 1:13)
plot(Region, main = "Region", col = 1:09)
plot(Traffic_Type, main = "Traffic_Type", col = 1:20)
plot(Visitor_Type, main = "Visitor_Type", col = 1:03)
plot(Weekend, main = "Weekend", col = 1:02)
plot(Shopping_Intention, main = "Shopping_Intention", col = 1:02)

# investigate the predictor-response relationships: Month – Shopping Intention
table <- table(data$Month, data$Shopping_Intention)
label <- c("Feb", "Mar", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
rownames(table) <- label
colnames(table) <- c("FALSE", "TRUE")

plot(table[, 1],
    type = "b", lwd = 2, xaxt = "n", xlab = "Session Month", ylab = "Number of Visitors",
    ylim = range(table), main = "The trend of visitors’ shopping intention based on months"
)

axis(1, at = seq(length(label)), labels = label)

lines(table[, 2], col = 2, type = "b", lwd = 2)
grid()
legend("topright", bg = "lightgrey", c("Negative Class", "Positive Class"), text.col = 1:2)

# investigate the predictor-response relationships: Operating Systems – Shopping Intention
table <- table(data$Operating_Systems, data$Shopping_Intention)
colnames(table) <- c("Not Making Purchase", "Making Purchase")
barplot(t(table), col = 1:2, xlab = "Operating_Systems")

legend("topright", c("Not Making Purchase", "Making Purchase"), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2)

# investigate the predictor-response relationships: Browser – Shopping Intention
table <- table(data$Browser, data$Shopping_Intention)
colnames(table) <- c("Not Making Purchase", "Making Purchase")
barplot(t(table), col = 1:2, xlab = "Browser")

legend("topright", c("Not Making Purchase", "Making Purchase"), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2)

# investigate the predictor-response relationships: Region – Shopping Intention
table <- table(data$Region, data$Shopping_Intention)
colnames(table) <- c("Not Making Purchase", "Making Purchase")
barplot(t(table), col = 1:2, xlab = "Region")

legend("topright", c("Not Making Purchase", "Making Purchase"), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2)

# investigate the predictor-response relationships: Traffic Type – Shopping Intention
table <- table(data$Traffic_Type, data$Shopping_Intention)
colnames(table) <- c("Not Making Purchase", "Making Purchase")
barplot(t(table), col = 1:2, xlab = "Traffic_Type")

legend("topright", c("Not Making Purchase", "Making Purchase"), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2)


# investigate the predictor-response relationships: Weekend – Shopping Intention
table <- table(data$Weekend, data$Shopping_Intention)
colnames(table) <- c("Not Weekend", "Weekend")
rownames(table) <- c("Not Making Purchase", "Making Purchase")
barplot(table, col = 1:2)
legend("topright", c("Not Not Making Purchase", "Making Purchase"), bg = "lightgrey", pch = 15, col = 1:2, text.col = 1:2)


# investigate the predictor-response relationships: Visitor Type – Shopping Intention
table <- table(data$Visitor_Type, data$Shopping_Intention)
rownames(table) <- c("New_Visitor", "Other", "Returning_Visitor")
colnames(table) <- c("Not Making Purchase", "Making Purchase")
barplot(table, col = 1:3)
legend("topright", c("New_Visitor", "Other", "Returning_Visitor"), bg = "lightgrey", pch = 15, col = 1:3, text.col = 1:3)


# data splitting and modeling
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")

library("caret")
library("rpart")
library("rpart.plot")

# splitting data
set.seed(1)
n <- dim(data)[1]
split <- createDataPartition(1:n, p = 0.8, list = FALSE)
train <- data[split, ]
test <- data[-split, ]

n <- dim(train)[1]
N <- dim(test)[1]

# performance metrics

metrics <- function(CM) {
    Accuracy <- (CM[1, 1] + CM[2, 2]) / sum(CM)
    ErrorRate <- (CM[1, 2] + CM[2, 1]) / sum(CM)
    Specificity <- CM[1, 1] / sum(CM[, 1])
    Sensitivity <- CM[2, 2] / sum(CM[, 2])
    Recall <- Sensitivity
    Precision <- CM[2, 2] / (CM[2, 1] + CM[2, 2])
    F1score <- 2 * (Precision * Recall) / (Precision + Recall)

    cat(
        "Accuracy    = ", round(Accuracy, 3), "\n",
        "Error Rate  = ", round(ErrorRate, 3), "\n",
        "Specificity = ", round(Specificity, 3), "\n",
        "Sensitivity = ", round(Sensitivity, 3), "\n",
        "Recall      = ", round(Recall, 3), "\n",
        "Precision   = ", round(Precision, 3), "\n",
        "F1Score     = ", round(F1score, 3)
    )
}

# Construct a classification tree and represent it graphically.

set.seed(1)
TREE <- rpart(formula = Shopping_Intention ~ ., data = train, method = "class")
rpart.plot(TREE, box.palette = "RdYlGn", type = 5)

# Present the variable importance plot for the random forest constructed.
# Which predictors are two the most important variables?

data.frame(TREE$variable.importance)

par(mar = c(12, 4, 4, 4)) # increase margin size

barplot(TREE$variable.importance, las = 2)

# evaluate the performance of the constructed tree by estimating the test misclassification rate
prediction <- predict(object = TREE, newdata = test, type = "class")
CM <- table(prediction, test$Shopping_Intention)
CM
metrics(CM)

# Random Forest Classification Tree
# Construct a random forest.
install.packages("randomForest")
library(randomForest)
set.seed(1) # because the process involves random selection
TREE.forest <- randomForest(formula = Shopping_Intention ~ ., data = train, importance = TRUE)
# By default the randomForest() function uses  for classification.

# Present the variable importance plot for the random forest constructed.
# Which predictors are two the most important variables?

varImpPlot(TREE.forest)


# Evaluate the performance of the constructed tree by estimating the test misclassification rate.
prediction <- predict(object = TREE.forest, newdata = test, type = "class")
CM <- table(prediction, test$Shopping_Intention)
CM
metrics(CM)

# Gradient Boosting Classification Tree

# encoding categorical features
train$Shopping_Intention <- as.integer(train$Shopping_Intention)
train$Shopping_Intention <- train$Shopping_Intention - 1

test$Shopping_Intention <- as.integer(test$Shopping_Intention)
test$Shopping_Intention <- test$Shopping_Intention - 1

# Construct a classification tree using Gradient Boosting algorithm.
install.packages("gbm")
library(gbm)
set.seed(1) # because the process involves random selection
TREE.boost <- gbm(formula = Shopping_Intention ~ ., data = train, distribution = "bernoulli")

par(mar = c(4, 12, 4, 4))
summary(TREE.boost, method = relative.influence, las = 2)

# Evaluate the performance of the constructed tree by estimating the test misclassification rate.
probability <- predict(object = TREE.boost, newdata = test, type = "response")
prediction <- rep(0, N)
prediction[probability > 0.5] <- 1
CM <- table(prediction, test$Shopping_Intention)
CM
metrics(CM)