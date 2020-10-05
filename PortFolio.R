setwd("C:/Users/Deepak/OneDrive/Desktop/CSUGlobal/MIS581 Capstone BI and DA/winequality")

Whitewine <- read.csv("WinequalityFormatted.csv", header = TRUE)

summary(Whitewine) #Shows summary of complete data set.

set.seed(10)
library(Hmisc)
library(caret)
library(corrplot)
library(randomForest)
#describe(Whitewine)
#dim(Whitewine) #Shows dimesion of dataset
Whitewine[1:20,1:5]  # shows 1 to 20th rows and 1th to 5th columns of data frame
str(Whitewine)
table(Whitewine$quality)
barplot(table(Whitewine$quality))

#Linear Regression line

par(mfrow = c(4,3))
for (i in c(1:11)) {
  plot(Whitewine[, i], jitter(Whitewine[, "quality"]), xlab = names(Whitewine)[i],
       ylab = "quality", col = "firebrick", cex = 0.8, cex.lab = 1.3)
  abline(lm(Whitewine[, "quality"] ~ Whitewine[ ,i]), lty = 2, lwd = 2)
}

#Correlation matrix

par(mfrow = c(1, 1))
cor.Whitewine <- cor(Whitewine)
corrplot(cor.Whitewine, method = 'number')

#classify all wines into bad, average, or good, depending on whether their quality -
#is less than, equal to, or greater than 6 respectively

Whitewine$taste <- ifelse(Whitewine$quality < 6, 'Bad', 'Good')
Whitewine$taste[Whitewine$quality == 6] <- 'Average'
Whitewine$taste <- as.factor(Whitewine$taste)
table(Whitewine$taste)

# Prepare training(60%) and testing(40%) dataset. 

set.seed(120)
rf <- sample(nrow(Whitewine), 0.6 * nrow(Whitewine))
train <- Whitewine[rf, ]
test <- Whitewine[-rf, ]

#Building the model.

model <- randomForest(taste ~ . - quality, data = train)
model
pred <- predict(model, newdata = test)
table(pred, test$taste)

#Testing the accuracy as follows:

(658 + 455 + 247) / nrow(test)
