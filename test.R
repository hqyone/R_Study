library(biomaRt)
library(caret)
library(caTools)

print(iris$Sepal.Length)
sample.split(iris,SplitRatio = 0.7)->s
subset(iris, s==TRUE)->train_data
subset(iris, s==FALSE)->test_data

lm(train_data$Sepal.Length~., train_data)->model1
summary(model1)
