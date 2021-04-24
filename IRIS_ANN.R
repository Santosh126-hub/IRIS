#Install and load the packages

install.packages("nnet")
install.packages("neuralnet")
install.packages("caret")
install.packages("NeuralNetTools")
install.packages("ggplot2")
install.packages("GGally")
library(nnet)
library(neuralnet)
library(caret)
library("NeuralNetTools")
library("ggplot2")
library("GGally")
#Input the file

iris<-read.csv(file.choose())
str(iris)

#Exploratory Data Analysis

hist(iris$SepalLengthCm , prob = T, breaks = 30)
lines(density(iris$SepalLengthCm))

hist(iris$SepalWidthCm , prob = T, breaks = 30)
lines(density(iris$SepalWidthCm))

hist(iris$PetalLengthCm , prob = T, breaks = 30)
lines(density(iris$PetalLengthCm))

hist(iris$PetalWidthCm , prob = T, breaks = 30)
lines(density(iris$PetalWidthCm))
# Similar Histogram for other features confirms that the data has different scales and needs a normalization.

summary(iris)
# Confirms on the different scale and demands normalizing the data
# Apply Normalization technique to the whole dataset 

#Normalisation Function
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

iris_norm<-as.data.frame(lapply(iris[,c("SepalLengthCm","SepalWidthCm",
                                        "PetalLengthCm","PetalWidthCm" )],FUN=normalize))
summary(iris$SepalLengthCm)  # Before Normalisation 
summary(iris_norm$SepalLengthCm)   # After Normalisation
iris_norm$Species<-iris$Species
iris_norm$setosa <- iris_norm$Species=="Iris-setosa"
iris_norm$virginica <- iris_norm$Species == "Iris-virginica"
iris_norm$versicolor <- iris_norm$Species == "Iris-versicolor"

# Data Partition 

set.seed(123)
ind <- sample(2, nrow(iris_norm), replace = TRUE, prob = c(0.7,0.3))
iris_train <- iris_norm[ind==1,]
iris_test  <- iris_norm[ind==2,]

# Creating a neural network model on training data

iris_model<-neuralnet(Species ~  SepalLengthCm+SepalWidthCm+PetalLengthCm+PetalWidthCm ,data = iris_train , hidden = 10)
plotnet(iris_model, 
        alpha.val = 0.7, 
        circle_col = list('purple', 'white', 'white'), 
        bord_col = 'red')

# Test the model with test data
iris_prediction <- compute(iris_model, iris_test)
idx <- apply(iris_prediction$net.result, 1, which.max)
predicted <- as.factor(c('Iris-setosa', 'Iris-versicolor', 'Iris-virginica')[idx])
iris_test$Species<-as.factor(iris_test$Species)
confusionMatrix(predicted,iris_test$Species)


