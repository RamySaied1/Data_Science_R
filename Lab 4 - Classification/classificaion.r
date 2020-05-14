#################### req 1#########################
rm(list = ls())
setwd("C:\\Users\\ramym\\Desktop\\SecondTerm\\BigData\\Lab 4 - Classification")
# install.packages("e1071")
library("caret")
library("e1071")
#################### req 2#########################

dfm <- read.csv("nbtrain.csv")
summary(dfm)

if (FALSE)
{
    "variabels are 
    1- age 
    2- gender
    3- educ
    4- income"
}
#################### req 3#########################
train <- dfm[1:9000,]
test <- dfm[-c(1:9000), ]

summary(train)
summary(test)

if (FALSE) {
    "we split the data to see performance of our model 
    on unseen data to make sure it is general"
}
################### req 4%5 ##########################
model <- naiveBayes(income~., train, laplace =0.01)
model

if (FALSE)
{
    "
    laplace smoothing is used to handle zero propabilities 
    by adding some constant to count of each value and  
    also add it to the denomenator
    "
}
################### req 6 ##########################
testy<-predict(model,test)
testy
################### req 7 ##########################
# ?"confusionMatrix"
conMat<-confusionMatrix(testy, test$income)
conMat

if (FALSE)
{
    " for 
    1- class 10-50k ---> classification powe is good and only 6 of 803 wre wrong classified
    2- class 50-80K ---> classification power is so weak and no example is correctly is correctly classified
    3- class GT-80k ---> classification powe is not good but it is better than 50-80 class 
    because 8 out of 75 were correctly classified
    this can be deduced from sensitivity of each class 
    sensitivity of class 10-50k is 0.9925
    sensitivity of class 50-80K is 0
    sensitivity of GT-80k is 0.106

    this happend because the data was unbalanced so the classifier preferes the majority class (10-50k) most of the time
    "


}
#################### req 8 *************************************

conMat$overall

if (FALSE) {
    " accuracy = 0.797  ------------> from confusionMatrix function"
}
#### accuracy but it isnot a good measure because the data imbalanced so it is better using of f1 score
#### (minority classes have large misclassfiaction rates)
##################################################################
1-conMat$byClass[, "Sensitivity"]

if (FALSE) {
    " miss classification rates of each class is equal to 1- sensetivity 
    so from confisutionMatri function :
    sensitivity of each class 
    sensitivity of class 10-50k is 1-0.9925 = 0.0075
    sensitivity of class 50-80K is 1-0      = 1        
    sensitivity of GT-80k is       1-0.106  = 0.894"
}
