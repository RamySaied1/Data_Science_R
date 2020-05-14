#setwd("~/LAB")
rm(list=ls())
setwd("C:\\Users\\ramym\\Desktop\\SecondTerm\\BigData\\Lab 5 _ 6 - Regression\\Lab 5 - Linear Regression")



#=============================Part(1)=====================================
x <- runif(100, 0, 10)     # 100 draws between 0 & 10


#(Q1) Try changing the value of standard deviation (sd) in the next command 
#How do the data points change for different values of standard deviation?
y <- 5 + 6*x + rnorm(100, sd =2)  # default values for rnorm (mean = 0 and sigma = 1)

#Plot it
plot (x,y)

# OLS model
# OLS : Ordinary Least Squares
model1 <- lm(y ~ x)
# Learn about this object by saying ?lm and str(d)

# Compact model results
print(model1)
#(Q2) How are the coefficients of the linear model affected by changing the value
#of standard deviation in Q1?

# Regression diagnostics --
ypred <- predict(model1) # use the trained model to predict the same training data
# Learn about predict by saying ?predict.lm

par(mfrow=c(1,1))
plot(y,y, type="l", xlab="true y", ylab="predicted y") # ploting the ideal line
points(y, ypred) # plotting the predicted points
                 # the nearer to the ideal line the better

# Detailed model results
d1 <- summary(model1)
print(model1)

#(Q3) How is the value of R-squared affected by changing the value
#of standard deviation in Q1?

# Learn about this object by saying ?summary.lm and by saying str(d)
cat("OLS gave slope of ", d1$coefficients[2,1],   
    "and an R-sqr of ", d1$r.squared, "\n")

#Graphic dignostic (cont.)
par(mfrow=c(1,1)) # parameters for the next plot
plot(model1, 1) # plot one diagnostic graphs

#(Q4)What do you conclude about the residual plot? Is it a good residual plot?
#========================End of Part(1)==============================================

#========================Part(2)=====================================================
#Training a linear regression model
x1 <- runif(100) 
# introduce a slight nonlinearity
#(A)
y1 = 5 + 6*x1 + 60*x1*x1 + rnorm(100)
plot(x1,y1)
model <- lm(y1 ~ x1)
summary(model)
#Creating a test set (test vector)
#EDIT: We renamed the variable as x1 instead of xtest (in previous versions)
#becaues the lm function searches in the formula for variables named 
#with x1 and not any other name.
#So, if you used xtest, the lm function will not know what is xtest and
#a random plot will be generated. 
ytrue = 5 + 6*x1 + 60*x1*x1 + rnorm(100)  # same equation of y1 but on xtest to get true y for xtest

ypred <- predict(model, data.frame(x1))

par(mfrow=c(1,1))
plot(ytrue, ytrue, type="l", xlab="true y", ylab="predicted y")
points(ytrue, ypred)

# graphic dignostic (cont.)
par(mfrow=c(1,1)) # parameters for the next plot
plot(model, 1) # plot the diagnostic graphs

#(Q5)What do you conclude about the residual plot? Is it a good residual plot?

#(Q6)Now, change the coefficient of the non-linear term in the original model for (A) training 
#and (B) testing to a large value instead. What do you notice about the residual plot?
#===============================End of Part(2)=============================================




############################################################
############ solution of the parts 1 and 2 are in the documents ###########
#############################################################






#=================================Part(3)==================================================
#(Q7) Import the dataset LungCapData.tsv. What are the variables in this dataset?
data <- read.csv('LungCapData.tsv',sep='\t',header=TRUE)
head(data,100)


# variables are LungCap Age Height Smoke Gender Caesarean
########## 

#(Q8) Draw a scatter plot of Age (x-axis) vs. LungCap (y-axis). Label x-axis "Age" and y-axis "LungCap"
plot(data$Age, data$LungCap, xlab = "Age", ylab = "LungCap")


#(Q9) Draw a pair-wise scatter plot between Lung Capacity, Age and Height. 
# Basic Scatterplot Matrix
pairs(~ LungCap + Age + Height ,
    data = data,
    main = " Scatterplot Matrix between (Age, Height, LungCap)"
)
#Check the slides for how to plot a pair-wise scatterplot

#(Q10) Calculate correlation between Age and LungCap, and between Height and LungCap.
cor(data$Age, data$LungCap)
cor(data$Height, data$LungCap)

#Hint: You can use the function cor

#(Q11) Which of the two input variables (Age, Height) are more correlated to the 
#dependent variable (LungCap)?

########### Height is the highes corrletaed with Lung Cap


#(Q12) Do you think the two variables (Height and LungCap) are correlated ? why ?
########### yes from graph between age and height we can see correlation

#(Q13) Fit a liner regression model where the dependent variable is LungCap 
#and use all other variables as the independent variables
model <- lm(LungCap ~ Age +Height+ Smoke+ Gender+ Caesarean,  data)

#(Q14) Show a summary of this model
summary(model)
mean(model$residual^2)
#(Q15) What is the R-squared value here ? What does R-squared indicate?

# r^2 value = 0.854

#(Q16) Show the coefficients of the linear model. Do they make sense?
#If not, which variables don't make sense? What should you do?
model$coefficients



#(Q17) Redraw a scatter plot between Age and LungCap. Display/Overlay the linear model (a line) over it.
#Hint: Use the function abline(model, col="red").
plot(data$Age, data$LungCap, xlab = "Age", ylab = "LungCap")
abline(model)
#Note (1) : A warning will be displayed that this function will display only the first two 
#           coefficients in the model. It's OK.
#Note (2) : If you are working correctly, the line will not be displayed on the plot. Why?

# because the functon have taken Intercept and Age coeficient as the line parameters and so the line fitted not 
# in the range of age and lungCap scatter plot

#(Q18)Repeat Q13 but with these variables Age, Smoke and Cesarean as the only independent variables.
model <- lm(LungCap ~ Age + Smoke + Caesarean, data)
summary(model)




#(Q19)Repeat Q16, Q17 for the new model. What happened?
model$coefficients
plot(data$Age, data$LungCap, xlab = "Age", ylab = "LungCap")
abline(model,col='red')

# line have been drawn normally


#(Q20)Predict results for this regression line on the training data.
ypred <- predict(model,data[c("Age","Smoke","Caesarean")])
#(Q21)Calculate the mean squared error (MSE)of the training data.
mean(model$residual^2)
mean((data$LungCap-ypred)^2)








