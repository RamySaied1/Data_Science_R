rm(list = ls())
setwd("C:\\Users\\ramym\\Desktop\\SecondTerm\\BigData\\Lab 4 - Classification")

#install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
library("ROCR")

#Read the data
play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
play_decision
summary(play_decision)

#Build the tree to "fit" the model
?'rpart'
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 3),
             parms=list(split='information'))
# split='information' : means split on "information gain" 
#plot the tree
rpart.plot(fit, type = 4, extra = 1,)

summary(fit)
#######################################################################################
# Q1: what is the defult value for split?  
# default value  is 'gini'. function that can be used in splittings
# The Gini Index is calculated by subtracting the sum of the squared probabilities of each class from one
                            
###########################################################################################
# Q2: what are the meanings of these control parameters?  
#          1- "minsplit=2"
# the minimum number of observations that must exist in a node in order for a split to be attempted
#
#          2- "maxdepth=3" 
# Set the maximum depth of any node of the final tree, with the root node counted as depth 0
#
#          3- "minbucket=4" 
# the minimum number of observations in any terminal node
#
# Support your answers with graphs for different values of these parameters.
# attached with the code the graphs 
##########################################################################################

#Q3: What will happen if only one of either minsplit or minbucket is specified
#    and not the other?
# If only one of 'minbucket' or 'minsplit' is specified, the code either sets 'minsplit' to 'minbucket*3' or
#          'minbucket' to 'minsplit/3', as appropriate
#

#Q4: What does 'type' and 'extra' parameters mean in the plot function?
    #Type : Type of plot. Possible values:
    #0 Draw a split label at each split and a node label at each leaf.
    #1 Label all nodes, not just leaves. Similar to text.rpart's all=TRUE.
    #2 Default. Like 1 but draw the split labels below the node labels. Similar to the plots in the CART book.
    #3 Draw separate split labels for the left and right directions.

    #extra : Display extra information at the nodes. Possible values:
    #"auto" (case insensitive) Default. Automatically select a value based on the model type
    #0 No extra information.
    #1 Display the number of observations that fall in the node 
    #2 Class models: display the classification rate at the node
    # and so on 
######################################################################################

#Q5: Plot the tree with propabilities instead of number of observations in each node.
rpart.plot(fit, type = 4, extra = 4, )
######################################################################################
 
#Predict if Play is possible for condition rainy, mild humidity, high temperature and no wind
newdata <- data.frame(Outlook="overcast",Temperature="mild",Humidity="high",Wind=FALSE)
newdata
predict(fit,newdata=newdata,type=c("class"))
# type can be class, prob or vector for classification trees.

######################################################################################
#Q6: What is the predicted class for this test case?
# yes (play)

#Q7: State the sequence of tree node checks to reach this class (label).
# 1- check on tempreture and see that temp = Mild so go to left 
# 2- check on outLook and see that toutlook = overcast so predict yes

## ================================= END ===================================== ##