############ req1 #####################################
rm(list = ls())
setwd("C:\\Users\\ramym\\Desktop\\SecondTerm\\BigData\\Lab 3 - Association Rule Mining")
####################  req2  ###########################
#??"rule mining"
#install.packages("arulesViz")

library("arules")
library('arulesViz')
########################### REQ 3 ##################################### 
data <- read.transactions('AssociationRules.csv')
########################## req 4 #####################################
print(inspect(data[1:100],linebreak=TRUE, itemSep = "   ", setStart = " * ", setEnd = " * "))
########################## req 5 ####################################
summary(data)
#    
#     most frequient items
#     item 13 = 4948
#     item 5  = 3699
######################## req 6 #####################################
itemFrequencyPlot(data,topN=5)
########################## req 7 ###############################
par <- list(supp = 0.01, conf = 0.5, minlen =2)

rules <- apriori(data, parameter = par)

summary(rules)
######################## req 8 #################################
rules_support<-sort(rules,by="support")
inspect(rules_support[1:6], linebreak = FALSE, ruleSep = "--->", itemSep = "  +", setStart = "", setEnd = "")
######################## req 9 #################################
rules_confid <- sort(rules, by = "confidence")
inspect(rules_confid[1:6], linebreak = FALSE, ruleSep = "--->", itemSep = "  +", setStart = "", setEnd = "")
######################## req 10 #################################
rules_lift <- sort(rules, by = "lift")
inspect(rules_lift[1:6], linebreak = FALSE, ruleSep = "--->", itemSep = "  +", setStart = "", setEnd = "")

####################### req 11 ################################
plot(rules, measure = c("support", "confidence"), shading = "lift")
#################### req 12 #####################################

if (FALSE) {
    
   "
    *
    given rule x----->y
    1- high support -----> model P(x,y)---> means that rule occur frequently so it have high business value 
    (for example if we used this rule will we can increase profit greatly)
    2- high confidnace ----> model P(y|x) ------>means that how accurate our rule is.
    3- high lift -------> model independance ------> means that is our rule came by coincedence or not ?
    (for example if x occur in all transactions in database so x is independant from any other item so it is not useful rule )
    *
    rules that have high support and confidence and lift will be valuables rules
    *
     our thresold on support depend on how much profit for example we want to increase 
     so assume 0.01 support acceptable.
    *
     so rules that have high confidance and lift will be the best and give good insight.
    *
    the highest 6 rules in support is  not useful because lift ~=1 and confidance ~=0.5 and and often items in the rules 
    are most frequent (so they are coincedence rules)
    *
    so the most importnant rules are like : 
                                            support     confidence  lift
    item15  +item30  +item49 ---> item56    0.0101      0.9619048   16.58456 
    item15  +item30  +item56 ---> item49    0.0101      0.7709924   19.42046 
    item30  +item56  +item84 ---> item49    0.0100      0.7407407   18.65846 

    "
}
