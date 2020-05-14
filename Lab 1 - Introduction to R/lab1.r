rm(list=ls())

setwd("C:\\Users\\ramym\\Desktop\\SecondTerm\\BigData\\ass1")

dfm <- read.csv("titanic.csv")

dim(dfm)

str(dfm)

head(dfm,10)

tail(dfm,10)

summary(dfm)

summary(dfm$Age)

if (FALSE){
    "1-  first quartile of age = 20.12 and it means that 25% dataless than it .
     2- Third quartile of age = 38 and it means that 75% of data less than it.
    " 
}

any(is.na(dfm$Age))


class(dfm$Embarked)

levels(dfm$Embarked)

if (FALSE){
    "
    Data type of Embarked is Factor(Catagorial)

    levels of Embarked contain empty string so it's needed to be removed

    age contain null values so it's needed also to be removed.
    "
}

dfm<- dfm[complete.cases(dfm$Age), ]

any(is.na(dfm$Age))

dfm<-dfm[!(dfm$Embarked==""),]

summary(dfm$Embarked)


dfm<-subset( dfm, select = -c(Ticket,Cabin) ) 
str(dfm)

summary(dfm)

######################################################

female <-summary(dfm$Gender)[[1]][[1]]
male <-summary(dfm$Gender)[[2]][[1]]



slices <- c(female, male)
lbls <- c("Female", "Male")
colors <- c("red", "blue") 
pie(slices, labels = lbls, main="Pie Chart of Gender", col=colors)

dfmsur<-dfm[(dfm$Survived==1),]

female <-summary(dfmsur$Gender)[[1]][[1]]
male <-summary(dfmsur$Gender)[[2]][[1]]

slicessur <- c(female, male)
lblssur <- c("Female", "Male")
colorssur <- c("red", "blue") 
pie(slicessur, labels = lblssur, main="Pie Chart of Gender survised", col=colorssur)

if (FALSE){
    "
    from pie graphs we can conclude that females are better in survising than females
    "
}

count <- table(dfm$Survived, dfm$Pclass)
barplot(count, legend.text = TRUE, xlab = "Social class", ylab = "counts", col = c("red", "blue"))


if (FALSE) {
    "
    from barplot graph we can conclude that social class has greet effect on numberof supervised people
    1- class that max survised number is class 1
    2- class 1 ratio of (survised / total) is max
    "
}

boxplot(dfm$Age)

if (FALSE) {
    "
    Box plot is an easy way to describe distribution of numeric values and show imoprtant numbers
    1- min
    2- max
    3- medina
    4- first quartile
    5- third quartile   
    "
}

d <- density(dfm$Age)
plot(d, main = "Density of Age")



dfm_pre <- subset(dfm, select = c(Name, Survived))



write.csv(dfm_pre, "titanic_preprocessed.csv", row.names = FALSE)






