
################################# Req 1 -----> clean worksapce + install packages ###########################
rm(list = ls())
setwd("C:\\Users\\ramym\\Desktop\\SecondTerm\\BigData\\Lab 2 - Clustering\\Lab 2")
#install.packages("rattle.data")
#install.packages(c("HSAUR", "NbClust"))
library(NbClust)
library(cluster)
library(HSAUR)

set.seed(1234)



############################### Req2 read data and plot the points#########################################
dfm <- read.csv("clustering_data.csv")
head(dfm)
plot(dfm$X,dfm$Y)

############################## req 3 clustering ####################################
km <- kmeans(dfm, 10, 15)
plot(km$centers)
############################ req 4 ############################################
plot(km$centers)
############################ re1 5 ############################################
plot(dfm, col = km$cluster)
############################ req 6 ############################################
points(km$centers,bg="Black", col="Black", pch=24)
############################ req 7 ############################################
# points() ----> (low level function )to add points to existing plot (manual Drawing)
# plot() -----> (high level function)to plot points on new graph 

########################### req 8  #############################################
# first method use Dindex as the measure 
nc <- NbClust(dfm, min.nc = 2, max.nc = 15, method = "kmeans",index='dindex')
#  from the graph we see that the knee at value 3

# second method by calculating the meteric manually (withing group sum of squares).
wss <- (nrow(dfm) - 1) * sum(apply(dfm, 2, var))
for (i in 1:15) wss[i] <- sum(kmeans(dfm, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
# from the graph the best number of cluster is at the   knee = 4


# another way by using majority vote of many meritces
nc <- NbClust(dfm, min.nc = 2, max.nc = 15, method = "kmeans", index = "all")
####################### req 9 #############################################
t<-table(nc$Best.n[1,], dnn=c(" votes"))
row.names(t) <- paste0(row.names(t), "'clusters")
t[1:7] <- paste0(t[1:7], " votes")
t
# 3 has the maximum number of votes

############################# 10,11 #################################
km <- kmeans(dfm, 3, 100)
plot(km$centers)
plot(dfm, col = km$cluster)
points(km$centers, bg = "Black", col = "Black", pch = 24)





