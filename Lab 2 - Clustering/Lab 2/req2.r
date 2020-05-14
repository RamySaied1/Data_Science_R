################################# Req 1 -----> clean worksapce + install packages ###########################
rm(list = ls())
setwd("C:\\Users\\ramym\\Desktop\\SecondTerm\\BigData\\Lab 2 - Clustering\\Lab 2")
# install.packages("rattle.data")
# install.packages(c("HSAUR", "NbClust"))
#install.packages("png")
library(cluster)
library(HSAUR)
library(png)
set.seed(1234)
############################ req 2 $$$$$$$#############################
image <- readPNG("bird_small.png")
######################### req 3 #########################################
dim(image) <- c(128*128,3)
dfm <-data.frame(image)
colnames(dfm) <- c('r','g','b')
head(dfm)
####################### req 4 ##################################################
km <- kmeans(dfm, 16, 15)
######################## req 5 ###############################################
clusplot(dfm, km$cluster, color = TRUE, span=FALSE,line=0,col.p=km$cluster)
cat(" number of pixels belongs to each cluster")
for (i in 1:length(km$size)) {
    cat("cluster ",i,"have ",km$size[i],' pixels belongs to it \n')
}
# clusplot draw clusters after doing pca to trancsform data to 2 dimeninal form
####################### req 6 ###############################################
km$centers
# centers represents the color of each cluster or segment  (mean of the cluster)
# we are trying to do image segmentation
# now each point can be replaced by mean of it's cluster
###################### req 7,8,9 ################################################
clustered_index <- km$cluster
clustered_color <- km$centers[clustered_index,1:3]
km$centers
dim(clustered_color) <- c (128,128,3)
dim (clustered_index) <- c(128,128,1)
clustered_index <- clustered_index
writePNG(clustered_color,"compressed.png")
save(clustered_index, file = "compressed_index")
##################################################################################

######### i have stored the file by two ways ###################
# 1-clustered image as 3d matrix (rpg) and using value of each cluster  ("compressed_rgb.png")
# 2- storing only cluster index as integer ("compressed_index.png")


