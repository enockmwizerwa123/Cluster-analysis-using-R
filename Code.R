
###################-------------------###################
#############                                 ###########
#               Preprosessing the Data                  #
#############                                 ###########
###################-------------------###################

rm(list=ls()) ## this command clears the history
## setwd("/home/alabi") ## set your working directiory

data<-my_data <- read.csv("Data.csv") ## importing the data

rownames(data) <- data[,1]  ## Setting rownames

new_data=data[,-1]  ## removing the first column because it only serves as our row names

class(new_data) # checking your data class

dim(new_data) # getting the dimension of your data

names(new_data) # getting the column names

str(new_data) # checking data structure

anyNA(new_data) # checking for missing values

summary(new_data) # summary of the data

View(new_data) # view your data

###################-------------------###################
#############                                 ###########
#                    Cluster Analysis                   #
#############                                 ###########
###################-------------------###################


## standardizing and preparing the dataset
new_data_scaled <- scale(new_data) ## standardizing the variables
new_data_scaled.dist <- dist(new_data_scaled) # distance computation with year as observation
new_data_scaled.distT <- dist(t(new_data_scaled)) # distance computation with year as variables



# performing hierarchical clustering of the observations (variables) using Ward method,
#single and average linkage 
ward = hclust(new_data_scaled.distT,method='ward.D2')
single= hclust(new_data_scaled.distT,method='single')
average = hclust(new_data_scaled.distT,method='average')

## Plotting the dendogram
par(mfrow = c(1 ,3))
plot(ward, main = 'Wards Method',xlab =" ", sub ="")
rect.hclust(ward, k =3, border = 'red') ## selecting three clusters
plot(single, main = 'Single Linkage',xlab =" ", sub ="")
rect.hclust(single, k =3, border = 'red') ## selecting three clusters
plot(average, main = 'Average Linkage',xlab =" ", sub ="")
rect.hclust(average, k =3, border = 'red') ## selecting three clusters

## cutting the trees to see the groups selected above
cutree (ward,3)
cutree (single,3)
cutree (average,3)

# performing hierarchical clustering of the observations (years) using Ward method,single 
# and average linkage 
wardT = hclust(new_data_scaled.dist,method='ward.D2')
singleT = hclust(new_data_scaled.dist,method='single')
averageT = hclust(new_data_scaled.dist,method='average')

par(mfrow = c(3 ,1))
plot(wardT, main = 'Wards Method',xlab =" ", sub ="")
rect.hclust(wardT, k =3, border = 'red') ## selecting three clusters
plot(singleT, main = 'Single Linkage',xlab =" ", sub ="")
rect.hclust(singleT, k =4, border = 'red') ## selecting four clusters
plot(averageT, main = 'Average Linkage',xlab =" ", sub ="")
rect.hclust(averageT, k =5, border = 'red') ## selecting five clusters

## cutting the trees to see the groups selected above
cutree (wardT,3)
cutree (singleT,4)
cutree (averageT,5)