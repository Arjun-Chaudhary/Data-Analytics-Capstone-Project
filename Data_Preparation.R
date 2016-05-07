require(corrplot)
require(matrixStats)
require(qVarSel)
require(data.table)

# Setting the working directory
setwd("Path to Working Directory")

# Reading the Data Set which is located in the working directory
cap_data <- read.csv("county_data.csv")

# Make a copy of the original dataset to avoid modifications in the main dataset
cap_data1<-copy(cap_data)

#-------Checking for Missing values--------

# list of rows with missing values
cap_data1[!complete.cases(cap_data1),]

# list of columns with missing values
cap_data1[,!complete.cases(cap_data1)]

# Omit any missing values
# cap_data1 <- na.omit(cap_data1,na.action=TRUE)
# Cannot Omit data as each row in the dataset represents county

#------Data reduction and Projection----------------

# Remove non-numeric columns from the dataset
cap_data1 <- cap_data1[,-c(1,2,3)]

## from this plot we observe that there are so many entries whose values are in the outlier category.
#Considering sensitivity of this entry it would not be a good idea to remove this outlier.

##Since the data attributes are of different varieties their scales are also different.
#In order to maintain uniform scalability we scale the columns

#Scale the dataset to minimize the effect of outliers and bringing the variables on the same scale
cap_data1 <- scale(cap_data1, center = TRUE, scale = TRUE)

# Calculate Correlation between variables in the dataset
cap_cor<- cor(cap_data1)

# Plot correlation Matrix
corrplot(cap_cor, method = "square", tl.cex=0.2, tl.srt = 45, tl.col = "black")

#-------------------------------------- Data Mining Task-------------------------

#-------------- choosing no. of Clusters K---------------------

# Calculating Sum of Squared errors(SSE)
wss <- (nrow(cap_data1)-1)*sum(colVars(as.matrix(cap_data1)))

# Iteratively calculating k-means for cluster number from 2 to 15
for (i in 2:15) wss[i] <- sum(kmeans(cap_data1,centers=i)$withinss)

#Plot Line chart of the Sum of Squared errors for each value of K
plot(1:15,wss,type="b",main="15 clusters",xlab="no. of cluster",ylab="with clsuter sum of squares")

#--------- Selecting Important variables---------------------------------------------------
cap_km <- kmeans(cap_data1, 8, iter.max = 200,nstart = 10, algorithm = "L")
temp_centers = cap_km$centers
distance = PrtDist(cap_data1, temp_centers)
# Select 15 most representative variables using qVarselH function, use 200 iterations
lsH <- qVarSelH(distance, 15, maxit = 200)
sq = 1:(dim(cap_data1)[2])
vrb = sq[lsH$x > 0.01]
sel_cap = as.data.frame(cap_data1[ ,vrb])

#------------Choosking best K from new set of variables-------------------------

wss <- (nrow(sel_cap)-1)*sum(colVars(as.matrix(sel_cap)))
for (i in 2:15) wss[i] <- sum(kmeans(sel_cap,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


temp_data<-copy(cap_data)

#Preparing the data for Shiny App
sel_cap <- cbind.data.frame(cap_data[,c(1,2,3)],sel_cap)
write.csv(sel_cap,"cap_data.csv", row.names = FALSE)



