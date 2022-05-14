#set working directory

setwd("C:/Users/ogech/OneDrive/Desktop/ASDM Coursework Clustering")
getwd()

#install necessary packages and load libraries
# install.packages("skmeans")
# install.packages(" NbClust")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("tidyr")
# install.packages("factoextra")
# install.packages("cluster")
# install.packages("caret")
library(caret)
library(cluster)
library(ggplot2) 
library(readr) 
library(tidyverse) 
library(ggthemes) 
library(stringr) 
library(plotly)
library(skmeans)
library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(gridExtra)
library(tidyr)
library(factoextra)

#read the data into a dataframe
clients <- read.csv("mall_customers_dataset.csv", header=T)
clients

View(clients)

#data exploration
names(clients)
head(clients)
tail(clients)
summary(clients)
str(clients)
dim(clients)

#view the data
View(clients)

#Attach the data for easy access
attach(clients)

#now check the gender distribution in the dataset
## GENDERS DISTRIBUTION

as.data.frame(table(Gender))  %>% 
  ggplot(aes(x = Gender, y = Freq))  +
  geom_bar(stat = "identity", fill = "#E08845") +
  geom_text(y = as.vector(table(Gender)), label = paste0((as.vector(table(Gender))/sum(as.vector(table(Gender))))*100, "%"))


names(clients)[names(clients) == "Annual.Income..k.."] <- "Annual_Income"
names(clients)[names(clients) == "Spending.Score..1.100."] <- "Spending_Score"

View(clients)
sapply(clients, function(x) sum(is.na(x)))

#Check the summary of the Age variable in the dataset
summary(Age)
ggplot(as.data.frame(Age), aes(y = Age)) + geom_boxplot(fill="#AF5400")

#Plot and visualize the Age variable distribution 
ggplot(clients, aes( x = Age, fill = Gender)) + geom_density(alpha = 0.5)

#Visualize the Boxplot for Annual Income & Spending score variable

Annual_Income <- Annual.Income..k..
Spending_Score <- Spending.Score..1.100.

Box_plot1 <- ggplot(as.data.frame(Annual_Income), aes(y = Annual_Income)) + geom_boxplot(fill='#F8766D') + ylim(c(1,150))

Box_plot2 <- ggplot(as.data.frame(Spending_Score), aes(y = Spending_Score)) + geom_boxplot(fill='#00BFC4') + ylim(c(1,150))
grid.arrange(Box_plot1, Box_plot2, ncol = 2)

#check for N/A values in the dataset

cat("There are", sum(is.na(clients)), "N/A values.")


normalise <- function(df)
{
  return(((df- min(df)) /(max(df)-min(df))*(1-0))+0)
} 





min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization to first four columns in iris dataset
clients_norm <- as.data.frame(lapply(clients[3:5], min_max_norm))
clients_norm

View(clients_norm)


hist(clients$Annual_Income)

hist(clients$Spending_Score)
distance <- dist(clients, method = "euclidean")
distance

fviz_dist(distance)

#Create a dataframe for the variables of interest
filtered <- clients[,c(4,5)]


#Now we determine the number of clusters using the Elbow approach

tot.withinss <- vector("numeric", length = 10)
for (i in 1:10){
  kDet <- kmeans(filtered, i)
  tot.withinss[i] <- kDet$tot.withinss
}

ggplot(as.data.frame(tot.withinss), aes(x = seq(1,10), y = tot.withinss)) + 
  geom_point(col = "#F8766D") +    
  geom_line(col = "#F8766D") + 
  theme(axis.title.x.bottom = element_blank()) +
  ylab("Within-cluster Sum of Squares") +
  xlab("Number of Clusters") +
  ggtitle("Elbow K Estimation")

#Drawing from the graph, I will choose the k = 5
#Hence, I create 5 clusters to generate segments

### CLUSTER THE DATA ###

clients.hclust <- hclust(distance)
clients.hclust

plot(clients.hclust, hang = -1)

plot(clients.hclust,labels=clients$Annual_Income)
rect.hclust(clients.hclust, 5)

plot(clients.hclust,labels=clients$Spending_Score)
rect.hclust(clients.hclust, 5)


### VISUALIZE THE CLUSTERS ###

clientsClusters <- kmeans(filtered, 5)
clientsClusters

ggplot(filtered, aes(x = Annual_Income, y = Spending_Score)) + 
  geom_point(stat = "identity", aes(color = as.factor(clientsClusters$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  ggtitle("Mall Clients Segments", subtitle = "K-means Clustering")


k_clients<-kmeans(filtered,5) #k=5
k_clients

clusplot(clients, k_clients$cluster, color=TRUE, shade=TRUE, lines=0)


#Scale and center the data


set.seed(123)
kc.fit <- kmeans(filtered, 5, nstart = 25) 
kc.fit$cluster
kc.fit$size


fviz_cluster(kc.fit,filtered)


filtered2 <-  subset(clients,
                          rownames(clients)!="Annual Income")


# Compute k-means 
set.seed(123)
kc.fit2 <- kmeans(filtered, 5, nstart = 25) 
kc.fit2$cluster
kc.fit2$size

# Visualise the clusters 

fviz_cluster(kc.fit2,filtered)


filtered3 <-  subset(clients,
                     rownames(clients)!="Spending Score")

# Compute k-means 
set.seed(123)
kc.fit3 <- kmeans(filtered, 5, nstart = 25) 
kc.fit3$cluster
kc.fit3$size

fviz_cluster(kc.fit3,filtered)




