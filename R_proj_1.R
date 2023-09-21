library(readxl)
all_years_states <- read_excel("Desktop/June_15_Learn/College_project/all-years-states.xls")
View(all_years_states)

state_abbreviations <- c(
  "AL" = 1,
  "AZ" = 2,
  "AR" = 3,
  "CA" = 4,
  "CO" = 5,
  "CT" = 6,
  "DE" = 7,
  "FL" = 8,
  "GA" = 9,
  "ID" = 10,
  "IL" = 11,
  "IN" = 12,
  "IA" = 13,
  "KS" = 14,
  "KY" = 15,
  "LA" = 16,
  "ME" = 17,
  "MD" = 18,
  "MA" = 19,
  "MI" = 20,
  "MN" = 21,
  "MS" = 22,
  "MO" = 23,
  "MT" = 24,
  "NE" = 25,
  "NV" = 26,
  "NH" = 27,
  "NJ" = 28,
  "NM" = 29,
  "NY" = 30,
  "NC" = 31,
  "ND" = 32,
  "OH" = 33,
  "OK" = 34,
  "OR" = 35,
  "PA" = 36,
  "RI" = 37,
  "SC" = 38,
  "SD" = 39,
  "TN" = 40,
  "TX" = 41,
  "UT" = 42,
  "VT" = 43,
  "VA" = 44,
  "WA" = 45,
  "WV" = 46,
  "WI" = 47,
  "WY" = 48
)
def=all_years_states
df=all_years_states
count <- length(def$state)
count
count <- sum(!is.na(def$state))
count

table(def$state)
table(def$prod_year)
summary(def$state)

# Convert the "state" column to numerical values using state_abbreviations
def$state <- state_abbreviations[as.character(def$state)]
def
table(def$state)

#Production year and no of gas wells Visualization

a=table(def$prod_year)
barplot(a,main="Using BarPlot to display Production year and no of gas wells  Comparision",
        ylab="no of gas wells ",
        xlab="Production year ",
        col=rainbow(2),
        legend=rownames(a))


#States and no of gas wells Visualization

a=table(df$state)
barplot(a,main="Using BarPlot to display states and no of gas wells  Comparision",
        ylab="no of gas wells ",
        xlab="states ",
        col=rainbow(2),
        legend=rownames(a))

#K-means Algorithm
# Installing Packages
install.packages("ClusterR")
install.packages("cluster")
install.packages("ClusterR", type = "source")

install.packages("scales")
install.packages("gmp")
install.packages("ggplot2")
install.packages("ClusterR")
library(scales)
library(gmp)
library(ggplot2)
library(ClusterR)
# Loading package
library(Cluster)
library(cluster)

# Removing initial label of
#Species from original

def1<- def[, c("state", "prod_year","num_oil_wells","num_gas_wells")]
def1
plot(def1[, c("state","num_oil_wells")])
plot(def1[, c("state","num_gas_wells")])
plot(def1[, c("state","prod_year")])
def2 = def1[, c("state","prod_year")]
summary(def2)
table(def2)

km2 <- kmeans(na.omit(def2),2,nstart=50)
# list of cluster assignment
km2$cluster
# plot clustering result
plot(def2,col=km2$cluster,pch=20,cex=3)

#add cluster centers to the plot
points(kn2$centers, col=3, pch=15,cex=3)


library(cluster)
clusplot(def2,km2$cluster,lines =0,shade=TRUE,color=TRUE,labels = 2,span = TRUE, main= paste('clustering US States'),xlab ='US states',ylab='Production Year')

# Fitting K-Means clustering Model
# to training data
set.seed(240) # Setting seed
kmeans.re <- kmeans(def1, centers = 3, nstart = 20)
kmeans.re

# Cluster identification for
# each observation
kmeans.re$cluster

# Confusion Matrix
cm <- table(def1$state, kmeans.re$cluster)
cm

# Model Evaluation and visualization
plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")],
     col = kmeans.re$cluster)
plot(iris_1[c("Sepal.Length", "Sepal.Width")],
     col = kmeans.re$cluster,
     main = "K-means with 3 clusters")

## Plotiing cluster centers
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]

# cex is font size, pch is symbol
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")],
       col = 1:3, pch = 8, cex = 3)

## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')
