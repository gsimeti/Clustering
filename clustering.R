# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))
install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
new_wine <- wine
new_wine[,1] <- NULL
new_wine <- scale(new_wine)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(new_wine)

# Exercise 2:
#   * How many clusters does this method suggest?
# The method suggests 3 clusters.
#   * Why does this method work? What's the intuition behind it?
# The idea behind this method is that as the number of clusters grows, the sum of squares
# within will decrease. Basically, if the variance within each group decreases, then the 
# cluster makes up observations that are more closely related to one another. With the 
# incorporation of the k-means function, we know that the clusters are adequately diverse
# to one another. When the WSS levels off, then it's a signal that the number of clusters
# has gotten to a point at which there is significantly quantifiable difference.
#   * Look at the code for wssplot() and figure out how it works
# The code calculates the sum of squares within for each number of clusters, then plots
# the number against the number of clusters per the kmeans function.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(new_wine, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# This method suggests 3 as the optimal number of clusters.


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(new_wine, centers = 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

wine.table <- table(fit.km$cluster, wine$Type)
wine.table

# This is a good clustering, as it distinguishes between the wine types very well.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

library(cluster)
clusplot(new_wine, fit.km$cluster)

# This is a good clustering as well because it shows visually distinctive data types
# for each cluster.

install.packages("flexclust")
library(flexclust)

# Use Rand index to determine efficiency of clustering
randIndex(wine.table)
