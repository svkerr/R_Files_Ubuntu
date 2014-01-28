# Set local working directory
setwd("/home/skerr/DataSets/")

# Read in the data set
whiskies <- read.csv("whiskies.txt", row.names = 1, stringsAsFactors = FALSE)
str(whiskies)
sum(!is.na(whiskies))   # check for missing observations
whiskies_k <- scale(whiskies[2:13])  # rescale selected vars for kmeans

ssPlot <- function(data,maxCluster = 9) {
  # Ititialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data,2,var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:maxCluster,SSw,type = 'b', xlab = 'Number of Clusters',ylab = 'Within groups sum ofn squares')
}
ssPlot(whiskies_k)