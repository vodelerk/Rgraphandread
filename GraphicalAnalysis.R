library(ggplot2)
library(png)
library(RColorBrewer)


# a variable to store the source image
fileName <- "hand_linear_grey_small.png"
# convert the image to a ratser array
importedImage <- readPNG(fileName)
# Obtain te number of dimensions of the dataframe
dimensions <- dim(importedImage)
str(dimensions)


# Assign coordinates and RGB channels as data frame
originalImage <- data.frame(
  x = rep(1:dimensions[2], each = dimensions[1]),
  y = rep(dimensions[1]:1, dimensions[2]),
  value = as.vector(importedImage)
)



# Build the original color image
image_ggplot <- ggplot(data = originalImage, aes(x = x, y = y))
image_ggplot <- image_ggplot + geom_point(colour = grey(originalImage$value))
image_ggplot <- image_ggplot + labs(title = as.character(fileName))
image_ggplot <- image_ggplot + xlab("x") + scale_size("identity")
image_ggplot <- image_ggplot +ylab("y")


# Plot image
image_ggplot




# Number of clusters
kClusters <- 7
# Clustering
kMeans <- kmeans(originalImage[,c("value")], centers = kClusters)
# bind cluster data
originalImage$clusters <- kMeans$cluster
# Remove Backgorund data
finalImg <- originalImage
# Plot the resulting image
pal <- brewer.pal(kClusters,"BuPu")
clusteredImage <- ggplot (finalImg, aes(x,y, colour= as.factor(finalImg$clusters)))
clusteredImage <- clusteredImage + geom_point(data = subset(finalImg,finalImg$clusters==1), color = pal[1], alpha = 0.6, size = 0.1)
clusteredImage <- clusteredImage + geom_point(data = subset(finalImg,finalImg$clusters==2), color = pal[2], alpha = 0.6, size = 0.1)
clusteredImage <- clusteredImage + geom_point(data = subset(finalImg,finalImg$clusters==3), color = pal[3], alpha = 0.6, size = 0.1)
clusteredImage <- clusteredImage + geom_point(data = subset(finalImg,finalImg$clusters==4), color = pal[4], alpha = 0.6, size = 0.1)
clusteredImage <- clusteredImage + geom_point(data = subset(finalImg,finalImg$clusters==5), color = pal[5], alpha = 0.6, size = 0.1)
clusteredImage <- clusteredImage + geom_point(data = subset(finalImg,finalImg$clusters==6), color = pal[6], alpha = 0.6, size = 0.1)
clusteredImage <- clusteredImage + geom_point(data = subset(finalImg,finalImg$clusters==7), color = pal[7], alpha = 0.6, size = 0.1)
# facets
facetedImage <- ggplot (finalImg, aes(x,y), colour = cluster) + geom_point(size=0.5, alpha = 1/100)
facetedImage <- facetedImage + facet_wrap( ~ clusters)



clusteredImage


facetedImage


cDist <- ggplot(finalImg,aes(as.factor(clusters), fill=as.factor(clusters)))
cDist <- cDist + geom_bar()
cDist <- cDist + scale_fill_brewer(palette= "BuPu")
cDist


