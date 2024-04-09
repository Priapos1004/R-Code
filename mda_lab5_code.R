suppressPackageStartupMessages(library(reshape)) # Useful to re-arrange data set
library(fpc)
suppressPackageStartupMessages(library(tables)) # Using `tables` for summary statistics

temp=read.csv("data/temperatures.csv")
temp=temp[,2:ncol(temp)]

temp.long <- melt(temp,id.vars=c("City","Area")) # from wide to long format
names(temp.long)[3:4] <- c("Month","Temp")
boxplot(Temp ~ Month,data=temp.long,xaxt="n",yaxt="n")
axis(1, las=2,at=1:12,labels=unique(temp.long$Month)); axis(2, las=1)

#summary of te dataset
summary(temp[,3:14])

temp2 <- scale(temp[,3:14])
d <- dist(temp2, method = "euclidean") # Distance matrix: Euclidean on standarised variables
hc <- hclust(d, method="ward.D") # Ward's clustering method
plot(hc,labels=temp$City) # dendrogram

# Mapping areas to colors
area_colors <- c("South"="blue", "North"="green", "West"="red", "East"="orange")
label_colors <- area_colors[temp$Area[hc$order]]
plot(hc,labels=FALSE)
text(x = 1:length(temp$City), y = -3, labels = temp$City[hc$order], col = label_colors, srt = 90, adj = c(1,0.5), xpd = TRUE)
rect.hclust(hc, k=4, border="red")

plot(hc,labels=temp$City) # Needs to be called again
ward4 <- cutree(hc, k=4) # Extract a 4-cluster solution
ward4
rect.hclust(hc, k=4, border="red") # Show them on the dendrogram

plot(hc,labels=temp$City) # Needs to be called again
ward3 <- cutree(hc, k=3) # Extract a 3-cluster solution
ward3
rect.hclust(hc, k=3, border="red") # Show them on the dendrogram

### CH index ###
clusval3 <- cluster.stats(d,ward3) # Validation stats for a 3-cluster solution
clusval3$ch # CH index for a 3-cluster solution

clusval4 <- cluster.stats(d,ward4) # Validation stats for a 4-cluster solution
clusval4$ch # CH index for a 4-cluster solution

### Cluster centroids (standarised data) ###
st <- by(temp2,ward3,colMeans) # Mean vector by cluster
st <- matrix(unlist(st), nrow = 12)
colnames(st) <- c("Cluster 1","Cluster 2","Cluster 3")
st <- as.data.frame(st,row.names=levels(temp.long$Month))
print(st)

### Scatterplot matrix using the groups ###
dat <- cbind(ward3,temp2) # Append grouping information
dat <- as.data.frame(dat)
library(lattice) # This package simplifies using group colours
a <- splom(~dat[c(2:3,7:8)], groups = ward3, data = dat,
           par.settings = simpleTheme(col = 1:3, pch = 1:3),
           auto.key = list(columns = 3, title = "Cluster"))
b <- splom(~dat[c(5:6,11:12)], groups = ward3, data = dat,
           par.settings = simpleTheme(col = 1:3, pch = 1:3),
           auto.key = list(columns = 3, title = "Cluster"))
print(a, split=c(1,1,2,1), more=TRUE) # Side-by-side arrangement
print(b, split=c(2,1,2,1))

### Distances to the final cluster centroids (within-group distances) ###
d1 <- apply(t(temp2[ward3==1,]),2,function(x) dist(rbind(x,st[,1]))) # Cluster 1
d2 <- apply(t(temp2[ward3==2,]),2,function(x) dist(rbind(x,st[,2]))) # Cluster 2
d3 <- apply(t(temp2[ward3==3,]),2,function(x) dist(rbind(x,st[,3]))) # Cluster 3
c1 <- rep(1:3,times=c(length(d1),length(d2),length(d3)))
c2 <- c(as.character(temp$City[ward3==1]),as.character(temp$City[ward3==2]),
        as.character(temp$City[ward3==3]))
c3 <- c(as.character(temp$Area[ward3==1]),as.character(temp$Area[ward3==2]),
        as.character(temp$Area[ward3==3]))
distances <- data.frame(Cluster=c1,City=c2,Area=c3,Dist=c(d1,d2,d3))
# Plot distances to the cluster centroids
plot(distances$Cluster,distances$Dist,axes=F,ylab="Distance to cluster centroid",
     xlab="Cluster",col="white",xlim=c(0.75,3.25))
text(distances$Cluster,distances$Dist,labels=distances$City)
axis(1,at=1:3,labels=c("Cluster 1","Cluster 2","Cluster 3")); axis(2)


distances <- data.frame(Cluster=c1,City=c2,Area=c3,Dist=c(d1,d2,d3))
levels(distances$Area)=unique(distances$Area)#converting to levels
distances$Area=as.factor(distances$Area)
plot(distances$Cluster,distances$Dist,axes=F,ylab="Distance to cluster centroid",
     xlab="Cluster",col="white",xlim=c(0.75,3.25))
text(distances$Cluster,distances$Dist,labels=distances$City,col=as.numeric(distances$Area))
axis(1,at=1:3,labels=c("Cluster 1","Cluster 2","Cluster 3")); axis(2)
legend("topright",legend=levels(distances$Area),text.col=1:4,bty="n")

###############
### Whiskey ###
###############

#Summary statistics
whisky=read.csv("data/whisky.csv")
whisky=whisky[,2:ncol(whisky)]
dat <- whisky[,2:13] # subset of taste categories
dat.long <- melt(dat,measure.vars=colnames(dat)) # Data re-arrangement
names(dat.long) <- c("Category","Score")
dat.long <- as.data.frame(dat.long)
tab <- tabular(Category ~ Heading("Summary statistics")*
                 (Score*Heading(" "))*((Heading(Mean)*mean +
                                          (Heading(Median)*median +
                                             (Heading(St.Dev)*sd + (Heading(Max)*max +
                                                                      (Heading(Min)*min))))))*Format(digits=4), data=dat.long)
tab

### k-means ###
dat2 <- scale(dat) # On standardise variables
n <- nrow(dat2)
maxC <- 10 # Max number of clusters
wss <- rep(0, maxC)
wss[1] <- (n - 1) * sum(apply(dat2, 2, var))
for (i in 2:maxC) wss[i] <- sum(kmeans(dat2,centers = i,nstart=100)$withinss)
plot(1:10, wss, type = "b",axes = F,xlab = "Number of groups",
     ylab = "Within-groups sum of squares")
axis(side=1,at=1:maxC)
axis(side=2)

dat2.km <- kmeans(dat2, centers = 4, nstart=100) # k-means with k = 4
t(dat2.km$centers) # Cluster centres

# Within-cluster variability and size
dat2.km$withinss # Cluster homogeneity
dat2.km$size # Cluster sizes

# Cluster allocations
dat2.km$cluster

# The mtcars dataset:
data <- as.matrix(dat2)
# Default Heatmap
heatmap(data)
