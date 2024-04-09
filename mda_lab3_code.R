A <- matrix(c(1, 1/2, 1/2, 2), nrow=2)
eigen_A <- eigen(A)
pca_A <- princomp(covmat=A)

poll_data <- subset(read.csv("data/pollution.csv",na.strings="."), select=-c(1,3))
poll_features <- subset(poll_data, select=-c(2))

# data analysis
summary(poll_data)
plot(poll_features)
cor(subset(poll_features, select=-c(1)))
apply(subset(poll_features, select=-c(1)), c(2), FUN=sd)

# normalize data
library(caret)
process <- preProcess(poll_data, method=c("range"))
norm_scale <- predict(process, poll_data)
poll_numeric <- subset(norm_scale, select=-c(1))
rownames(poll_numeric) <- norm_scale$town

# PCA
pol.pca <- princomp(poll_numeric, cor=T) # based on eigenvalues
summary(pol.pca)
pol.pca$loadings

pol.pca2 <- prcomp(poll_numeric) # based on svd
head(pol.pca2$x)
plot(pol.pca2, type="line")

library(dplyr)
cor_pca <- round(pol.pca$scores, digits=4)
cor_pca

biplot(pol.pca)
abline(h=0)
abline(v=0)

# NIR data
nir_data <- subset(read.csv("data/nir.csv",na.strings="."), select=-c(1))
nir.pcs <- prcomp(nir_data)
biplot(nir.pcs)
abline(h=0)
abline(v=0)
