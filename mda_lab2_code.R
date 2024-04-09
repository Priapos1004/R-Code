library(mvtnorm)
library(energy)
# library(ICS)
library(ICSNP)

mean <- c(1,2)
sigma <- matrix(c(1,2,2,5), nrow=2)
x <- rmvnorm(n=1000, mean = mean, sigma=sigma)
x

quantile <- qmvnorm(0.95, sigma=sigma, mean=mean)$quantile
quantile
dS <- dmvnorm(x, mean=mean, sigma=sigma)
mvnorm.test(x, R=100)

# Create the Q-Q plot
qqnorm(x)
qqline(x, col = "red") # Adds a reference line

# next part

data("trees")
summary(trees)

# plot(trees)
mvnorm.test(trees, R=10000) # p-value below 0.05 -> not normal distributed

# equality test (one-sided)
HotellingsT2(trees, mu=c(13, 75, 30))

# equality test (two-sided)
HotellingsT2(X=trees[1:15, ], Y=trees[16:31, ], mu=c(13, 75, 30))
