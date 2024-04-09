mu <- log(2) - 1/2
sigma <- 1
m <- 15
K <- 10000

# theoretical expectation
for (i in 0:m){
  print(paste0(i,": ", dexp(i,rate = mu + sigma**2/2)))
}

x <- rnorm(K, mean=0, sd=sigma)

# monte carlo expectation
prob_mc <- c()
prob_true <- dpois(0:m,lambda=2)
for (i in 0:m){
  prob_mc[i+1] <- mean(dpois(i, lambda=exp(mu + x)))
  print(paste0(i,": ", prob_mc[i+1], " / ", prob_true[i+1]))
}
plot(prob_mc, type="o", col="red")
lines(prob_true, type="o", col="blue")

# generalisation
doverpois <- function(m, mu, sigma, K){
  x <- rnorm(K, mean=0, sd=sigma)
  lambda <- exp(mu + sigma**2/2)
  prob_true <- dpois(0:m,lambda=lambda)
  for (i in 0:m){
    prob_mc[i+1] <- mean(dpois(i, lambda=exp(mu + x)))
  }
  data.frame(id=0:m, cbind(prob_mc, prob_true))
}

library(ggplot2)
plot_results <- function(data){
  ggplot(data, aes(x = id)) +
    geom_line(aes(y = prob_mc, colour = "prob_mc")) +
    geom_point(aes(y = prob_mc, colour = "prob_mc")) +
    geom_line(aes(y = prob_true, colour = "prob_true")) +
    geom_point(aes(y = prob_true, colour = "prob_true")) +
    scale_colour_manual(values = c("prob_mc" = "red", "prob_true" = "blue")) +
    labs(x = "m", y = "Probability", title = "Comparison of prob_mc and prob_true") +
    theme_minimal()
}
result <- doverpois(m=30, mu=log(8)-1/8, sigma=1/2, K=10000)
plot_results(result)
