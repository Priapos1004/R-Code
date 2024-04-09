# input variable
n <- 100
theta_true <- c(2, -2, -2, 3)

##################
### Simulation ###
##################

xs <- c()
for (i in 1:n){
  xs[i] <- (i - 1)/(n - 1)
}
X <- cbind(rep(1, n), xs)

mu_sigma_calculator <- function(theta, X) {
  mu <- X %*% theta[1:2]
  sigma <- exp(X %*% theta[3:4])
  return(list(mu = mu, sigma = sigma))
}

simulator <- function(theta, X) {
  result <- mu_sigma_calculator(theta, X)
  data.frame(value = rnorm(nrow(X), mean = result$mu, sd = result$sigma))
}
y <- simulator(theta_true, X)

# plot simulated data
plot(y$value)

##################
### Estimation ###
##################

neg_log_like <- function(theta, y, X){
  result <- mu_sigma_calculator(theta, X)
  -sum(dnorm(y$value, mean=result$mu, sd=result$sigma, log=TRUE))
}

opt <- optim(
  rep(0,4),
  neg_log_like,
  y=y,
  X=X,
  method="Nelder-Mead",
  control=list(trace=1),
  hessian = TRUE,
)

theta_est <- exp(opt$par)

#####################
### Check results ###
#####################

library(ggplot2)
library(tidyr)
library(dplyr)

result_true <- mu_sigma_calculator(theta_true, X)
result_est <- mu_sigma_calculator(theta_est, X)

result_df <- data.frame(sigma_true = result_true$sigma, mu_true = result_true$mu, sigma_est = result_est$sigma, mu_est = result_est$mu)

# Ensure your result_df has an 'index' column or something similar
result_df <- result_df %>% mutate(index = row_number())

# Reshape the data into long format
long_df <- result_df %>%
  pivot_longer(cols = c(sigma_true, sigma_est), names_to = "variable", values_to = "sigma")

# Plotting
ggplot(long_df, aes(x = index, y = sigma, color = variable)) +
  geom_point() +
  labs(title="Comparison of Sigma True vs. Sigma Estimated",
       x="Index",
       y="Sigma Values",
       color="Variable") +
  facet_wrap(~ variable)

#######################
### Additional task ###
#######################

hessian_mat <- opt$hessian
invserse_hessian_mat <- solve(hessian_mat, diag(nrow(hessian_mat)))
