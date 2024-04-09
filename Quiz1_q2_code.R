# Define the negative log-likelihood function
negloglik <- function(theta, y, N) {
  # Extract parameters
  theta1 <- theta[1]
  theta2 <- theta[2]
  
  # Compute the probability for each observation
  logit_p <- theta1 + (1:length(y)) * theta2
  p <- exp(logit_p) / (1 + exp(logit_p))
  
  # Compute the negative log-likelihood
  -sum(dbinom(y, size = N, prob = p, log = TRUE))
}

# Test data
ytest <- c(1, 2, 4, 7, 4, 9, 11, 8, 9, 13) # rbinom(n = 20, size = 10, prob = 0.75)

# Use optim to minimize the negative log-likelihood
opt <- optim(par = c(0, 0), fn = negloglik, y = ytest, N = 15, hessian = T)

get_p_k <- function(theta, k) {
  logit_p_k <- theta[1] + k * theta[2]
  p_k <- exp(logit_p_k) / (1 + exp(logit_p_k))
  return(p_k)
}
p_estimates <- sapply(1:length(ytest), function(k) get_p_k(opt$par, k))
p_mean <- mean(p_estimates)

opt$par
p_mean
