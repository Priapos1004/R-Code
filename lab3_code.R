lambda <- 7
n <- 100
n.rep <- 100

theta1 <- function(lambda){
  lambda
}
theta_hat1 <- function(y, n){
  sum(y)/n
}
theta_hat_sig1 <- function(theta_hat, n){
  sqrt(1/n * abs(theta_hat))
}

theta2 <- function(lambda){
  sqrt(lambda)
}
theta_hat2 <- function(y, n){
  sqrt(sum(y)/n)
}
theta_hat_sig2 <- function(theta_hat, n){
  sqrt(1/(4*n*theta_hat))
}

theta3 <- function(lambda){
  log(lambda)
}
theta_hat3 <- function(y, n){
  log(sum(y)/n)
}
theta_hat_sig3 <- function(theta_hat, n){
  sqrt(1/(n*theta_hat)) 
}

experiment <- function(n, n.rep, lambda, theta_func, theta_hat_func, theta_hat_sig_func, alpha=0.05){
  n.ci <- 0
  theta <- theta_func(lambda)
  for(i in 1:n.rep){
    y <- rpois(n, theta)
    theta.hat <- theta_hat_func(y,n)
    if (is.infinite(theta.hat) || is.nan(theta.hat)){
      next
    }
    sig.theta <- theta_hat_sig_func(theta.hat, n)
    if (theta.hat + qnorm(p=alpha/2, lower.tail=TRUE)*sig.theta < theta && theta.hat + qnorm(p=alpha/2, lower.tail=FALSE)*sig.theta > theta){
      n.ci <- n.ci + 1
    }
  }
  n.ci/n.rep
}

experiment(n, n.rep, lambda, theta1, theta_hat1, theta_hat_sig1)
experiment(n, n.rep, lambda, theta2, theta_hat2, theta_hat_sig2)
experiment(n, n.rep, lambda, theta3, theta_hat3, theta_hat_sig3)