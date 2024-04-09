m <- 100000
n <- 5
a <- 1/5
lambda <- 10

y <- rpois(n, lambda)
y_sum <- sum(y)
ga_mean <- log((1+y_sum)/(n+a))
ga_sd <- sqrt(1/(1 + y_sum))
x <- rnorm(m, mean=ga_mean, sd=ga_sd)
w_log_denominator <- dnorm(x, mean=ga_mean, sd=ga_sd, log=T)

pyt <- function(x, y, n){
  exp(-n*exp(x))*exp(x*sum(y))*1/prod(factorial(y))
}

pyt_log <- function(x, y, n){
  -n*exp(x)+x*sum(y) - log(prod(factorial(y)))
}

pt <- function(x, a){
  a*exp(x - a*exp(x))
}

pt_log <- function(x, a){
  log(a) + x - a*exp(x)
}

w_log <- pt_log(x,a) + pyt_log(x,y,n) - w_log_denominator
w <- exp(w_log - max(w_log))




### next ####


wquantile = function (x, probs = seq(0, 1, 0.25), na.rm = FALSE, type = 7,
                      weights = NULL, ...)
{
  if (is.null(weights) || (length(weights) == 1)) {
    weights <- rep(1, length(x))
  }
  stopifnot(all(weights >= 0))
  stopifnot(length(weights) == length(x))
  if (length(x) == 1) {
    return(rep(x, length(probs)))
  }
  n <- length(x)
  q <- numeric(length(probs))
  reorder <- order(x)
  weights <- weights[reorder]
  x <- x[reorder]
  wecdf <- pmin(1, cumsum(weights)/sum(weights))
  if (type == 1) {
  }
  else {
    weights2 <- (weights[-n] + weights[-1])/2
    wecdf2 <- pmin(1, cumsum(weights2)/sum(weights2))
  }
  for (pr_idx in seq_along(probs)) {
    pr <- probs[pr_idx]
    if (pr <= 0) {
      q[pr_idx] <- x[1]
    }
    else if (pr >= 1) {
      q[pr_idx] <- x[n]
    }
    else {
      if (type == 1) {
        j <- 1 + pmax(0, pmin(n - 1, sum(wecdf <= pr)))
        q[pr_idx] <- x[j]
      }
      else {
        j <- 1 + pmax(0, pmin(n - 2, sum(wecdf2 <= pr)))
        g <- (pr - c(0, wecdf2)[j])/(wecdf2[j] - c(0,
                                                   wecdf2)[j])
        q[pr_idx] <- (1 - g) * x[j] + g * x[j + 1]
      }
    }
  }
  q
}

wquantile(x, weights = w)
