#' Samuel Brinkmann, s2623525
#' Add your own function definitions on this file.

#' Log-Exponential density
#'
#' Compute the density or log-density for a Log-Exponential (LogExp)
#' distribution
#'
#' @param x vector of quantiles
#' @param rate vector of rates
#' @param log logical; if TRUE, the log-density is returned

dlogexp <- function(x, rate = 1, log = FALSE) {
  result <- log(rate) + x - rate * exp(x)
  if (!log) {
    exp(result)
  }
  result
}

#' Log-Sum-Exp
#'
#' Convenience function for computing log(sum(exp(x))) in a
#' numerically stable manner
#'
#' @param x numerical vector

log_sum_exp <- function(x) {
  max_x <- max(x, na.rm = TRUE)
  max_x + log(sum(exp(x - max_x)))
}


#' wquantile 
#'
#' Calculates empirical sample quantiles with optional weights, for given probabilities. 
#' Like in quantile(), the smallest observation corresponds to a probability of 0 and the largest to a probability of 1. 
#' Interpolation between discrete values is done when type=7, as in quantile(). 
#' Use type=1 to only generate quantile values from the raw input samples.
#'
#' @param x numeric vector whose sample quantiles are wanted
#' NA and NaN values are not allowed in numeric vectors unless na.rm is TRUE
#' @param probs numeric vector of probabilities with values in [0,1]
#' @param na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed
#' @param type numeric, 1 for no interpolation, or 7, for interpolated quantiles. Default is 7
#' @param weights	 numeric vector of non-negative weights, the same length as x, or NULL. The weights are normalised to sum to 1. If NULL, then wquantile(x) behaves the same as quantile(x), with equal weight for each sample value

wquantile <- function (x, probs = seq(0, 1, 0.25), na.rm = FALSE, type = 7, 
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

#' Compute empirical weighted cumulative distribution
#'
#' Version of `ggplot2::stat_ecdf` that adds a `weights` property for each
#' observation, to produce an empirical weighted cumulative distribution function.
#' The empirical cumulative distribution function (ECDF) provides an alternative
#' visualisation of distribution. Compared to other visualisations that rely on
#' density (like [geom_histogram()]), the ECDF doesn't require any
#' tuning parameters and handles both continuous and discrete variables.
#' The downside is that it requires more training to accurately interpret,
#' and the underlying visual tasks are somewhat more challenging.
#'
# @inheritParams layer
# @inheritParams geom_point
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If `TRUE`, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @section Computed variables:
#' \describe{
#'   \item{x}{x in data}
#'   \item{y}{cumulative density corresponding x}
#' }
#' @seealso wquantile
#' @export
#' @examples
#' library(ggplot2)
#'
#' n <- 100
#' df <- data.frame(
#'   x = c(rnorm(n, 0, 10), rnorm(n, 0, 10)),
#'   g = gl(2, n),
#'   w = c(rep(1/n, n), sort(runif(n))^sqrt(n))
#' )
#' ggplot(df, aes(x, weights = w)) + stat_ewcdf(geom = "step")
#'
#' # Don't go to positive/negative infinity
#' ggplot(df, aes(x, weights = w)) + stat_ewcdf(geom = "step", pad = FALSE)
#'
#' # Multiple ECDFs
#' ggplot(df, aes(x, colour = g, weights = w)) + stat_ewcdf()
#' ggplot(df, aes(x, colour = g, weights = w)) +
#'   stat_ewcdf() +
#'   facet_wrap(vars(g), ncol = 1)

stat_ewcdf <- function(mapping = NULL, data = NULL,
                       geom = "step", position = "identity",
                       ...,
                       n = NULL,
                       pad = TRUE,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEwcdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      ...
    )
  )
}


#' @title StatEwcdf ggproto object
#' @name StatEwcdf
#' @rdname StatEwcdf
#' @aliases StatEwcdf
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 aes after_stat has_flipped_aes Stat
NULL

StatEwcdf <- ggplot2::ggproto(
  "StatEwcdf", ggplot2::Stat,
  required_aes = c("x|y", "weights"),
  dropped_aes = c("weights"),     
  
  default_aes = ggplot2::aes(y = ggplot2::after_stat(y)),
  
  setup_params = function(data, params) {
    params$flipped_aes <-
      ggplot2::has_flipped_aes(data,
                               params,
                               main_is_orthogonal = FALSE,
                               main_is_continuous = TRUE)
    
    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    if (!has_x && !has_y) {
      rlang::abort("stat_ewcdf() requires an x or y aesthetic.")
    }
    has_weights <- !(is.null(data$weights) && is.null(params$weights))
    #    if (!has_weights) {
    #      rlang::abort("stat_ewcdf() requires a weights aesthetic.")
    #    }
    
    params
  },
  
  compute_group = function(data, scales, n = NULL, pad = TRUE, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      x <- unique(data$x)
    } else {
      x <- seq(min(data$x), max(data$x), length.out = n)
    }
    
    if (pad) {
      x <- c(-Inf, x, Inf)
    }
    if (is.null(data$weights)) {
      data_ecdf <- ecdf(data$x)(x)
    } else {
      data_ecdf <-
        spatstat.geom::ewcdf(
          data$x,
          weights = data$weights / sum(abs(data$weights)) 
        )(x)
    }
    
    df_ecdf <- vctrs::new_data_frame(list(x = x, y = data_ecdf), n = length(x))
    df_ecdf$flipped_aes <- flipped_aes
    ggplot2::flip_data(df_ecdf, flipped_aes)
  }
)


#' negative log likelihood
#' Negative log likelihood function for parameter optimization
#'
#' @param beta vector of model parameters
#' @param data dataframe with columns CAD_Weight and Actual_Weight
#' @param model string that is either "A" (mathematical model) or "B" (physical model)

neg_log_like <- function(beta, data, model){
  X <- data$CAD_Weight
  mu <- beta[1] + beta[2] * X
  if (model == "A"){
    sigma <- sqrt(exp(beta[3] + beta[4] * X))
  } else if (model == "B"){
    sigma <- sqrt(exp(beta[3]) + exp(beta[4]) * X**2)
  }
  -sum(dnorm(data$Actual_Weight, mean=mu, sd=sigma, log=TRUE))
}

#' filament1 estimate function
#' Calculates model parameter for given data using the negative
#' log likelihood function.
#' Returns the optimised model parameter and their hessian matrix.
#' If model="A", it uses the initial model parameter values (-0.1, 1.07, -2, 0.05).
#' If model="B", it uses the initial model parameter values (-0.15, 1.07, -13.5, -6.5)
#'
#' @param data dataframe with columns CAD_Weight and Actual_Weight
#' @param model string that is either "A" (mathematical model) or "B" (physical model)

filament1_estimate <- function(data, model){
  if (model == "A"){
    initial_values <- c(-0.1, 1.07, -2, 0.05)
  } else if (model == "B"){
    initial_values <- c(-0.15, 1.07, -13.5, -6.5)
  }
  opt <- optim(par = initial_values, fn = neg_log_like, data = data, model=model, hessian = T)
  
  # Check for convergence
  if (opt$convergence != 0) {
    warning("Optimization did not converge. Check initial values and model specification.")
  }
  
  list(par=opt$par, hessian=opt$hessian)
}

#' Confidence interval from fit
#' Calculates the confidence interval for a fit and returns it as a
#' dataframe (4 x 4) with columns "Lower", "Estimate", "Upper", and "Width".
#'
#' @param fit list containing a parameter vector and their hessian
#'    intended to use with return value of filament1_estimate function
#' @param alpha confidence level, default is 90%

get_CI_from_fit <- function(fit, alpha=0.9){
  var_cov_matrix <- solve(fit$hessian)  # Inverse of the Hessian matrix
  
  # Calculate standard errors
  std_errors <- sqrt(diag(var_cov_matrix))
  
  # z-score for 'alpha' confidence interval (two-tailed)
  z_score <- qnorm((1+alpha)/2)
  
  # Construct confidence intervals
  conf_intervals <- data.frame(
    Lower = fit$par - z_score * std_errors,
    Estimate = fit$par,
    Upper = fit$par + z_score * std_errors,
    Width = 2 * z_score * std_errors
  )
  rownames(conf_intervals) <- c("Beta1", "Beta2", "Beta3", "Beta4")
  return(conf_intervals)
}

#' log prior density
#' Calculates the logarithmic prior density for the model parameter
#' of the bayesian model and returns the value of the joint prior density.
#'
#' @param theta vector of model parameter
#' @param params vector of prior distribution parameter of model parameter

log_prior_density <- function(theta, params){
  # Extracting individual gamma values for readability
  gamma1 <- params[1]
  gamma2 <- params[2]
  gamma3 <- params[3]
  gamma4 <- params[4]
  
  log_prior_theta1 <- dnorm(theta[1], mean = 0, sd = sqrt(gamma1), log = TRUE)
  log_prior_theta2 <- dnorm(theta[2], mean = 1, sd = sqrt(gamma2), log = TRUE)
  log_prior_theta3 <- dlogexp(theta[3], rate = gamma3, log = TRUE)
  log_prior_theta4 <- dlogexp(theta[4], rate = gamma4, log = TRUE)
  
  # Sum of log priors
  log_prior_theta1 + log_prior_theta2 + log_prior_theta3 + log_prior_theta4
}

#' log likelihood
#' Evaluates the observation log-likelihood.
#'
#' @param theta vector of model parameter
#' @param x vector of CAD weight
#' @param y vector of actual weight

log_like <- function(theta, x, y){
  mu <- theta[1]+theta[2]*x
  sigma <- sqrt(exp(theta[3]) + exp(theta[4])*x**2)
  sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
}

#' log posterior density
#' Evaluates the logarithm of the posterior density, 
#' apart from some unevaluated normalisation constant.
#'
#' @param theta vector of model parameter
#' @param x vector of CAD weight
#' @param y vector of actual weight
#' @param params vector of prior distribution parameter of model parameter

log_posterior_density <- function(theta, x, y, params){
  log_like(theta,x,y) + log_prior_density(theta, params)
}

#' negative log posterior density
#' Evaluates the negative logarithm of the posterior density, 
#' apart from some unevaluated normalisation constant.
#' Used for maximazation with 'optim'.
#'
#' @param theta vector of model parameter
#' @param x vector of CAD weight
#' @param y vector of actual weight
#' @param params vector of prior distribution parameter of model parameter

neg_log_posterior_density <- function(theta, x, y, params){
  -log_posterior_density(theta, x, y, params)
}

#' posterior mode
#' Finds the mode mu of the log-posterior-density and evaluates 
#' the Hessian at the mode as well as the inverse of the negated Hessian, S.
#' Returns a list with 'mode', 'hessian', and 'S' as keys.
#'
#' @param theta_start vector of initial model parameter for optimization
#' @param x vector of CAD weight
#' @param y vector of actual weight
#' @param params vector of prior distribution parameter of model parameter

posterior_mode <- function(theta_start, x, y, params){
  opt <- optim(par = theta_start, fn = neg_log_posterior_density, x=x, y=y, params=params, hessian = T)
  mode <- opt$par
  hessian <- opt$hessian
  S <- solve(hessian)
  list(mode=mode, hessian=hessian, S=S)
}

#' importance sampling
#' Calculates importance sampling for bayesian model and
#' returns a dataframe with five columns,
#' beta1, beta2, beta3, beta4, and log_weights.
#'
#' @param N number of samples to generate
#' @param mu mean vector for the importance distribution
#' @param S covariance matrix
#' @param x vector of CAD weight
#' @param y vector of actual weight
#' @param params vector of prior distribution parameter of model parameter

library(mvtnorm)

do_importance <- function(N, mu, S, x, y, params){
  theta_samples <- rmvnorm(N, mean=mu, sigma=S)
  
  # Preallocate a vector for the log weights
  log_weights <- rep(NA, N)
  
  # Calculate log weights for each sample
  for (i in 1:N) {
    # Calculate the log-posterior for the ith sample
    log_posterior <- log_posterior_density(theta_samples[i, ], x, y, params)
    
    # Calculate the log of the multivariate normal density for the ith sample
    log_importance <- dmvnorm(theta_samples[i, ], mean = mu, sigma = S, log = TRUE)
    
    # Store the log weight (log posterior minus log importance)
    log_weights[i] <- log_posterior - log_importance
  }
  
  # Normalize the log weights using the log_sum_exp function
  log_weights <- log_weights - log_sum_exp(log_weights)
  
  # Create a dataframe with the parameter beta_samples and normalized log-weights
  data.frame(beta1 = theta_samples[,1],
             beta2 = theta_samples[,2], 
             beta3 = exp(theta_samples[,3]), 
             beta4 = exp(theta_samples[,4]), 
             log_weights = log_weights)
}

#' make confidence interval
#' Calculates weighted confidence interval and
#' returns a dataframe with columns,
#' Lower (lower bound), Upper (upper bound), and
#' Width (width of confidence interval).
#'
#' @param x vector of data points
#' @param weights vector of weights of the data points
#' @param prob confidence value, default is 90%

make_CI <- function(x, weights, prob = 0.9) {
  p <- (1 - prob) / 2
  lower <- wquantile(x, probs = p, weights = weights, type = 7)
  upper <- wquantile(x, probs = 1 - p, weights = weights, type = 7)
  data.frame(Lower = lower, Upper = upper, Width = upper-lower)
}
