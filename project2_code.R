#' Samuel Brinkmann, s2623525
#' Add your own function definitions on this file.

#' neg_log_lik
#
#' @description Evaluate the negated log-likelihood for model A and B
#' @param beta A vector with the beta parameters
#' @param data A `data.frame` with the same variables as the `filament1` data set.
#' Must have columns `CAD_Weight` and `Actual_Weight`
#' @param model Either "A" for a log-linear variance model, or "B" for a proportional
#' scaling error model

neg_log_lik <- function(beta, data, model){
  
  mu <- beta[1] + beta[2]*data[["CAD_Weight"]]
  
  # distinguish between the two models to find the particular standard deviation for the betas
  if(model == "A") {
    sigma <- sqrt(exp(beta[3] + beta[4]*data[["CAD_Weight"]]))
  }else{
    sigma <- sqrt(exp(beta[3])+exp(beta[4]) * (data[["CAD_Weight"]]^2))
  }
  - sum(dnorm(data[["Actual_Weight"]],
              mean = mu,
              sd=sigma,
              log = TRUE))
  
}

#' filament_estimate
#
#' @description Estimate filament models with different variance structure
#' @param data A `data.frame` with the same variables as the `filament1` data set.
#' Must have columns `CAD_Weight` and `Actual_Weight`
#' @param model Either "A" for a log-linear variance model, or "B" for a proportional
#' scaling error model
#' @return An estimation object suitable for use with [filament1_predict()]

filament1_estimate <- function(data, model) {
  model <- match.arg(model, c("A", "B"))
  if (model == "A") {
    beta_start <- c(-0.1, 1.07, -2, 0.05)
  } else {
    beta_start <- c(-0.15, 1.07, -13.5, -6.5)
  }
  opt <- optim(beta_start,
               neg_log_lik,
               data = data,
               model = model,
               hessian = TRUE,
               method = "Nelder-Mead",
               control = list(maxit = 5000)
  )
  fit <- list(
    model = model,
    par = opt$par,
    hessian = opt$hessian
  )
  class(fit) <- c("filament1_estimate", "list")
  fit
}

#' filament1_aux_EV
#' 
#' @description Evaluate the expectation and variance for model A and B
#' @param beta A vector with the beta parameters
#' @param data A `data.frame` containing the required predictors, including `CAD_Weight`
#' @param model Either "A" for a log-linear variance model, or "B" for a proportional
#' scaling error model
#' @param Sigma_beta : If not NULL, an estimate of the covariance matrix for
#                 the uncertainty of estimated betas
#' @return A list with four elements:
#     E : E(y|beta,x)
#     V : Var(y|beta,x)
#     VE : Var(E(y|beta,x)|x) or NULL
#     EV : E(Var(y|beta,x)|x) or NULL

filament1_aux_EV <- function(beta, data, model = c("A", "B"),
                             Sigma_beta = NULL) {
  
  model <- match.arg(model)
  if (model == "A") {
    
    ZE.0 <- model.matrix( ~ 1 + CAD_Weight, data = data)
    ZV.0 <- model.matrix( ~ 1 + CAD_Weight, data = data)
    ZE = cbind(ZE.0, ZV.0 * 0) 
    ZV = cbind(ZE.0 * 0, ZV.0)
    
    VE <- EV <- NULL
    if (!is.null(Sigma_beta)) {
      # E(Var(y|beta,x)|x)
      EV <- exp(ZV %*% beta + rowSums(ZV * (ZV %*% Sigma_beta)) / 2)
      # Var(E(y|beta,x)|x)
      VE <- rowSums(ZE * (ZE %*% Sigma_beta))
    }
    out <- list(
      E = ZE %*% beta,
      V = exp(ZV %*% beta),
      VE = VE,
      EV = EV
    )
  } else {
    
    ZE.0 <- model.matrix( ~ 1 + CAD_Weight, data = data)
    ZV.0 <- model.matrix( ~ 1 + I(CAD_Weight^2), data = data)
    ZE = cbind(ZE.0, ZV.0 * 0) 
    ZV = cbind(ZE.0 * 0, ZV.0)
    
    VE <- EV <- NULL
    if (!is.null(Sigma_beta)) {
      # E(Var(y|beta,x)|x)
      # (pmin: Ignore large Sigma_beta values)
      EV <- ZV %*% exp(beta + pmin(0.5^2, diag(Sigma_beta)) / 2)
      # Var(E(y|beta,x)|x)
      VE <- rowSums(ZE * (ZE %*% Sigma_beta))
    }
    out <- list(
      E = ZE %*% beta,
      V = ZV %*% exp(beta),
      VE = VE,
      EV = EV
    )
  }
  out
}

#' filament1_predict
#' 
#' @description Compute predictive distributions and 95% prediction intervals for a new dataset 'test_data' with a model trained on 'train_data'
#' @param train_data A `data.frame` containing the required predictors, including `CAD_Weight`
#' @param test_data A `data.frame` containing the required predictors, including `CAD_Weight`
#' @param model Either "A" for a log-linear variance model, or "B" for a proportional scaling error model
#' @param lwr_upr_on : If TRUE, the lower and upper bound of the prediction interval are returned as two columns in the final data.frame
#' @param VE_EV_on : If TRUE, VE (Var(E(y|beta,x)|x) or NULL) and EV (E(Var(y|beta,x)|x) or NULL) are returned as two columns in the final data.frame
#' @return A data.frame with columns:
#     mean : expectation of the test data
#     sd : standard deviation of the test data
#     lwr : lower bound of prediction interval (optional)
#     upr : upper bound of prediction interval (optional)
#     VE : Var(E(y|beta,x)|x) or NULL of test data (optional)
#     EV : E(Var(y|beta,x)|x) or NULL of test data (optional)
filament1_predict <- function(train_data, test_data, model, lwr_upr_on=TRUE, VE_EV_on = FALSE) {
  # Ensure the model argument is matched properly
  model <- match.arg(model, c("A", "B"))
  
  # Estimate the model parameters using the training data
  fit <- filament1_estimate(train_data, model)
  
  # Retrieve estimated parameters and covariance matrix of the parameters
  beta <- fit$par
  Sigma_beta <- solve(fit$hessian)
  
  # Compute expectation and variance for the test data
  aux <- filament1_aux_EV(beta, test_data, model, Sigma_beta)
  
  # Calculate the mean and standard deviation for the predictive distribution
  pred_mean <- aux$E
  pred_sd <- sqrt(aux$V)
  
  # Combine results into a data frame
  prediction_results <- data.frame(
    mean = pred_mean,
    sd = pred_sd
  )
  
  if(lwr_upr_on) {
    # Compute 95% prediction intervals
    # Using the quantile function of the normal distribution
    lower_bound <- pred_mean - qnorm(0.975) * pred_sd
    upper_bound <- pred_mean + qnorm(0.975) * pred_sd
    
    prediction_results$lwr <- lower_bound
    prediction_results$upr <- upper_bound
  }
  
  # include the expected variance and variance of expectation
  if (!is.null(aux$VE) && !is.null(aux$EV) && VE_EV_on) {
    prediction_results$VE <- aux$VE
    prediction_results$EV <- aux$EV
  }
  
  # Return the prediction results
  return(prediction_results)
}

#' scoring_func
#' 
#' @description Calculate SE- and DS-score for given data and prediction on them
#' @param pred A `data.frame` containing the 'mean' and 'sd' column for the predictions
#' @param data A `data.frame` containing the 'Actual_Weight' column with observations
#' @return A data.frame containing the columns from 'data', 'pred', and a 'se' and 'ds' column with the scores
scoring_func <- function(pred, data){
  score <- cbind(data, pred) %>%
    mutate(
      se = (Actual_Weight - mean)**2,
      ds = (Actual_Weight - mean)**2/sd**2 + 2 * log(sd)
    )
  
  return(score)
}

#' leave1out
#' 
#' @description Calculate leave-one-out 'se' and 'ds' scores
#' @param data A `data.frame` containing the 'Actual_Weight' column with observations
#' @param model Either "A" for a log-linear variance model, or "B" for a proportional scaling error model
#' @return A data.frame containing the columns from 'data' and 'mean', 'sd', 'se', and 'ds' as columns
leave1out <- function(data, model){
  final_df <- data.frame()
  
  # Loop through each observation in the dataset
  for(idx in 1:nrow(data)){
    # Split the data into training and test sets
    train_data <- data[-idx,]
    test_data <- data[idx, , drop = FALSE]
    
    # Predict the outcome for the left-out observation
    pred <- filament1_predict(train_data = train_data, test_data = test_data, model = model, lwr_upr_on=FALSE)
    
    # Calculate the score for the prediction
    score <- scoring_func(pred, test_data)
    
    final_df <- rbind(final_df, score)
  }
  
  return(final_df)
}

#' test_stats
#' 
#' @description Calculate mean difference between scores A and B
#' @param A_scores A vector of scores
#' @param B_scores A vector of scores
#' @return A vector with the mean difference between A_scores and B_scores
test_stats <- function(A_scores, B_scores){
  mean(A_scores - B_scores)
}


#' pvalue_estimator
#' 
#' @description Monte Carlo estimate of the p-value to test the exchangeability between model predictions
#' from A and B against the alternative hypothesis that B is better than A
#' @param Y_A A vector of scores
#' @param Y_B A vector of scores
#' @param test_function A function to calculate the test statistic
#' @param n_perm Number of permutations
#' @return A float representing the p-value for the hypothesis that B is better than A
pvalue_estimator <- function(Y_A, Y_B, test_function = test_stats, n_perm = 10000){
  n_A <- length(Y_A)
  n_B <- length(Y_B)
  test_stats_AB <- test_function(Y_A, Y_B)
  
  test_statistic_permuted <- rep(0, n_perm)
  
  for (loop in seq_len(n_perm)) {
    Y_AB_permuted <- sample(c(Y_A, Y_B),
                            size = n_A + n_B,
                            replace = FALSE)
    
    Y_A_permuted <- Y_AB_permuted[1:n_A]
    Y_B_permuted <- Y_AB_permuted[n_A + (1:n_B)]
    
    test_statistic_permuted[loop] <- test_function(Y_A_permuted, Y_B_permuted)
  }
  
  p_values <- mean(test_statistic_permuted > test_stats_AB)
  return(p_values)
}


### Part 2 ###

# y is a matrix, data with columns 'N' and 'phi'

#' arch_loglike
#' 
#' @description Evaluate the combined log-likelihood log[p(y|N, ϕ)] for a collection y of y-observations. 
#' If a data.frame with columns N and phi is provided, the log-likelihood for each
#' row-pair (N, ϕ) will be returned.
#' @param y A vector or matrix of one or multiple observations
#' @param N A value for N
#' @param phi A value for phi
#' @param data A data frame with length of number of observations and columns 'N' and 'phi' (instead of 'N' and 'phi' parameter)
#' @return A list of vectors with the log-likelihood
arch_loglike <- function(y, N = NULL, phi = NULL, data = NULL){
  
  # if data.frame data is provided, the log-likelihood 
  # for each row-pair (N, ϕ) will be returned
  if (!is.null(data)){
    N <- data$N
    phi <- data$phi
  }
  
  if(is.null(N) || is.null(phi)){
    stop("('N' and 'phi') or 'data' must be provided")
  }
  
  # check if a collection of y values or only one pair is provided
  if(is.vector(y)){
    y1 <- c(y[1])
    y2 <- c(y[2])
  } else{
    y1 <- y[,1]
    y2 <- y[,2]
  }
  
  all_values <- list()
  for (i in 1:length(y1)){
    term1 <- -lgamma(y1[i] + 1) -lgamma(y2[i] + 1) 
    term2 <- -lgamma(N - y1[i] + 1) -lgamma(N - y2[i] + 1) +2*lgamma(N+1)
    term3 <- (y1[i]+y2[i])*log(phi) +(2*N-y1[i]-y2[i])*log(1-phi)
    all_values[[i]] <- term1+term2+term3
  }
  
  return(all_values)
}

#' estimate
#' 
#' @description Monte Carlo integration method to approximate py(y), E(N|y), and E(ϕ|y)
#' @param y A vector or matrix of one or multiple observvations
#' @param xi Paramter of geometric distribution used for N values
#' @param a First parameter of beta distribution used for phi values
#' @param b Second parameter of beta distribution used for phi values
#' @param K Number of samples for Monte Carlo method
#' @return A list with three vectors containing the values:
#     py :  py(y)
#     ENy : E(N|y)
#     Ephiy : E(ϕ|y)
estimate <- function(y, xi, a, b, K){
  set.seed(12345L) # set.seed at the beginning of the report somehow did not work...
  N <- rgeom(K, xi)
  phi <- rbeta(K, a, b)
  
  # calculate the likelihood of y given N, phi
  all_log_prob_y <- arch_loglike(y, data=data.frame(N=N, phi=phi))
  
  pys <- c()
  ENys <- c()
  Ephiys <- c()
  for(i in 1:length(all_log_prob_y)){
    prob_y <- exp(all_log_prob_y[[i]])
    py <- mean(prob_y)
    pys[i] <- py
    ENys[i] <- mean(prob_y * N / py)
    Ephiys[i] <- mean(prob_y * phi / py)
  }
  return(list(py=pys, ENy=ENys, Ephiy=Ephiys))
}

### helper functions ###

#' create_table
#' 
#' @description Create stylish knitr::kable
#' @param data Dataframe to display
#' @param caption String with caption of table
#' @param digits Number of digits that shall be displayed for numerical values
#' @param index_col If TRUE, the content of the cells of the first column is printed in bold
#' @return styled knitr::kable
create_table <- function(data, caption, digits=4, index_col=F){
  # Only round the numerical columns in the data frame
  rounded_data <- data %>%
    mutate_if(is.numeric, round, digits = digits)
  
  # Use kable from knitr to create a basic table
  kable_output <- knitr::kable(rounded_data, caption = caption, booktabs = TRUE, align = 'c')
  
  # Use kableExtra to style the table
  styled_kable_output <- kable_output %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                  full_width = FALSE, position = "center") %>%
    row_spec(0, bold = TRUE, align="center", color = "white", background = "#4d4d4d")
  
  if (index_col){
    styled_kable_output <- styled_kable_output %>%
      column_spec(1, bold = TRUE) %>%
      column_spec(2:nrow(data), width = "2.5cm")
  } else {
    styled_kable_output <- styled_kable_output %>%
      column_spec(1:nrow(data), width = "2.5cm")
  }
  
  # Return the styled table
  return(styled_kable_output)
}
