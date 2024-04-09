## Load Libraries
library(tidyverse)
theme_set(theme_bw())

##  Functions  ##
#################


# Description:
#   Simulate data for a polynomial regression model 
#   Returns data frame with covariate x and response variable y
#   Explanatory variable generated uniformly on the interval [-3, 3]
# Params:
#   size  - integer, number of samples to generate
#   alpha - intercept parameter
#   betas - vector, coefficient parameter for powers of the explanatory variable
#   sigma - positive value, error standard error

simulate_poly_regression_data <- function(size, alpha = 0, betas = 0, sigma = 1){
  x <- runif(size, -3, 3)
  y <- rnorm(size, mean = alpha, sd = sigma)
  for(j in seq_along(betas)) y <- y + betas[j]*x^j
  df <- data.frame(y = y, x = x)
  return(df)
}


# Description:
#   Fit a polynomial regression model, eg, if order = 0 is the constant model
#   and order = 2 is a quadratic model.
# Params:
#   data  - data frame of response & covariate data
#   order - single non-negative integer defining the polynomial order 

fit_poly_regresssion <- function(data, order = 0){
  if(order == 0){
    #Intercept only
    model <- lm(y ~ 1, data = data)
  }else{
    model <- lm(y ~ poly(x, degree = order, raw = TRUE), data = data)
  }
  return(model)
}


# Description:
#   Calculate summary statistics of the prediction distribution. Returns a 
#   data frame containing the original data and the expectation (mean), 
#   standard deviation (sd) and interval (lwr & upr) estimates.
# Params:
#   data  - data frame of response & covariate data (ie new data, not used when fitting)
#   model - lm object containing a fitted (polynomial) model
#   alpha - significance level for evaluating the prediction interval.

predict_poly_regression <- function(data,
                                    model, 
                                    alpha = 0.05){
  predictions_output = data
  
  #Evaluate summaries of the prediction distribution
  prediction <- predict.lm(object = model, 
                           newdata = data, 
                           se.fit = TRUE, 
                           interval = "prediction", 
                           level = 1 - alpha)
  
  predictions_output$mean = prediction$fit[,1]
  predictions_output$lwr =  prediction$fit[,2]
  predictions_output$upr = prediction$fit[,3]
  predictions_output$sd <- sqrt(prediction$se.fit^2 + prediction$residual.scale^2)
  
  return(predictions_output)
}


# Description:
#   Calculate the squared-error, Dawid-Sebastiani and interval scores
# Params:
#   pred  - data frame returned from predict_poly_regression()
#   alpha - significance level for evaluating the prediction interval.
compute_scores <- function(pred, alpha = 0.05){
  se  <- (pred$y - pred$mean)^2
  ds  <- ((pred$y - pred$mean) / pred$sd)^2 + 2 * log(pred$sd)
  int <- pred$upr - pred$lwr + 2/alpha * ((pred$lwr - pred$y) * 
                                            (pred$y < pred$lwr) + (pred$y - pred$upr) * (pred$y > pred$upr))
  # Combine scores and data for output
  scores <- data.frame(se = se, ds = ds, int = int)
  output <- cbind(
    pred[ ,!(colnames(pred) %in% c("mean","sd","lwr","upr"))],  #ie, just the original data!
    scores)
  return(output)
}


##  Generate the data ##
########################

#True model (G):  Y ~ (4-x)*(3+x) + N(0, 3^2)
set.seed(1)
data <- simulate_poly_regression_data(size = 100, alpha = 0, 
                                      betas = c(12, 1, -1), sigma = 3)

ggplot(data) +
  geom_point(aes(x = x ,y = y), cex = 2) +
  geom_function(fun = function(x) {x*(4-x)*(3+x)})


## CASE 1: Same Train Data and Validate Data ##
###############################################

# MODELS:
#########
# F0 -- constant model    Y ~ b0 + error
# F1 -- linear model      Y ~ b0 + b1*x + error
# F2 -- quadratic model   Y ~ b0 + b1*x + b2*x^2 + error
# F3 -- cubic model       Y ~ b0 + b1*x + b2*x^2 + b3*x^3 + error
# F4 -- quartic model     Y ~ b0 + b1*x + b2*x^2 + b3*x^3 + b4*x^4 + error

# Fit the models
Flist <- list()
for(ord in 0:4){
  Flist[[ord+1]] <- fit_poly_regresssion(data = data, 
                                         order = ord)
}

# Compute the prediction distribution summaries
prediction_estimates <- NULL
for(modID in seq_along(Flist)){
  pred <- predict_poly_regression(data = data,    # <-- same as used when fitting
                                  model = Flist[[modID]])
  pred$order <- paste0("F",modID - 1)
  prediction_estimates <- rbind(prediction_estimates, pred)
}

# Compute the 3 scores per observation
all_scores <- compute_scores(pred = prediction_estimates)

# Compute mean score and standard error estimate per score & model
result_all_data <- all_scores %>% 
  pivot_longer(cols = c("se","ds","int"), 
               names_to = "score_type", 
               values_to = "score") %>% 
  group_by(score_type, order) %>%
  summarise(avg_score = mean(score), 
            std_err = sd(score)/sqrt(n()),
            .groups = "drop") %>%
  pivot_wider(values_from = c(avg_score,std_err), names_from = score_type)

result_all_data

## VISUALISE THE FITTED MODELS ##
#################################

x_seq <- seq(-5, 5, by = 0.1)
plot_data <- data.frame(x = x_seq, y = rep(0, length(x_seq)))
pred_plot <- NULL
for(modID in seq_along(Flist)){
  pred <- predict_poly_regression(plot_data, Flist[[modID]])
  pred$order <- paste0("F",modID - 1)
  pred_plot <- rbind(pred_plot, pred)
}

ggplot(pred_plot) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr, fill = order), alpha = 0.5) + 
  geom_line(aes(x = x, y = mean, colour = order)) + 
  geom_point(data = data, aes(x = x ,y = y)) +
  facet_wrap( ~ order)



