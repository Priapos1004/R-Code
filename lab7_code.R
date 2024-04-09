suppressPackageStartupMessages(library(tidyverse))

load("data/ghcnd_stations.Rdata")
load("data/ghcnd_values.Rdata")

ghcnd <- left_join(ghcnd_values, ghcnd_stations, by = "ID")
head(ghcnd)


data_raw <- ghcnd %>%
  filter(Element == "TMIN", Name == "BRAEMAR", 
         !is.na(Element), !is.na(Name)) %>%
  group_by(Year) %>%
  summarise(year_avg = mean(Value)) %>%
  ungroup()
names(data_raw) <- c("x", "y")
head(data_raw)


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


modelling <- function(data_raw, element_value = "TMIN", station_name = "BRAEMAR"){
  
  data <- data_raw %>%
    filter(Element == element_value, Name == station_name, 
           !is.na(Element), !is.na(Name)) %>%
    group_by(Year) %>%
    summarise(year_avg = mean(Value)) %>%
    ungroup()
  names(data) <- c("x", "y")
  
  set.seed(42)
  
  # Calculate the size of the dataset
  n <- nrow(data)
  
  # Determine the size of the training set (e.g., 80% of the dataset)
  train_size <- floor(0.8 * n)
  
  # Generate a random sample of indices for the training data
  train_indices <- sample(seq_len(n), size = train_size)
  
  # Split the data into training and testing sets
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
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
    Flist[[ord+1]] <- fit_poly_regresssion(data = train_data, 
                                           order = ord)
  }
  
  # Compute the prediction distribution summaries
  prediction_estimates <- NULL
  for(modID in seq_along(Flist)){
    pred <- predict_poly_regression(data = test_data,
                                    model = Flist[[modID]])
    pred$order <- paste0("F",modID - 1)
    prediction_estimates <- rbind(prediction_estimates, pred)
  }
  
  all_scores <- compute_scores(pred = prediction_estimates, alpha = 0.2)
  
  result_all_data <- all_scores %>% 
    pivot_longer(cols = c("se","ds","int"), 
                 names_to = "score_type", 
                 values_to = "score") %>% 
    group_by(score_type, order) %>%
    summarise(avg_score = mean(score), 
              std_err = sd(score)/sqrt(n()),
              .groups = "drop") %>%
    pivot_wider(values_from = c(avg_score,std_err), names_from = score_type)
  
  x_seq <- seq(min(data$x), max(data$x), by = 1)
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
  
  return(result_all_data)
}

data_results <- modelling(ghcnd)

###### cross validation ######

modelling_k_fold <- function(data_raw, element_value = "TMIN", station_name = "BRAEMAR", k = 5){
  data <- data_raw %>%
    filter(Element == element_value, Name == station_name, 
           !is.na(Element), !is.na(Name)) %>%
    group_by(Year) %>%
    summarise(year_avg = mean(Value)) %>%
    ungroup()
  names(data) <- c("x", "y")
  
  # Calculate the size of the dataset
  n <- nrow(data)
  
  # Determine the size of the training set (e.g., 80% of the dataset)
  train_size <- floor(0.8 * n)
  
  
  all_fold_results <- list()
  for(i in 1:k){
    set.seed(42 + i)
    # Generate a random sample of indices for the training data
    train_indices <- sample(seq_len(n), size = train_size)
    
    # Split the data into training and testing sets
    train_data <- data[train_indices, ]
    test_data <- data[-train_indices, ]
    
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
      Flist[[ord+1]] <- fit_poly_regresssion(data = train_data, 
                                             order = ord)
    }
    
    # Compute the prediction distribution summaries
    prediction_estimates <- NULL
    for(modID in seq_along(Flist)){
      pred <- predict_poly_regression(data = test_data,
                                      model = Flist[[modID]])
      pred$order <- paste0("F",modID - 1)
      prediction_estimates <- rbind(prediction_estimates, pred)
    }
    
    all_scores <- compute_scores(pred = prediction_estimates, alpha = 0.2)
    
    result_all_data <- all_scores %>% 
      pivot_longer(cols = c("se","ds","int"), 
                   names_to = "score_type", 
                   values_to = "score") %>% 
      group_by(score_type, order) %>%
      summarise(avg_score = mean(score), 
                std_err = sd(score)/sqrt(n()),
                .groups = "drop") %>%
      pivot_wider(values_from = c(avg_score,std_err), names_from = score_type)
    
    all_fold_results[[i]] <- result_all_data
  }
  
  avg_fold_scores <- bind_rows(all_fold_results) %>%
    group_by(order) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

  return(avg_fold_scores)
}

# Assuming `ghcnd` is your dataset
data_results_k_fold <- modelling_k_fold(ghcnd)
head(data_results)
head(data_results_k_fold)
