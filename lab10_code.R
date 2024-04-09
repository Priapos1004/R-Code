library(tidyverse)

load("data/filament1.rda")

lm_prediction <- function(data, model, newdata) {
  if (model == "A") {
    fit0 <- lm(Actual_Weight ~ 1 + CAD_Weight, data = data)
  } else {
    fit0 <- lm(Actual_Weight ~ 1 + CAD_Weight + Material, data = data)
  }
  pred0 <- predict(fit0, newdata = newdata, se.fit = TRUE,
                   interval = "prediction", level = 0.95)
  mean = pred0$fit[, "fit"]
  sd <- sqrt(pred0$se.fit**2 + summary(fit0)$sigma**2)
  q <- qt(1 - 0.05/2, df = Inf)
  lwr <- mean - q * sd
  upr <- mean + q * sd
  data.frame(mean = mean, sd = sd,
             lwr = lwr, upr = upr)
}

pred_A <- lm_prediction(data = filament1, model = "A", newdata = filament1)
pred_B <- lm_prediction(data = filament1, model = "B", newdata = filament1)
score_A <- cbind(pred_A, filament1) %>%
  mutate(
    se = (Actual_Weight - mean)**2,
    ds = (Actual_Weight - mean)**2/sd**2 + 2 * log(sd)
  )
score_B <- cbind(pred_B, filament1) %>%
  mutate(
    se = mean((Actual_Weight - mean)**2),
    ds = (Actual_Weight - mean)**2/sd**2 + 2 * log(sd)
  )

### leave-one-out-cv

ovso_cv <- function(data, model){
  total_score <- list("se"=c(), "ds"=c())
  for(idx in 1:nrow(data)){
    train_data <- data[-idx,]
    test_data <- data[idx, ]
    pred <- lm_prediction(data = train_data, model = model, newdata = test_data)
    score <- cbind(pred, test_data) %>%
      mutate(
        se = mean((Actual_Weight - mean)**2),
        ds = (Actual_Weight - mean)**2/sd**2 + 2 * log(sd)
      )
    total_score$se[idx] <- score$se
    total_score$ds[idx] <- score$ds
  }
  
  return(total_score)
}

scores_A <- ovso_cv(filament1, "A")
names(scores_A) <- c("se_A", "ds_A")
scores_B <- ovso_cv(filament1, "B")
names(scores_B) <- c("se_B", "ds_B")

total_data <- cbind(filament1, scores_A, scores_B)

ggplot(total_data) +
  geom_point(aes(x=CAD_Weight, y=se_A), color="red") +
  geom_point(aes(x=CAD_Weight, y=se_B))

test_stats <- function(A_scores, B_scores){
  mean(A_scores - B_scores)
}

pvalue_estimator <- function(Y_A, Y_B, test_function, n_perm = 10000){
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

pvalue_estimator(scores_A$se, scores_B$se, test_function = test_stats)
pvalue_estimator(scores_A$ds, scores_B$ds, test_function = test_stats)
