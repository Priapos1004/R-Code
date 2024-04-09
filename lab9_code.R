library(tidyverse)

load("data/filament1.rda")

obs_data <- filament1 %>%  
  dplyr::select(c("CAD_Weight", "Material", "Actual_Weight"))
names(obs_data) <- c("x1", "x2", "y")
obs_data <- obs_data %>%
  mutate(id = seq_along(y))

fitA = lm(y ~ x1, data = obs_data)
fitB = lm(y ~ x1 + x2, data = obs_data)

# Examine results:
summary(fitA)
summary(fitB)
anova(fitB, fitA)

pred_df_func <- function(fit, test_data){
  pred <- predict(fit, newdata = test_data, se.fit = TRUE, interval = "prediction", level = 0.95)
  pred_stderr <- sqrt(pred$se.fit^2 + summary(fit)$sigma^2)
  pred_df <- cbind(test_data, pred$fit, se.pred = pred_stderr, sd = summary(fit)$sigma) 
  return(pred_df)
}

vis_pred <- function(pred1_df, pred2_df, test_data){
  ggplot() + 
    geom_ribbon(data = pred1_df, mapping = aes(x = x1, y = fit, ymin = lwr, ymax = upr, fill = "A: no x2"), 
                alpha = 0.3) +
    geom_ribbon(data = pred2_df, mapping = aes(x = x1, y = fit, ymin = lwr, ymax = upr, fill = "B: /w x2"), alpha = 0.3) +
    geom_line(data = pred1_df, mapping = aes(x = x1, y = fit, colour = "A: no x2"),lwd = 2) + 
    geom_line(data = pred2_df, mapping = aes(x = x1, y = fit, colour = "B: /w x2"), lwd = 2, lty = "dashed") +
    geom_point(data = test_data, mapping = aes(x = x1, y = y, colour = "Test"), cex = 2, colour = "red") 
  
}

predA_df <- pred_df_func(fitA, obs_data)
predB_df <- pred_df_func(fitB, obs_data)

vis_pred(predA_df, predB_df, obs_data)

S_sqerr_A <- (predA_df$y - predA_df$fit)^2
S_sqerr_B <- (predB_df$y - predB_df$fit)^2

DS_sqerr_A <- (predA_df$y - predA_df$fit)^2 / predA_df$sd^2
DS_sqerr_B <- (predB_df$y - predB_df$fit)^2 / predB_df$sd^2

S_sqerr_DF <- data.frame(S_sqerr = c(S_sqerr_A, S_sqerr_B),
                         DS_sqerr = c(DS_sqerr_A, DS_sqerr_B),
                         ID = c(predA_df$id, predB_df$id),
                         Model = c(rep("A",length(S_sqerr_A)), 
                                   rep("B",length(S_sqerr_B))
                         ))

knitr::kable(S_sqerr_DF %>% group_by(Model) %>% summarise(mean_S = mean(S_sqerr), mean_DS = mean(DS_sqerr)))

### train-test-split ###

set.seed(123)
indices <- sample(1:nrow(obs_data), size = 0.5 * nrow(obs_data))
train_data <- obs_data[indices, ]
test_data <- obs_data[-indices, ]

fitA2 = lm(y ~ x1, data = train_data)
fitB2 = lm(y ~ x1 + x2, data = train_data)

predA2_df <- pred_df_func(fitA2, test_data)
predB2_df <- pred_df_func(fitB2, test_data)

S_sqerr_A2 <- (predA2_df$y - predA2_df$fit)^2
S_sqerr_B2 <- (predB2_df$y - predB2_df$fit)^2

DS_sqerr_A2 <- (predA2_df$y - predA2_df$fit)^2 / predA2_df$sd^2 # wrong score!!!! Missing log
DS_sqerr_B2 <- (predB2_df$y - predB2_df$fit)^2 / predB2_df$sd^2

S_sqerr_DF2 <- data.frame(S_sqerr = c(S_sqerr_A2, S_sqerr_B2),
                         DS_sqerr = c(DS_sqerr_A2, DS_sqerr_B2),
                         ID = c(predA2_df$id, predB2_df$id),
                         Model = c(rep("A",length(S_sqerr_A2)), 
                                   rep("B",length(S_sqerr_B2))
                         ))

knitr::kable(S_sqerr_DF2 %>% group_by(Model) %>% summarise(mean_S = mean(S_sqerr), mean_DS = mean(DS_sqerr)))
