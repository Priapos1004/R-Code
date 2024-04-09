z <- rep(1:10, times = 10)
data <- data.frame(z = z, y = 2 * z + rnorm(length(z), sd = 4))

### first way of plotting
# plot(data$z, data$y)

### second way of plotting
# install.packages("ggplot2")
# install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
theme_set(theme_bw())

# Now we can plot:
# ggplot(data) + geom_point(aes(z, y))


### modelling
newdata <- data.frame(z = -10:20)
formula <- y ~ z

plot_model <- function(formula, train_data, show_data, x_label = "z points", y_label = "y ~ x", xname = "z"){
  mod <- lm(formula, train_data)
  print(paste0(y_label, " r2 value: ", summary(mod)$r.squared))
  show_data_pred <- cbind(show_data, predict(mod, show_data, interval="prediction"))
  show_data_conf <- cbind(show_data, predict(mod, show_data, interval="confidence"))
  ggplot(train_data) +
    geom_point(aes_string(xname, "y")) +
    geom_line(aes_string(xname, "fit"), data = show_data_pred, colour = "red") +
    geom_ribbon(data = show_data_pred, aes_string(x = xname, ymin = "lwr", ymax = "upr"), alpha = 0.25) +
    geom_ribbon(data = show_data_conf, aes_string(x = xname, ymin = "lwr", ymax = "upr"), alpha = 0.25) +
    xlab(x_label) +
    ylab(y_label)
}

# plot_model(formula, data, newdata) +
#   geom_abline(intercept = 0, slope = 2, colour = "blue")
# 
# quadratic_formula <- y ~ 1 + z + I(z**2)
# plot_model(quadratic_formula, data, newdata) +
#   geom_abline(intercept = 0, slope = 2, colour = "blue")

polynomial_formulas <- list(y ~ z, y ~ 1 + z + I(z^2), y ~ 1 + z + I(z^2) + I(z^3), y ~ 1 + I(1.2^z))
y_labels <- list("y ~ z", "y ~ 1 + z + I(z^2)", "y ~ 1 + z + I(z^2) + I(z^3)", "y ~ 1 + I(1.2^z)")
plots <- list()
for (k in seq_along(polynomial_formulas)) {
  plots[[k]] <- plot_model(polynomial_formulas[[k]], data, newdata, y_label=y_labels[[k]]) +
    geom_abline(intercept = 0, slope = 2, colour = "blue")
}
grid.arrange(grobs = plots, ncol = 2)
