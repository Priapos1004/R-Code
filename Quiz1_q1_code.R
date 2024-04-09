# Load necessary libraries
library(dplyr)

# Set seed for reproducibility
set.seed(0)

# Generating 'obs' DataFrame
# Assuming time in hours and temperature in degrees Celsius
time_obs <- 1:10  # Time points from 1 to 10
temperature_obs <- rnorm(length(time_obs), mean = 20, sd = 5)  # Random temperatures around 20 degrees

# Creating 'obs' DataFrame
obs <- data.frame(time = time_obs, temperature = temperature_obs)

# Generating 'pred' DataFrame
# New time points for prediction, from 11 to 15
time_pred <- 11:15

# Creating 'pred' DataFrame
pred <- data.frame(time = time_pred)

# Display the data frames
print(obs)
print(pred)

fit <- lm(obs$temperature ~ obs$time)
predict(fit, pred) # 10 rows -> :(

fit <- lm(temperature ~ time, data = obs)
predict(fit, data = pred) # 10 rows -> :(

fit <- lm(temperature ~ time, data = obs)
predict(fit, pred) # 5 rows -> :)