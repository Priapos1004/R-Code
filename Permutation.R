## Week 10

# Load Libraries
library(tidyverse)
library(ggplot2)

## Permutation test

## Example: Overdispersed Poisson ##
####################################

set.seed(12345L)
n_A <- 30
n_B <- 28
Y_A <- rpois(n_A, lambda = 2)
Y_B <- rpois(n_B, lambda = 2 * exp(-1/2 + rnorm(n_B, sd = 1)))

p1 <- hist(Y_A)                    
p2 <- hist(Y_B)               
plot(p1, col=rgb(0,0,1,1/4), xlim=c(0,20), ylim=c(0,25), main="", xlab="value")  
abline(v = mean(Y_A), lwd = 2)
plot( p2, col=rgb(1,0,0,1/4), add=T) 
abline(v = mean(Y_B), lwd = 2, col=2)

test_statistic <- c(abs(mean(Y_B) - mean(Y_A)),
                    var(Y_B) / var(Y_A))
n_perm <- 10000
test_statistic_permuted <- matrix(0, n_perm, 2)

for (loop in seq_len(n_perm)) {
  Y_AB_permuted <- sample(c(Y_A, Y_B),
                          size = n_A + n_B,
                          replace = FALSE)
  
  Y_A_permuted <- Y_AB_permuted[1:n_A]
  Y_B_permuted <- Y_AB_permuted[n_A + (1:n_B)]
  
  test_statistic_permuted[loop,] <-
    c(abs(mean(Y_B_permuted) - mean(Y_A_permuted)),
      var(Y_B_permuted) / var(Y_A_permuted))
  
}

plot.ecdf(test_statistic_permuted[,1])
abline(v = test_statistic[1])
plot.ecdf(test_statistic_permuted[,2])
abline(v = test_statistic[2])
p_values <- c(Expectation_p_value = mean(test_statistic_permuted[,1] > test_statistic[1]),
              Variance_p_value = mean(test_statistic_permuted[,2] > test_statistic[2]))
print(p_values)


## Example: Small sample size ##
###################################

# Data
data_EG1 <- data.frame(
  values = c(14.2, 16.8, 17.3, 22.8, 20.9, 21.1, 24.5),
  group = c("x", "x", "x", "x", "y", "y", "y")
)

test <- t.test(values ~ group, data = data_EG1)
test

# Function: computes the test statistic t = xbar - ybar
test_stat <- function(values, group, perm = NULL, statistic_fn = mean){
  if(!is.null(perm)){
    values <- c(values[perm], values[-perm])
  }
  xbar = statistic_fn(values[group == "x"]) 
  ybar = statistic_fn(values[group == "y"])
  return(xbar - ybar)
}

# Compute the observed test statistic for data
t_stat <- test_stat(values = data_EG1$values, 
                    group = data_EG1$group)
t_stat   

n  <- nrow(data_EG1)
nx <- sum(data_EG1$group == "x")
all_permutations <- combn(n, nx)
all_permutations

# Compute the test statistic for each permutation
t_null <- numeric(ncol(all_permutations))
for(i in seq_along(t_null)){
  t_null[i] <- test_stat(values = data_EG1$values, 
                         group = data_EG1$group, 
                         perm = all_permutations[,i])
}

pvalue = 2*min(mean(t_null >= t_stat) , mean(t_null <= t_stat))

ggplot() +
  geom_histogram(aes(x = t_null, y = ..density..), 
                 fill = "blue", alpha = 0.5, bins = 30) + 
  geom_vline(xintercept = t_stat, size = 1.2) + 
  geom_function(fun = function(x){dt(x/test$stderr, 
                                     df = test$parameter)/test$stderr}, 
                colour = "red", size = 1.2, 
                xlim=c(-1.1,1.1)*max(abs(t_null))) +
  geom_point(aes(x = quantile(t_null, prob=c(0.05,0.95)), y = c(-0.01,-0.01)), 
             size = 5, col = "blue", size = 2, pch = 3) + 
  geom_point(aes(x = qt(c(0.05,0.95), df = test$parameter)*test$stderr, 
                 y = c(-0.01, -0.01)), size = 5, col = "red", size = 2, pch = 3)


ggplot() + 
  geom_function(fun = function(x){pt(x/test$stderr, df = test$parameter)}, 
                colour = "red", size = 1.2) +
  geom_vline(xintercept  = t_stat, lty = 5, size = 1.2) +   
  stat_ecdf(aes(x = t_null), colour = "blue", size = 1.2) +
  geom_hline(yintercept  = c(0.05, 0.95), lty = 3, size = 1.2) 



