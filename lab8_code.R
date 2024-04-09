vec_norm <- function(x) {
  sum(x**2)**0.5
}

c(vec_norm(c(1, 2, 3)), sqrt(1 + 4 + 9))

### Test 1 ###

n <- 100000
m <- 100
b <- rnorm(n)
A <- matrix(rnorm(n * m), n, m)

bm_results1 <- bench::mark(t(A)%*%b, t(t(b) %*% A))

bench_table <- function(bm) {
  knitr::kable(
    bm %>%
      dplyr::select(expression, median, `itr/sec`, mem_alloc)
  )
}

bench_table(bm_results1)

### Test 2 ###

m <- 1000
n <- 1000
p <- 1000
A <- matrix(rnorm(m * n), m, n)
B <- matrix(rnorm(n * p), n, p)
b <- rnorm(p)

bm_results2 <- bench::mark(A %*% B %*% b, (A %*% B) %*% b, A %*% (B %*% b))
bench_table(bm_results2)

### Test 3 ###

bm_results3 <- bench::mark(solve(A) %*% b, solve(A,b), check = F) # not equal results
bench_table(bm_results3)

range(solve(A) %*% b, solve(A,b))

### Test 4 ###
## Set the "seed" for the random number sequences, so we can
## reproduce exactly the same random numbers every time we run the code
set.seed(1)
## Simulate the data
# x: 100 random numbers between 100 and 101
data <- data.frame(x = 100 + sort(runif(100)))

beta_true <- c(10080.2, -200.8, 1)
# y: random variation around a quadratic function:
data <- data %>%
  mutate(y_synt = (x - 100.5) / 5 + (x - 100.5)**2 + rnorm(n(), sd = 0.1)) %>%
  mutate(y_true = beta_true[1]+x * beta_true[2]+x**2 * beta_true[3])

model <- lm(y_synt ~ poly(x, degree = 2, raw = TRUE), data = data)

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

pred_results <- predict_poly_regression(data, model)

ggplot(pred_results) + 
  geom_point(aes(x, y_synt, colour = "synt")) +
  geom_line(aes(x, y_true, colour = "true")) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), alpha = 0.25) + 
  geom_line(aes(x = x, y = mean, colour = "estimate"))

# normal equations

X <- model.matrix(y_synt ~ x + I(x^2), data=data)
# beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y # error because X is singular
# names(beta_hat) <- c("(Intercept)", "x", "I(x^2)")
# beta_hat

### Test 5 ###

data2 <- data %>%
  mutate(x = x + 1000)

# model2 <- lm(y_synt ~ poly(x, degree = 2, raw = TRUE), data = data2, singular.ok = F) # singular model fails
# pred_results <- predict_poly_regression(data2, model2)
X2 <- model.matrix(y_synt ~ poly(x, degree = 2, raw = TRUE), data = data2)

cond_nr <- function(X){
  svd_result <- svd(X)
  singular_values <- svd_result$d
  max(singular_values) / min(singular_values)
}

cond_nr(X)
cond_nr(X2)

cor(X2[, 2:3])
ggplot() +
  geom_point(aes(X2[, 2], X2[, 3])) +
  ggtitle(paste("Correlation =", cor(X2[,2], X2[,3])))
## Very close to co-linear

X3 <- cbind(X2[,1], (X2[,2]-mean(X2[,2]))/sd(X2[,2]), (X2[,3]-mean(X2[,3]))/sd(X2[,3]))
data3 <- cbind(data2, X3)
names(data3) = c("x", "y_synt", "y_true", "x1", "x2", "x3")

cond_nr(X3)

model3 <- lm(y_synt ~ x1 + x2 + x3 - 1, data = data3)
pred_results3 <- predict_poly_regression(data3, model3)

ggplot(pred_results3) + 
  geom_point(aes(x, y_synt, colour = "synt")) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), alpha = 0.25) + 
  geom_line(aes(x = x, y = mean, colour = "estimate"))

ggplot() +
  geom_point(aes(X3[, 2], X3[, 3])) +
  ggtitle(paste("Correlation =", cor(X3[,2], X3[,3])))

### Test 6 ###

data4 <- data2 %>%
  mutate(x_shifted = x - mean(x))
X4 <- model.matrix(y_synt ~ poly(x_shifted, degree = 2, raw = TRUE), data = data4)
cond_nr(X4)
