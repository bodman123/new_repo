# scratch file for hw 6 04/07/2025
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

# Figure 5.6, left panel -----
f_2_9 <- function(x) {
  -4.5 * ((x - 10) / 50)^3 + 9 * ((x - 10) / 50)^2 + 4.2
}

set.seed(1)
n_train <- 45
n_test <- 15
n <- n_train + n_test
var_epsilon <- 2

# The test data is only used to compute the true MSEP:
test_data <- 
  tibble(
    x = runif(n = n_test, min = 0, max = 100),
    true_f_x = f_2_9(x),
    y = true_f_x + rnorm(n_test, sd = sqrt(var_epsilon)),
    type = "te"
  )

# We will try to estimate the test data based on the training set:
train_data <- 
  tibble(
    x = runif(n = n_train, min = 0, max = 100),
    true_f_x = f_2_9(x),
    y = true_f_x + rnorm(n_train, sd = sqrt(var_epsilon)),
    type = "tr"
  )

df_seq <- seq(2, 25, 0.1)

# Compute true MSEP:
MSEP_true <- tibble(df = rep(NA, length(df_seq)), True = NA)

for(i in 1:length(df_seq)) {
  MSEP_true$df[i] <- df_seq[i]
  smsp_2_9 <- smooth.spline(x = train_data$x, y = train_data$y, df = df_seq[i])
  
  pred_test <- predict(smsp_2_9, x = test_data$x)
  MSEP_true$True[i] <- mean((test_data$y - pred_test$y)^2)
} 

# Estimate with LOOCV:
MSEP_LOOCV <- tibble(i = rep(NA, n_train * length(df_seq)), df = NA, MSEP_i = NA)
j <- 1
for (i in 1:n_train) {
  for (df in df_seq) {
    smsp_2_9 <- smooth.spline(x = train_data[setdiff(1:n_train, i), ]$x, y =train_data[setdiff(1:n_train, i), ]$y, df = df)
    
    pred <- predict(smsp_2_9, x = train_data[i, ]$x)
    MSEP_LOOCV$MSEP_i[j] <- (train_data[i, ]$y - pred$y) ^ 2
    MSEP_LOOCV$i[j] <- i
    MSEP_LOOCV$df[j] <- df
    
    j <- j + 1
  }    
}

MSEP_LOOCV_summary <- 
  MSEP_LOOCV %>%
  group_by(df) %>%
  summarize(LOOCV = mean(MSEP_i)) 

# Estimate with k-fold:
k <- 10
k.o <- k
kvals <- unique(round(n_train/(1L:floor(n_train/2))))
temp <- abs(kvals - k)
if (!any(temp == 0))  { k <- kvals[temp == min(temp)][1L] }
f <- ceiling(n_train/k)

MSEP_k_fold <- tibble(i = rep(NA, k * length(df_seq)), df = NA, MSEP_i = NA)
j <- 1
train_data$fold <- sample(rep(1L:k, f), n_train)
# table(train_data$fold)
for (i in 1:k) {
  current_train_folds <- train_data %>% filter(fold != i)
  current_test_fold <- train_data %>% filter(fold == i)
  
  for (df in df_seq) {
    smsp_2_9 <- smooth.spline(x = current_train_folds$x, y = current_train_folds$y, df = df)
    pred <- predict(smsp_2_9, x = current_test_fold$x)
    
    MSEP_k_fold$MSEP_i[j] <- mean( (current_test_fold$y - pred$y) ^ 2 )
    MSEP_k_fold$i[j] <- i
    MSEP_k_fold$df[j] <- df
    
    j <- j + 1
  }    
}

MSEP_k_fold_summary <- 
  MSEP_k_fold %>%
  group_by(df) %>%
  summarize(k_fold = mean(MSEP_i)) 

MSEP_compare <-
  MSEP_true %>%
  left_join(MSEP_LOOCV_summary, by = "df") %>%
  left_join(MSEP_k_fold_summary, by = "df") %>%
  pivot_longer(cols = -df, names_to = "Type", values_to = "MSEP")

min_test_df <-
  MSEP_compare %>%
  group_by(Type) %>%
  filter(MSEP == min(MSEP))

ggplot(MSEP_compare) +
  geom_line(aes(x = df, y = MSEP, color = Type)) +
  theme_bw() +
  xlab("Flexibility") +
  ylab("Mean Squared Error") +
  geom_point(data = min_test_df, aes(x = df, y = MSEP, color = Type), shape = "X", size = 3.5)



## Bootstrap Modified::
set.seed(1)
data_set <- ISLR2::Default
#categorical: default (Y/N) & student(Y/N)
#numeric : balanec & income -- double
data_set <-
  data_set %>%
  mutate(response = ifelse(default=="Yes",1,0))

fit_model <- glm(response ~ income+balance,family = "binomial", data = data_set)
summary(fit_model)

# -- now to bootstrap
N <- nrow(data_set)
M <- 1000
coeff_estimates <- tibble(intercept = numeric(M), income = numeric(M), balance = numeric(M))
for (i in 1:M){
  
  m_Fit <- glm(response ~ income+balance,family = "binomial", data = data_set, subset = sample(N,N,replace = TRUE))
  
  coeff_estimates[i,] = t(as.numeric(m_Fit$coefficients))
  
  
}

(standard_errors <-
  coeff_estimates %>%
  summarise(intercept = sd(intercept), income = sd(income), balance = sd(balance)))


#####--- HW 7::::

## Problem 1::

set.seed(30) 

n <- 1000
p <- 20
X <- matrix(rnorm(n*p), nrow = n, ncol = p) 
B_0 <-  5.1
beta <- c(2, -3, 0, 4, 0, 0, -2, 0, 0, 0, 1.5, 0, 0, 0, 0, 0, 3, 0, 0, 0)
e <- rnorm(n)
Y <- B_0 + X %*% beta + e
df <- data.frame(Y = Y, X)

#split
train_indices <- sample(1:n, 100)
df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

#best subset
nvmax <- p
best_sub <- regsubsets(Y ~ ., data = df_train, nbest = 1, really.big = TRUE, nvmax = nvmax)

fit_full <- lm(Y~., data=df_train)

summary(best_sub)


summary(fit_full)


# Initializing vectors for MSE storage
train_mse <- numeric(nvmax)
test_mse <- numeric(nvmax)

for (i in 1:nvmax) {
  # Identifying predictors included in the model
  predictors <- names(coef(best_sub, id = i))
  
  model_formula <- as.formula(paste("Y ~", paste(predictors[-1], collapse = " + ")))
  model_train <- lm(model_formula, data = df_train)
  
  train_pred <- predict(model_train, newdata = df_train)
  test_pred <- predict(model_train, newdata = df_test)
  
  train_mse[i] <- mean((df_train$Y - train_pred)^2)
  test_mse[i] <- mean((df_test$Y - test_pred)^2)
}

mse_data <- data.frame(ModelSize = 1:nvmax, TrainMSE = train_mse, TestMSE = test_mse)
mse_data_long <- tidyr::pivot_longer(mse_data, cols = c("TrainMSE", "TestMSE"), names_to = "Set", values_to = "MSE")

ggplot(mse_data_long, aes(x = ModelSize, y = MSE, color = Set)) +
  geom_line() +
  geom_point() +
  labs(title = "MSE vs. Model Size", x = "Model Size", y = "MSE", color = "Dataset")


cat("the number of predictors for the model with the lowest training mse is ", which.min(mse_data$TrainMSE))


cat("\nthe number of predictors for the model with the lowest test mse is ", which.min(mse_data$TestMSE))



beta_distance <- numeric(p)

# Calculate distance for each value of r
for (r in 1:p) {
  # Extract coefficient vector of the best model with r predictors
  suppressWarnings( {coef_r <- coef(best_sub, id = r)
  beta_distance[r] <- sqrt(sum((coef(fit_full)[-1] - coef_r[-1])^2))  })
}

# Plotting
plot(1:p, beta_distance, type = "l", xlab = "Number of Predictors (r)", ylab = "Distance from Full Model Coefficients",
     main = "Distance from Full Model Coefficients vs. Number of Predictors (r)")








#Credit Data::
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(leaps)
library(glmnet)

set.seed(1)
Credit <- ISLR2::Credit
n <- nrow(Credit)
x <- scale(model.matrix(Balance ~ ., data = Credit)[ , -1])
y <- Credit$Balance

set.seed(1)
n_train <- 0.8 * n
n_test <- n - n_train
train <- rep(FALSE, n)
train[sample(1:n, n_train, replace = FALSE)] <- TRUE
test <- (!train)

x_train <- x[train, ]
y_train <- y[train]
x_test <- x[test, ]
y_test <- y[test]

scaled_credit_df_train <- bind_cols(Balance = y_train, x_train)
scaled_credit_df_test <- bind_cols(Balance = y_test, x_test)

ncol(x_train)
#a...

lambda_grid <- 10 ^ seq(10, -2, length = 100)
ridge_reg <- cv.glmnet(x_train, y_train, lambda = lambda_grid, nfolds = ncol(x_train),alpha=0) 

#b...
(coeff_ridge <- coef(ridge_reg,s="lambda.min"))

ols_model <- lm(Balance ~ .,data = scaled_credit_df_train)
(coeff_ols <- coef(ols_model))

plot(coeff_ridge,coeff_ols)

plot(ridge_reg, xvar = "lambda")

lambda_v <- ridge_reg$lambda.min
#c...



# -- now to bootstrap
N <- nrow(x_test)
M <- 1000
coeff_matrix <- matrix(NA, nrow = M, ncol = ncol(x_train) + 1)
for (i in 1:M){
  boot_indices <- sample(1:N, size = N, replace = TRUE)
  x_boot <- x_train[boot_indices, ]
  y_boot <- y_train[boot_indices]
  
  fit <- glmnet(x_boot, y_boot, alpha = 0, lambda = lambda_v, standardize = TRUE)
  coeffs <- as.numeric(coef(fit))  
  coeff_matrix[i, ] <- coeffs
  }

(ridge_se <- apply(coeff_matrix, 2, sd))





coeff_matrixOLS <- matrix(NA, nrow = M, ncol = ncol(x_train) + 1)
for (i in 1:M){
  boot_indices <- sample(1:N, N, replace = TRUE)
  m_Fit <- lm(Balance ~ ., data = scaled_credit_df_train[boot_indices, ])
  
  # Store the coefficients (including intercept)
  coeff_matrixOLS[i, ] <- as.numeric(m_Fit$coefficients)
}
ols_se <- apply(coeff_matrixOLS, 2, sd)

plot(ridge_se,ols_se)



### prediction:: d)
ridge_MSE <- matrix(NA, nrow = M, ncol = 1)
for (i in 1:M) {
  
  boot_indices <- sample(1:N, N, replace = TRUE) 
  x_boot <- x_test[boot_indices, ]
  y_boot <- y_test[boot_indices]
  
  
  ridge_predictions_direct <- predict(ridge_reg, newx = x_boot, s = "lambda.min")
  ridge_MSE[i, ] <- mean((y_boot - ridge_predictions_direct)^2)
}
ridge_CI <- quantile(ridge_MSE, c(0.025, 0.975))




# Initialize a matrix to store MSE values for OLS regression
ols_MSE <- matrix(NA, nrow = M, ncol = 1)
for (i in 1:M) {
  boot_indices <- sample(1:N, N, replace = TRUE)  
  x_boot <- scaled_credit_df_test[boot_indices, ]  
  y_boot <- y_test[boot_indices]
  
  ols_predictions_direct <- predict(ols_model, newdata = as.data.frame(x_boot))
  
  ols_MSE[i, ] <- mean((y_boot - ols_predictions_direct)^2)
}

ols_CI <- quantile(ols_MSE, c(0.025, 0.975))



# e)
ridgekf_reg <- cv.glmnet(x_train, y_train, lambda = lambda_grid, nfolds = 10,alpha=0) 

(coeff_ridgekf <- coef(ridgekf_reg,s="lambda.min"))

lambda_vkf <- ridgekf_reg$lambda.min

coeff_matrixkf <- matrix(NA, nrow = M, ncol = ncol(x_train) + 1)
for (i in 1:M){
  boot_indices <- sample(1:N, size = N, replace = TRUE)
  x_boot <- x_train[boot_indices, ]
  y_boot <- y_train[boot_indices]
  
  fit <- glmnet(x_boot, y_boot, alpha = 0, lambda = lambda_vkf, standardize = TRUE)
  coeffs <- as.numeric(coef(fit))  
  coeff_matrixkf[i, ] <- coeffs
}

(ridge_sekf <- apply(coeff_matrixkf, 2, sd))



ridgekf_MSE <- matrix(NA, nrow = M, ncol = 1)
for (i in 1:M) {
  
  boot_indices <- sample(1:N, N, replace = TRUE) 
  x_boot <- x_test[boot_indices, ]
  y_boot <- y_test[boot_indices]
  
  
  ridge_predictions_direct <- predict(ridgekf_reg, newx = x_boot, s = "lambda.min")
  ridgekf_MSE[i, ] <- mean((y_boot - ridge_predictions_direct)^2)
}
ridgekf_CI <- quantile(ridgekf_MSE, c(0.025, 0.975))
