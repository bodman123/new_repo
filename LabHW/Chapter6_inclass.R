library(tidyverse)

Credit <- ISLR2::Credit

# The `Balance` variable is our response.

# How many predictors?
(p <- ncol(Credit %>% select(-Balance)))


# Best subset selection -----

# The `regsubsets()` function (part of the `leaps` library) performs best subset 
# selection by identifying the best model that contains a given number of predictors, 
# where *best* is quantified using RSS. The syntax is the same as for `lm()`. 
# The `summary()` command outputs the best set of variables for each model size.

library(leaps)
regfit_full <- regsubsets(Balance ~ ., Credit)
summary(regfit_full)

# An asterisk indicates that a given variable is included in the corresponding model.
# For instance, this output indicates that the best two-variable model contains only `Income` and `Rating`.

# But wait, we have 11 predictors!! Categorical variables with K>2 levels appear as K-1 predictors.
# In addition, `regsubsets()` only reports by default results up to the best eight-variable model.
# The `nvmax` option can be used in order to return as many variables as are desired. 11 is the maximal
# number, but we can choose a larger one without any effect.
regfit_full <- regsubsets(Balance ~ ., Credit, nvmax = 11)
summary(regfit_full)

regfit_full <- regsubsets(Balance ~ ., Credit, nvmax = 20)
summary(regfit_full)

# regsubsets does not return the full list of models and their R^2 scores (only the best for each number of 
# predictors).

# Manual implementation:
Credit <- ISLR2::Credit

Credit_dummified <-
  Credit %>%
  tidytable::get_dummies(drop_first = TRUE) %>%
  select(where(is.numeric))

response <- "Balance"
predictors <- Credit_dummified %>% select(-Balance) %>% colnames()
p <- length(predictors)

# Create list of models
nvmax <- 11
m <- NULL
for (i in 1:nvmax) {
  m <- 
    rbind(
      m, 
      t(apply(combn(1:nvmax, i), 2, function(vars) { r <- rep(FALSE, p); r[vars] <- TRUE; r })))
}
stopifnot(nrow(m) == 2^nvmax - 1)
colnames(m) <- predictors
n_models <- 2^nvmax - 1

Rsq_vec <- numeric(n_models)
RSS_vec <- numeric(n_models)
predictors_vec <- character(n_models)
for (i in 1:n_models) {
  rhs <- str_c(predictors[m[i, ]], collapse = " + ")
  predictors_vec[i] <- rhs
  f <- str_c(response, " ~ ", rhs)
  summary_lm_fit <- summary(lm(formula = f, data = Credit_dummified))
  RSS_vec[i] <- sum( resid(summary_lm_fit)^2 )
  Rsq_vec[i] <- summary_lm_fit$r.squared
}

best_subset_full <-
  as_tibble(m) %>%
  mutate(
    n_predictors = rowSums(.),
    predictors = predictors_vec,
    Rsq = Rsq_vec,
    RSS = RSS_vec
  ) %>%
  group_by(n_predictors) %>%
  mutate(
    max_Rsq = (Rsq == max(Rsq)),
    min_RSS = (RSS == min(RSS))
  )

g_RSS <-
  ggplot(best_subset_full, aes(x = n_predictors, y = RSS)) +
  geom_point(aes(text = predictors), color = "blue", alpha = 0.3) +
  geom_line(data = best_subset_full %>% filter(min_RSS), color = "red") +
  geom_point(data = best_subset_full %>% filter(min_RSS), color = "red") +
  theme_bw() +
  xlab("Number of predictors") +
  ylab("RSS")

library(plotly)
ggplotly(g_RSS)

g_Rsq <-
  ggplot(best_subset_full, aes(x = n_predictors, y = Rsq)) +
  geom_point(aes(text = predictors), color = "blue", alpha = 0.3) +
  geom_line(data = best_subset_full %>% filter(max_Rsq), color = "red") +
  geom_point(data = best_subset_full %>% filter(max_Rsq), aes(text = predictors), color = "red") +
  theme_bw() +
  xlab("Number of predictors") 

g_Rsq + ylab(expression(R^2))

ggplotly(g_Rsq)

# Forward stepwise selection -----
regfit_fwd <- regsubsets(Balance ~ ., data = Credit, nvmax = 11, method = "forward")
summary(regfit_fwd)

# Backward stepwise selection -----
regfit_bwd <- regsubsets(Balance ~ ., data = Credit, nvmax = 11, method = "backward")
summary(regfit_bwd)

# Selecting the optimal model -----
nvmax <- 11
regfit_full <- regsubsets(Balance ~ ., Credit, nvmax = nvmax)
summary_regfit <- summary(regfit_full)

n <- nrow(Credit)

# * Adjustment methods -----
adjustment_methods <- 
  tibble(
    n_predictors = 1:nvmax,
    adjr2 = summary_regfit$adjr2,
    cp = summary_regfit$cp,
    bic = summary_regfit$bic,
  ) %>%
  pivot_longer(-n_predictors, names_to = "Measure") %>%
  mutate(Measure = factor(Measure, levels = c("cp", "bic", "adjr2"), labels = c("C[p]", "BIC", "Adjusted~R^2")))

adjustment_methods_extreme <-
  tribble(
    ~n_predictors,                   ~Measure,  ~value,
    which.max(summary_regfit$adjr2), "adjr2",   max(summary_regfit$adjr2),
    which.min(summary_regfit$cp),    "cp",      min(summary_regfit$cp),
    which.min(summary_regfit$bic),   "bic",     min(summary_regfit$bic)
  ) %>%
  mutate(Measure = factor(Measure, levels = c("cp", "bic", "adjr2"), labels = c("C[p]", "BIC", "Adjusted~R^2")))


ggplot(adjustment_methods, aes(x = n_predictors, y = value)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_line(color = "purple", alpha = 0.7) +
  geom_point(data = adjustment_methods_extreme, shape = "X", size = 5, color = "blue", alpha = 0.7) +
  facet_wrap(~ Measure, scales = "free_y", labeller = label_parsed) +
  ylab(element_blank()) +
  xlab("Number of predictors") +
  theme_bw()

# Compute book definitions of Cp, BIC:
fit_full <- lm(Balance ~ ., data = Credit)
est_sigma_sq_full <- sum((fit_full$residuals)^2) / (n - p)   # the unbiased estimator

cp_book_def <-  (summary_regfit$rss +     2  * (1:nvmax) * est_sigma_sq_full) / n
bic_book_def <- (summary_regfit$rss + log(n) * (1:nvmax) * est_sigma_sq_full) / n

cor(cp_book_def, summary_regfit$cp) # linear transformation of one another
cor(bic_book_def, summary_regfit$bic) # not an exact linear transformation of one another!

cor(cp_book_def, bic_book_def) # nearly linearly dependent

adjustment_methods_book_def <- 
  tibble(
    n_predictors = 1:nvmax,
    adjr2 = summary_regfit$adjr2,
    cp = cp_book_def,
    bic = bic_book_def,
  ) %>%
  pivot_longer(-n_predictors, names_to = "Measure") %>%
  mutate(Measure = factor(Measure, levels = c("cp", "bic", "adjr2"), labels = c("C[p]", "BIC", "Adjusted~R^2")))

adjustment_methods_book_def_extreme <-
  tribble(
    ~n_predictors,                   ~Measure,  ~value,
    which.max(summary_regfit$adjr2), "adjr2",   max(summary_regfit$adjr2),
    which.min(cp_book_def),          "cp",      min(cp_book_def),
    which.min(bic_book_def),          "bic",     min(bic_book_def)
  ) %>%
  mutate(Measure = factor(Measure, levels = c("cp", "bic", "adjr2"), labels = c("C[p]", "BIC", "Adjusted~R^2")))


ggplot(adjustment_methods_book_def, aes(x = n_predictors, y = value)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_line(color = "purple", alpha = 0.7) +
  geom_point(data = adjustment_methods_book_def_extreme, shape = "X", size = 5, color = "blue", alpha = 0.7) +
  facet_wrap(~ Measure, scales = "free_y", labeller = label_parsed) +
  ylab(element_blank()) +
  xlab("Number of predictors") +
  theme_bw()

# * Validation set -----

# The effect of manufactured "randomness":
# Let's try the validation set approach with two different seeds:
nvmax <- 11
validation_methods <- tibble(n_predictors = 1:nvmax)
for (seed in 1:2) {
  set.seed(seed)
  
  Credit <- ISLR2::Credit
  
  n <- nrow(Credit)
  nvmax <- 11
  
  # 3/4 training set:
  n_train <- floor(0.75 * n)
  train <- rep(FALSE, n)
  train[sample(1:n, n_train, replace = FALSE)] <- TRUE
  # Sanity check:
  stopifnot(sum(train) == n_train)
  
  test <- (!train)
  
  regfit_best <- regsubsets(Balance ~ ., data = Credit[train, ], nvmax = nvmax)
  test_mat <- model.matrix(Balance ~ ., data = Credit[test, ]) # design matrix for test data
  
  validation_errors <- rep(NA, nvmax)
  for (i in 1:nvmax) {
    coef_i <- coef(regfit_best, id = i)  # coefficients of the best model with i predictors
    pred_i <- test_mat[, names(coef_i)] %*% coef_i   # X * beta_hat
    validation_errors[i] <- mean( (Credit$Balance[test] - pred_i)^2 )
  }
  
  validation_methods[ , str_c("validation_set_errors_seed", seed)] <- validation_errors
  
  print(which.min(validation_errors))
}
# Once we get that the best model has 4 predictors, once 6...

# Before we proceed to k-fold cross validation, let's create a function that will make 
# our task easier. Even before that, notice that there is no `predict` function for
# the object that the function `regsubsets()` returns:
regfit_best <- regsubsets(Balance ~ ., data = Credit, nvmax = nvmax)
predict(regfit_best)

# Note, the function `regsubsets()` returns an object of class `regsubsets`:
class(regfit_best)

# Compare that to `lm()`:
lm_fit <- lm(1 ~ 1)
class(lm_fit)
predict(lm_fit)

# All that The `predict()` function does is to redirect to its "methods".
# This is the entire code of the `predict()` function:
predict

# When it gets as input an object of class `lm`, it will call the `predict.lm()` function with the same object.
# So, calling `predict(lm_fit)` is the same as calling `predict.lm(lm_fit)`:
identical(predict.lm(lm_fit), predict(lm_fit))

# When we run `predict(regfit_best)`, R looks for the `predict.regsubsets()` method. Since it does not 
# exist, we get the error:
predict(regfit_best)

# However, we can write our own `predict` method for this class (based off the code for the validation set):
# `object` will be a `regsubsets` object
# `newdata` will be the data we want to generate predictions for.
# `id` will be the number of predictors in the model we wish to make predictions for.
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])   # retrieve the formula
  mat <- model.matrix(form, newdata)     # design matrix for the new data
  coef_i <- coef(object, id = id)        # coefficients of the best model with `id` predictors
  xvars <- names(coef_i)
  return( mat[, xvars] %*% coef_i )
}

# Then, to get predictions for the best 1 predictor model we can either call:
predict.regsubsets(regfit_best, newdata = Credit, id = 1)
# or:
predict(regfit_best, newdata = Credit, id = 1)


# * k-fold cross validation -----
k <- 10
nvmax <- 11
n <- nrow(Credit)
for (seed in 1:2) {
  set.seed(seed)
  folds <- sample(rep(1:k, length = n)) # sample from k classes with equal probability
  k_fold_cv_errors <- matrix(NA, k, nvmax, dimnames = list(NULL, 1:nvmax))
  
  for (j in 1:k) {
    best_fit <- 
      regsubsets(
        Balance ~ .,
        data = Credit %>% filter(folds != j),
        nvmax = nvmax
      )
    for (i in 1:nvmax) {
      pred_i <- predict(best_fit, Credit %>% filter(folds == j), id = i)
      k_fold_cv_errors[j, i] <- mean( (Credit$Balance[folds == j] - pred_i)^2 )
    }
  }
  
  validation_methods[ , str_c("k_fold_cv_errors_seed", seed)] <- unname(colMeans(k_fold_cv_errors))
  
  print(which.min(unname(colMeans(k_fold_cv_errors))))
}

validation_methods_long <- 
  validation_methods  %>%
  pivot_longer(-n_predictors, names_to = "Measure") %>%
  separate(col = Measure, sep = "_seed", into = c("Measure", "Seed")) %>%
  mutate(
    Measure =
      factor(
        Measure,
        levels = c("validation_set_errors", "k_fold_cv_errors"),
        labels = c("Validation Set Error", "k-Fold Cross Validation Error")
      )
  )


validation_methods_min <- 
  validation_methods_long %>%
  group_by(Measure, Seed) %>%
  mutate(min_value = (value == min(value))) %>%
  filter(min_value)

ggplot(validation_methods_long, aes(x = n_predictors, y = value, color = Seed)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  geom_point(data = validation_methods_min, shape = "X", size = 5, alpha = 0.7) +
  facet_wrap(~ Measure) +
  ylab("Estimated test MSE") +
  xlab("Number of predictors") +
  theme_bw()

# Ridge regression (Figure 6.4) -----
Credit <- ISLR2::Credit
x <- scale(model.matrix(Balance ~ ., data = Credit)[ , -1])
y <- Credit$Balance

library(glmnet)
lambda_grid <- 10 ^ seq(10, -2, length = 100)
ridge_reg <- glmnet(x, y, alpha = 0, lambda = lambda_grid) # alpha = 0 <-> Ridge regression

standardized_Credit <- tibble(Balance = y, as_tibble(x))
lm_reg <- lm(Balance ~ ., data = standardized_Credit)

plot(ridge_reg, xvar = "lambda")

coef_ridge <- coef(ridge_reg)
coef_ridge_mat <- t(as.matrix(coef_ridge))[ , -1]

par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1))
plot(imager::load.image("Chapter6_img02.png"), axes = FALSE) 

ll <- sqrt(rowSums(coef_ridge_mat ^ 2)) / sqrt( sum(coef(lm_reg)[-1]^2) )

coef_ridge_tibble <- 
  coef_ridge_mat %>%
  as_tibble() %>%
  mutate(
    lambda = lambda_grid,
    betaR_2_div_beta2 = ll
  ) %>%
  pivot_longer(-c("lambda", "betaR_2_div_beta2"), names_to = "coef_name", values_to = "Standardized coefficients")

g <- 
  ggplot(coef_ridge_tibble) +
  geom_line(aes(x = lambda, y = `Standardized coefficients`, group = coef_name)) +
  scale_x_continuous(trans = "log10", limits = c(0.05, 100000)) +
  theme_bw()

g + xlab(expression(lambda))

library(plotly)
ggplotly(g)  # Can use to identify most important coefficients

coef_ridge_tibble_highlight <- 
  coef_ridge_tibble %>%
  mutate(
    color = 
      ifelse(
        coef_name %in% c("Income", "Limit", "Rating", "StudentYes"), 
        coef_name,
        "Other coefficients"
      ),
    color = 
      factor(
        color, 
        levels = c("Limit", "Rating", "StudentYes", "Other coefficients", "Income"),
        labels = c("Limit", "Rating", "Student", "Other coefficients", "Income")
      )
  )

OLS_coefs <-
  tibble(
    `Standardized coefficients` = coef(lm_reg)[-1], 
    coef_name = names(coef(lm_reg)[-1]),
    lambda = 0
  ) %>%
  mutate(
    color = 
      ifelse(
        coef_name %in% c("Income", "Limit", "Rating", "StudentYes"), 
        coef_name,
        "Other coefficients"
      ),
    color = 
      factor(
        color, 
        levels = c("Limit", "Rating", "StudentYes", "Other coefficients", "Income"),
        labels = c("Limit", "Rating", "Student", "Other coefficients", "Income")
      )
  )

ggplot(coef_ridge_tibble_highlight, aes(x = lambda, y = `Standardized coefficients`, group = coef_name, color = color)) +
  geom_line() +
  scale_x_continuous(trans = "log10", limits = c(0.05, 100000)) +
  scale_color_manual(values = c("red", "blue", "orange", "grey", "black"), name = "Coefficient") +
  geom_point(data = OLS_coefs) +
  theme_bw() +
  xlab(expression(lambda)) +
  theme(legend.position = "none")

ggplot(coef_ridge_tibble_highlight) +
  geom_line(aes(x = betaR_2_div_beta2, y = `Standardized coefficients`, group = coef_name, color = color)) +
  scale_color_manual(values = c("red", "blue", "orange", "grey", "black"), name = "Coefficient") +
  theme_bw() +
  xlab(element_blank())


# * Bias-variance tradeoff (Figure 6.5) -----

# Comparison of four subset-selection techniques on a simulated linear 
# regression problem Y = X^T * beta + epsilon.
# There are n = 50 observations on p = 45 standard Gaussian variables, 
# with pairwise correlations all equal to 0.75. 
# For 10 of the variables, the coefficients are drawn at random from a N(0, 0.4) distribution;
# the rest are zero. The noise epsilon ~ N(0, 1)

# using a simulated data set containing p = 45
# predictors and n = 50 observations
set.seed(1)

n <- 50
p <- 45
sd_epsilon <- 1

Sigma <- matrix(0.75, p, p)
diag(Sigma) <- 1

matrixcalc::is.positive.definite(Sigma)

n_non_zero_coefs <- 10
true_beta <- c(rnorm(n_non_zero_coefs, 0, sqrt(0.4)), rep(0, p - n_non_zero_coefs))

test_predictors <- mvtnorm::rmvnorm(n, sigma = Sigma)
colnames(test_predictors) <- str_c("x", 1:p)

test_true_f_x <- c(test_predictors %*% true_beta)
test_y <- test_true_f_x + rnorm(n, 0, sd_epsilon)

test_df <- tibble(test_y = test_y, as_tibble(test_predictors))


summary(lm(test_y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, data = test_df))
summary(lm(test_y ~ ., data = test_df))


library(glmnet)
set.seed(1)
M <- 100
lambda_grid <- 10 ^ seq(10, -2, length = 100)
predictions <- NULL
ll <- NULL
Rsq <- NULL
for (m in 1:M) {
  # Sample a training dataset:
  train_predictors <- mvtnorm::rmvnorm(n, sigma = Sigma)
  colnames(train_predictors) <- str_c("x", 1:p)
  train_true_f_x <- c(train_predictors %*% true_beta)
  train_y <- train_true_f_x + rnorm(n, 0, sd_epsilon)
  
  # For each dataset, compute avg. bias, avg. variance and test MSE for each level of flexibility:
  
  ridge_reg <- glmnet(train_predictors, train_y, alpha = 0, lambda = lambda_grid)
  
  pred_test <-
    as_tibble(predict(ridge_reg, newx = test_predictors)) %>%
    mutate(
      test_obs_idx = factor(row_number()),
      m = m
    )
  
  predictions <- bind_rows(predictions, pred_test)
  
  # Store coefficients:
  train_df <- tibble(train_y = train_y, as_tibble(train_predictors))
  lm_coefs_vec <- coef(lm(train_y ~ ., data = train_df))[-1]
  
  coef_ridge <- coef(ridge_reg)
  coef_ridge_mat <- t(as.matrix(coef_ridge))[ , -1]
  
  ll <- rbind(ll, sqrt(rowSums(coef_ridge_mat^2)) / sqrt(sum(lm_coefs_vec^2)))
  
  # R^2 on training data (for Figure 6.8) (Homework):
  train_RSS <- colSums( (predict(ridge_reg, newx = train_predictors) - train_y)^2 )
  train_TSS <- sum( (train_y - mean(train_y))^2 )
  Rsq <- rbind(Rsq, 1 - (  train_RSS / train_TSS ))
}

bias_sq <-
  predictions %>%
  mutate(across(starts_with("s"), ~ (.x - test_true_f_x) )) %>%
  group_by(test_obs_idx) %>%
  summarize(across(starts_with("s"), ~ mean(.x)^2)) %>%
  ungroup() %>%
  summarize(across(starts_with("s"), ~ mean(.x))) %>%
  unlist()

var <-
  predictions %>%
  group_by(test_obs_idx) %>%
  summarize(across(starts_with("s"), ~ mean((.x - mean(.x))^2))) %>%
  ungroup() %>%
  summarize(across(starts_with("s"), ~ mean(.x))) %>%
  unlist()

MSEP <-
  predictions %>%
  mutate(across(starts_with("s"), ~ (.x - test_y)^2 )) %>%
  group_by(test_obs_idx) %>%
  summarize(across(starts_with("s"), ~ mean(.x))) %>%
  ungroup() %>%
  summarize(across(starts_with("s"), ~ mean(.x))) %>%
  unlist()

bias_var_MSE_ridge <- 
  tibble(
    lambda = lambda_grid, ll = colMeans(ll), bias_sq = bias_sq, var = var, MSEP = MSEP,
    Rsq = colMeans(Rsq) # Homework
  ) 

min_MSEP_ridge <- 
  bias_var_MSE_ridge %>%
  # select(lambda, ll, MSEP) %>%
  select(lambda, ll, Rsq, MSEP) %>%  # This line in HW sol, above inclass
  filter(MSEP == min(MSEP))

bias_var_MSE_ridge %>%
  pivot_longer(c(bias_sq, var, MSEP))  %>%
  ggplot() +
  geom_line(aes(x = lambda, y = value, color = name)) +
  geom_point(data = min_MSEP_ridge, aes(x = lambda, y = MSEP), color = "darkgreen", shape = "X", size = 4) +
  theme_bw() +
  scale_x_continuous(trans = "log10") +
  xlab(expression(lambda)) +
  ylab(element_blank()) +
  theme(legend.title = element_blank())

bias_var_MSE_ridge %>%
  pivot_longer(c(bias_sq, var, MSEP))  %>%
  ggplot() +
  geom_line(aes(x = ll, y = value, color = name)) +
  geom_point(data = min_MSEP_ridge, aes(x = ll, y = MSEP), color = "darkgreen", shape = "X", size = 4) +
  theme_bw() +
  xlab(expression("||"~hat(beta)[lambda]^R~"||"[2]~"/||"~hat(beta)[lambda]~"||"[2])) +
  ylab(element_blank()) +
  theme(legend.title = element_blank())



# Let's check if the ridge regression correctly detected the non-trivial predictors:

best_lamda <- min_MSEP_ridge$lambda
best_ridge <- glmnet(train_predictors, train_y, alpha = 0, lambda = best_lamda)
best_ridge$beta


# Lasso (Figure 6.6) -----
Credit <- ISLR2::Credit
x <- scale(model.matrix(Balance ~ ., data = Credit)[ , -1])
y <- Credit$Balance

library(glmnet)
lambda_grid <- 10 ^ seq(10, -2, length = 100)
lasso_reg <- glmnet(x, y, alpha = 1, lambda = lambda_grid)  # alpha = 1 for LASSO

standardized_Credit <- tibble(Balance = y, as_tibble(x))
lm_reg <- lm(Balance ~ ., data = standardized_Credit)

plot(lasso_reg, xvar = "lambda")

coef_lasso <- coef(lasso_reg)
coef_lasso_mat <- t(as.matrix(coef_lasso))[ , -1]

ll <- rowSums(abs(coef_lasso_mat)) / sum(abs(coef(lm_reg)[-1]))

coef_lasso_tibble <- 
  coef_lasso_mat %>%
  as_tibble() %>%
  mutate(
    lambda = lambda_grid,
    betaL_1_div_beta1 = ll
  ) %>%
  pivot_longer(-c("lambda", "betaL_1_div_beta1"), names_to = "coef_name", values_to = "Standardized coefficients")

g <- 
  ggplot(coef_lasso_tibble) +
  geom_line(aes(x = lambda, y = `Standardized coefficients`, group = coef_name)) +
  scale_x_continuous(trans = "log10", limits = c(0.05, 10000)) +
  theme_bw()

g + xlab(expression(lambda))

library(plotly)
ggplotly(g)  # Can use to identify most important coefficients

coef_lasso_tibble_highlight <- 
  coef_lasso_tibble %>%
  mutate(
    color = 
      ifelse(
        coef_name %in% c("Income", "Limit", "Rating", "StudentYes"), 
        coef_name,
        "Other coefficients"
      ),
    color = 
      factor(
        color, 
        levels = c("Limit", "Rating", "StudentYes", "Other coefficients", "Income"),
        labels = c("Limit", "Rating", "Student", "Other coefficients", "Income")
      )
  )

OLS_coefs <-
  tibble(
    `Standardized coefficients` = coef(lm_reg)[-1], 
    coef_name = names(coef(lm_reg)[-1]),
    lambda = 0
  ) %>%
  mutate(
    color = 
      ifelse(
        coef_name %in% c("Income", "Limit", "Rating", "StudentYes"), 
        coef_name,
        "Other coefficients"
      ),
    color = 
      factor(
        color, 
        levels = c("Limit", "Rating", "StudentYes", "Other coefficients", "Income"),
        labels = c("Limit", "Rating", "Student", "Other coefficients", "Income")
      )
  )

ggplot(coef_lasso_tibble_highlight, aes(x = lambda, y = `Standardized coefficients`, group = coef_name, color = color)) +
  geom_line() +
  scale_x_continuous(trans = "log10", limits = c(0.05, 100000)) +
  scale_color_manual(values = c("red", "blue", "orange", "grey", "black"), name = "Coefficient") +
  geom_point(data = OLS_coefs) +
  theme_bw() +
  xlab(expression(lambda))

ggplot(coef_lasso_tibble_highlight) +
  geom_line(aes(x = betaL_1_div_beta1, y = `Standardized coefficients`, group = coef_name, color = color)) +
  scale_color_manual(values = c("red", "blue", "orange", "grey", "black"), name = "Coefficient") +
  theme_bw() +
  xlab(expression("||"~hat(beta)[lambda]^L~"||"[1]~"/||"~hat(beta)[lambda]~"||"[1]))


# * Bias-variance tradeoff (Figure 6.5) -----

# Comparison of four subset-selection techniques on a simulated linear 
# regression problem Y = X^T * beta + epsilon.
# There are n = 50 observations on p = 45 standard Gaussian variables, 
# with pairwise correlations all equal to 0.85. 
# For 10 of the variables, the coefficients are drawn at random from a N(0, 0.4) distribution;
# the rest are zero. The noise epsilon ~ N(0, 1)

# using a simulated data set containing p = 45
# predictors and n = 50 observations

test_df <- readRDS("test_df.rds")

library(glmnet)
set.seed(1)
M <- 100
lambda_grid <- 10 ^ seq(10, -2, length = 100)
predictions <- NULL
ll <- NULL
Rsq <- NULL
for (m in 1:M) {
  # Sample a training dataset:
  train_predictors <- mvtnorm::rmvnorm(n, sigma = Sigma)
  colnames(train_predictors) <- str_c("x", 1:p)
  train_true_f_x <- c(train_predictors %*% true_beta)
  train_y <- train_true_f_x + rnorm(n, 0, sd_epsilon)
  
  # For each dataset, compute avg. bias, avg. variance and test MSE for each level of flexibility:
  
  lasso_reg <- glmnet(train_predictors, train_y, alpha = 1, lambda = lambda_grid)
  
  pred_test <-
    as_tibble(predict(lasso_reg, newx = test_predictors)) %>%
    mutate(
      test_obs_idx = factor(row_number()),
      m = m
    )
  
  predictions <- bind_rows(predictions, pred_test)
  
  # Store coefficients:
  train_df <- tibble(train_y = train_y, as_tibble(train_predictors))
  lm_coefs_vec <- coef(lm(train_y ~ ., data = train_df))[-1]
  
  coef_lasso <- coef(lasso_reg)
  coef_lasso_mat <- t(as.matrix(coef_lasso))[ , -1]
  
  
  ll <- rbind(ll, sqrt(rowSums(coef_lasso_mat^2)) / sqrt(sum(lm_coefs_vec^2)))
  
  # R^2 on training data (for Figure 6.8) (Homework):
  train_RSS <- colSums( (predict(lasso_reg, newx = train_predictors) - train_y)^2 )
  train_TSS <- sum( (train_y - mean(train_y))^2 )
  Rsq <- rbind(Rsq, 1 - (  train_RSS / train_TSS ))
}

bias_sq <-
  predictions %>%
  mutate(across(starts_with("s"), ~ (.x - test_true_f_x) )) %>%
  group_by(test_obs_idx) %>%
  summarize(across(starts_with("s"), ~ mean(.x)^2)) %>%
  ungroup() %>%
  summarize(across(starts_with("s"), ~ mean(.x))) %>%
  unlist()

var <-
  predictions %>%
  group_by(test_obs_idx) %>%
  summarize(across(starts_with("s"), ~ mean((.x - mean(.x))^2))) %>%
  ungroup() %>%
  summarize(across(starts_with("s"), ~ mean(.x))) %>%
  unlist()

MSEP <-
  predictions %>%
  mutate(across(starts_with("s"), ~ (.x - test_y)^2 )) %>%
  group_by(test_obs_idx) %>%
  summarize(across(starts_with("s"), ~ mean(.x))) %>%
  ungroup() %>%
  summarize(across(starts_with("s"), ~ mean(.x))) %>%
  unlist()

bias_var_MSE_lasso <- 
  tibble(
    lambda = lambda_grid, 
    ll = colMeans(ll), 
    bias_sq = bias_sq, 
    var = var, 
    MSEP = MSEP,
    Rsq = colMeans(Rsq) # Homework
  ) 

min_MSEP_lasso <- 
  bias_var_MSE_lasso %>%
  select(lambda, ll, Rsq, MSEP) %>%
  filter(MSEP == min(MSEP))

bias_var_MSE_lasso %>%
  pivot_longer(c(bias_sq, var, MSEP))  %>%
  ggplot() +
  geom_line(aes(x = lambda, y = value, color = name)) +
  geom_point(data = min_MSEP_lasso, aes(x = lambda, y = MSEP), color = "darkgreen", shape = "X", size = 4) +
  theme_bw() +
  scale_x_continuous(trans = "log10") +
  xlab(expression(lambda)) +
  ylab(element_blank()) +
  theme(legend.title = element_blank())

bias_var_MSE_lasso %>%
  pivot_longer(c(bias_sq, var, MSEP))  %>%
  ggplot() +
  geom_line(aes(x = ll, y = value, color = name)) +
  geom_point(data = min_MSEP_lasso, aes(x = ll, y = MSEP), color = "darkgreen", shape = "X", size = 4) +
  theme_bw() +
  xlab(expression("||"~hat(beta)[lambda]^R~"||"[2]~"/||"~hat(beta)[lambda]~"||"[2])) +
  ylab(element_blank()) +
  theme(legend.title = element_blank())



# Compare Lasso and Ridge -----
# The Rsq part and maybe the entire lasso part is HW.
bias_var_MSE_lasso %>%
  mutate(Type = "Lasso") %>%
  bind_rows(bias_var_MSE_ridge %>% mutate(Type = "Ridge")) %>%
  pivot_longer(c(bias_sq, var, MSEP)) %>%
  ggplot() +
  geom_line(aes(x = lambda, y = value, color = name, lty = Type)) +
  geom_point(data = min_MSEP_lasso, aes(x = lambda, y = MSEP), color = "darkgreen", shape = "X", size = 4) +
  geom_point(data = min_MSEP_ridge, aes(x = lambda, y = MSEP), color = "darkgreen", shape = "X", size = 4) +
  theme_bw() +
  scale_x_continuous(trans = "log10") +
  xlab(expression(lambda)) +
  ylab(element_blank()) +
  theme(legend.title = element_blank())

bias_var_MSE_lasso %>%
  mutate(Type = "Lasso") %>%
  bind_rows(bias_var_MSE_ridge %>% mutate(Type = "Ridge")) %>%
  pivot_longer(c(bias_sq, var, MSEP)) %>%
  ggplot() +
  geom_line(aes(x = Rsq, y = value, color = name, lty = Type)) +
  geom_point(data = min_MSEP_lasso, aes(x = Rsq, y = MSEP), color = "darkgreen", shape = "X", size = 4) +
  geom_point(data = min_MSEP_ridge, aes(x = Rsq, y = MSEP), color = "darkgreen", shape = "X", size = 4) +
  theme_bw() +
  xlab(expression(R^2~"on training data")) +
  ylab(element_blank()) +
  theme(legend.title = element_blank())

# Selecting the tuning parameter -----

# * Ridge -----
Credit <- ISLR2::Credit
x <- scale(model.matrix(Balance ~ ., data = Credit)[ , -1])
y <- Credit$Balance

library(glmnet)
lambda_grid <- 10 ^ seq(10, -2, length = 100)
cv_ridge <- 
  cv.glmnet(
    x,
    y,
    alpha = 0,
    lambda = lambda_grid,
    type.measure = "mse",
    nfolds = nrow(x)        # Set the number of folds to n to get LOOCV
  )

# Built in method:
plot(cv_ridge)

# Our way:
coef(cv_ridge)
cv_ridge$lambda
cv_ridge$cvm

cv_ridge_df <- tibble(lambda = cv_ridge$lambda, MSE = cv_ridge$cvm)

cv_ridge_df %>%
  filter(lambda < 5) %>%
  ggplot() +
  geom_line(aes(x = lambda, y = MSE), color = "blue") +
  geom_vline(xintercept = cv_ridge$lambda.min, lty = 2) +
  scale_x_continuous(trans = "log10") +
  xlab(expression(lambda)) +
  theme_bw()

ridge_reg <- glmnet(x, y, alpha = 0, lambda = lambda_grid)
coef_ridge <- coef(ridge_reg)
coef_ridge_mat <- t(as.matrix(coef_ridge))[ , -1]

coef_ridge_tibble_highlight <- 
  coef_ridge_mat %>%
  as_tibble() %>%
  mutate(lambda = lambda_grid) %>%
  pivot_longer(-c("lambda"), names_to = "coef_name", values_to = "Standardized coefficients") %>%
  mutate(
    color = 
      ifelse(
        coef_name %in% c("Income", "Limit", "Rating", "StudentYes"), 
        coef_name,
        "Other coefficients"
      ),
    color = 
      factor(
        color, 
        levels = c("Limit", "Rating", "StudentYes", "Other coefficients", "Income"),
        labels = c("Limit", "Rating", "Student", "Other coefficients", "Income")
      )
  )

coef_ridge_tibble_highlight %>%
  filter(lambda < 5) %>%
  ggplot(aes(x = lambda, y = `Standardized coefficients`, group = coef_name, color = color)) +
  geom_line() +
  geom_vline(xintercept = cv_ridge$lambda.min, lty = 2) +
  scale_x_continuous(trans = "log10") +
  scale_color_manual(values = c("red", "blue", "orange", "grey", "black"), name = "Coefficient") +
  theme_bw() +
  xlab(expression(lambda)) +
  theme(legend.position = "none")

# First PC (Figure 6.14) -----

true_lambda <- c(3.25 ^ 2, 1) # gleaned by visually comparing the axes in Figure 6.14
true_Lambda <- diag(true_lambda)

true_phi_11 <- 0.839
true_phi_21 <- sqrt(1 - true_phi_11 ^ 2)

true_phi_12 <- true_phi_21
true_phi_22 <- -sqrt(1 - true_phi_12 ^ 2)

true_Q <- 
  matrix(
    c(
      true_phi_11, true_phi_21,  # First eigenvector:  phi_11, phi_21 (column)
      true_phi_12, true_phi_22   # Second eigenvector: phi_11, phi_21 (column)
    ),
    ncol = 2,
    byrow = FALSE
  )

true_Sigma_raw <- true_Q %*% true_Lambda %*% true_Q
isSymmetric(true_Sigma_raw)
true_Sigma <- 0.5 * (true_Sigma_raw + t(true_Sigma_raw))
matrixcalc::is.positive.definite(true_Sigma)

true_mu <- c(40, 20)

set.seed(1)
ad_pop_mat <- mvtnorm::rmvnorm(100, mean = true_mu, sigma = true_Sigma)
colnames(ad_pop_mat) <- c("pop", "ad")
ad_pop_df <- as_tibble(ad_pop_mat)


sample_variance_covariance <- var(ad_pop_mat)
eigen_decomposition_Sigma <- eigen(sample_variance_covariance)

est_lambda <- eigen_decomposition_Sigma$values
est_Lambda <- diag(est_lambda)
est_Q <- eigen_decomposition_Sigma$vectors

for (i in 1:ncol(est_Q)) {
  if (all(sign(est_Q[ , i]) == -1)) {
    est_Q[ , i] <- -est_Q[ , i]
  }
}
# Post hoc fix: since eigenvectors are defined up to a sign, we take
# eigenvectors whose elements are all negative to be all positive.
est_Q

est_Sigma <- est_Q %*% est_Lambda %*% est_Q
est_mu <- colMeans(ad_pop_mat)

(g1 <-
    ggplot(ad_pop_df) +
    geom_point(aes(x = pop, y = ad), color = "purple") +
    geom_segment(
      aes(
        x = est_mu[1], 
        y = est_mu[2], 
        xend = est_mu[1] + est_Q[1, 1] * sqrt(est_lambda[1]) * sqrt(qchisq(0.95, 2)), 
        yend = est_mu[2] + est_Q[2, 1] * sqrt(est_lambda[1]) * sqrt(qchisq(0.95, 2))
      ), # qchisq(0.95, 2) for a 95% contour.
      arrow = arrow(length = unit(0.1, "inches")),
      color = "aquamarine4") +
    xlab("Population") +
    ylab("Advertising") +
    theme_bw())

# PC as projection (Figure 6.15) -----
projection_to_PC_1 <- 
  t(
    apply(
      ad_pop_df[ , c("pop", "ad")],
      1,
      function(x) est_mu + sum((x - est_mu) * est_Q[, 1]) * est_Q[, 1])
    # project point x onto a line v by <x, v> * v
  )
colnames(projection_to_PC_1) <- c("pop_proj_PC1", "ad_proj_PC1")

ad_pop_df_proj_PC1 <- bind_cols(ad_pop_df, as_tibble(projection_to_PC_1))

ad_pop_df_proj_PC1_sample <- ad_pop_df_proj_PC1 %>% slice_sample(n = 20)

origin_pop <- min(ad_pop_df_proj_PC1_sample$pop_proj_PC1)
origin_ad <- min(ad_pop_df_proj_PC1_sample$ad_proj_PC1)

ggplot(ad_pop_df_proj_PC1_sample) +
  geom_segment(aes(x = pop, y = ad, xend = pop_proj_PC1, yend = ad_proj_PC1), color = "black", lty = 3) +
  geom_segment(
    aes(
      x = origin_pop, 
      y = origin_ad, 
      xend = origin_pop + sqrt(2) * est_Q[1, 1] * sqrt(est_lambda[1]) * sqrt(qchisq(0.95, 2)), 
      yend = origin_ad + sqrt(2) * est_Q[2, 1] * sqrt(est_lambda[1]) * sqrt(qchisq(0.95, 2))
    ),
    color = "aquamarine4") +
  geom_point(aes(x = pop, y = ad), color = "purple") +
  geom_point(aes(x = pop_proj_PC1, y = ad_proj_PC1), color = "black", shape = "x", size = 3) +
  xlab("Population") +
  ylab("Advertising") +
  theme_bw()

# Figure 6.16 -----
ad_pop_df %>%
  mutate(z1 = est_Q[1, 1] * (pop - est_mu[1]) + est_Q[2, 1] * (ad - est_mu[2])) %>%
  pivot_longer(cols = c(pop, ad), names_to = "Variable") %>%
  mutate(Variable = factor(Variable, levels = c("pop", "ad"), labels = c("Population", "Advertising"))) %>%
  ggplot() +
  geom_point(aes(x = z1, y = value), color = "purple") +
  xlab("1st Principle Compopnent") +
  ylab(element_blank()) +
  theme_bw() +
  facet_wrap(~ Variable, scales = "free_y")


# Second PC (Figure 6.14) -----

g1 +
  geom_segment(
    aes(
      x = est_mu[1], 
      y = est_mu[2], 
      xend = est_mu[1] + est_Q[1, 2] * sqrt(est_lambda[2]) * sqrt(qchisq(0.95, 2)), 
      yend = est_mu[2] + est_Q[2, 2] * sqrt(est_lambda[2]) * sqrt(qchisq(0.95, 2))
    ),
    arrow = arrow(length = unit(0.1, "inches")),
    color = "cyan3")


ad_pop_df_proj_PC1_sample %>%
  mutate(
    z1 = est_Q[1, 1] * (pop - est_mu[1]) + est_Q[2, 1] * (ad - est_mu[2]),
    z2 = est_Q[1, 2] * (pop - est_mu[1]) + est_Q[2, 2] * (ad - est_mu[2])
  ) %>%
  ggplot() +
  geom_segment(aes(x = z1, y = 0, xend = z1, yend = z2), color = "black", lty = 3) +
  geom_hline(yintercept = 0, color = "aquamarine4") +
  geom_point(aes(x = z1, y = z2), color = "purple") +
  geom_point(aes(x = z1, y = 0), color = "black", shape = "x", size = 3) +
  xlab("1st principal component") +
  ylab("2nd principal component") +
  ylim(c(-3, 3)) +
  theme_bw()

# Figure 6.17 -----
ad_pop_df %>%
  mutate(z2 = est_Q[1, 2] * (pop - est_mu[1]) + est_Q[2, 2] * (ad - est_mu[2])) %>%
  pivot_longer(cols = c(pop, ad), names_to = "Variable") %>%
  mutate(Variable = factor(Variable, levels = c("pop", "ad"), labels = c("Population", "Advertising"))) %>%
  ggplot() +
  geom_point(aes(x = z2, y = value), color = "purple") +
  xlab("2nd Principle Compopnent") +
  ylab(element_blank()) +
  theme_bw() +
  facet_wrap(~ Variable, scales = "free_y")

# PCR -----

# So, we can compare the PCR on 1 PC, 2 PCs and the standard OLS on ad and pop:

# Assume that the true regression relationship is:
# E[sales] = 6 + 0.4 * pop + 0.9 * ad
true_beta0 <- 6
true_beta1 <- 0.1
true_beta2 <- 0.4
n <- nrow(ad_pop_df)

set.seed(1)
ad_pop_PCR <-
  ad_pop_df %>%
  mutate(
    z1 = est_Q[1, 1] * (pop - est_mu[1]) + est_Q[2, 1] * (ad - est_mu[2]),
    z2 = est_Q[1, 2] * (pop - est_mu[1]) + est_Q[2, 2] * (ad - est_mu[2])
  ) %>%
  mutate(
    true_fx = true_beta0 + true_beta1 * pop + true_beta2 * ad,
    sales = true_fx + rnorm(n),
    type = "Train"
  )

ad_pop_PCR$type[sample(n, 20)] <- "Test"

ad_pop_PCR_train <- ad_pop_PCR %>% filter(type == "Train")
ad_pop_PCR_test <- ad_pop_PCR %>% filter(type == "Test")

PCR1 <- lm(sales ~ z1, data = ad_pop_PCR_train)
# MSE for regression on 1 PC:
(MSE_1PC <- mean( (predict(PCR1, newdata = ad_pop_PCR_test) - ad_pop_PCR_test$sales)^2 ))

PCR2 <- lm(sales ~ z1 + z2, data = ad_pop_PCR_train)
# MSE for regression on 2 PCs:
(MSE_2PC <- mean( (predict(PCR2, newdata = ad_pop_PCR_test) - ad_pop_PCR_test$sales)^2 ))

lm_fit <- lm(sales ~ pop + ad, data = ad_pop_PCR_train)
# MSE for regression on 1 PC:
(MSE_lm_fit <- mean( (predict(lm_fit, newdata = ad_pop_PCR_test) - ad_pop_PCR_test$sales)^2 ))

# The MSE for the 1 PC regression is smaller than that for the 2PC regression:
MSE_1PC < MSE_2PC
# The MSE for the 2 PCs regression is the same as the MSE for the regression with both
# predictors:
all.equal(MSE_2PC, MSE_lm_fit)

library(pls)
pcr_fit <- pcr(sales ~ pop + ad, data = ad_pop_PCR_train)
summary(pcr_fit)

# Test MSE:
pred_pcr_1PC <- predict(pcr_fit, ad_pop_PCR_test, ncomp = 1)
MSE_1PC_pcr <- mean( (pred_pcr_1PC - ad_pop_PCR_test$sales)^2 )
all.equal(MSE_1PC, MSE_1PC_pcr)

pred_pcr_2PC <- predict(pcr_fit, ad_pop_PCR_test, ncomp = 2)
MSE_2PC_pcr <- mean( (pred_pcr_2PC - ad_pop_PCR_test$sales)^2 )
all.equal(MSE_2PC, MSE_2PC_pcr)

# We could also generate predictions for several regressions at the same time:
pred_pcr <- predict(pcr_fit, ad_pop_PCR_test, ncomp = c(1, 2))
# pred_pcr is an array.
# Predictions for first component:
pred_pcr[ , , 1]
# Predictions for first and second components:
pred_pcr[ , , 2]
# etc.

# We could, in this case, convert this to a matrix using:
pred_pcr_mat <- matrix(pred_pcr, nrow = nrow(ad_pop_PCR_test))
# or
apply(pred_pcr, 3, identity) == pred_pcr_mat

# And then get MSE using:
colMeans((pred_pcr_mat - ad_pop_PCR_test$sales)^2)

# Bias:
colMeans((pred_pcr_mat - ad_pop_PCR_test$true_fx))^2

# If we wanted to compute variance for different training sets we could:
pred_pcr_mat1 <- pred_pcr_mat
colnames(pred_pcr_mat1) <- c("PC1", "PC2")
pred_pcr_mat2 <- pred_pcr_mat + runif(40)  # (generating here fake predictions)
colnames(pred_pcr_mat2) <- c("PC1", "PC2")

pred_pcr_tibble <- 
  bind_rows(
    as_tibble(pred_pcr_mat1) %>% mutate(test_obs_idx = row_number()),
    as_tibble(pred_pcr_mat2) %>% mutate(test_obs_idx = row_number())
  )

var <-
  pred_pcr_tibble %>%
  group_by(test_obs_idx) %>%
  summarize(across(starts_with("PC"), ~ mean((.x - mean(.x))^2))) %>%
  ungroup() %>%
  summarize(across(starts_with("PC"), ~ mean(.x))) %>%
  unlist()


# * Bias-variance tradeoff (Figure 6.18) homework -----

# Complete the code



# Selecting the number of components -----

# Coefficients:
Credit_PCR <- pcr(Balance ~ ., data = standardized_Credit)

coef_Credit_PCR_array <- coef(Credit_PCR, ncomp = 1:11)
coef_Credit_PCR_mat <- apply(coef_Credit_PCR_array, 1, identity)
coef_Credit_PCR <- 
  as_tibble(coef_Credit_PCR_mat) %>%
  mutate(n_PC = row_number()) %>%
  pivot_longer(-n_PC, names_to = "coef_name", values_to = "Estimate")

coef_ridge_PCR_highlight <- 
  coef_Credit_PCR %>%
  mutate(
    color = 
      ifelse(
        coef_name %in% c("Income", "Limit", "Rating", "StudentYes"), 
        coef_name,
        "Other coefficients"
      ),
    color = 
      factor(
        color, 
        levels = c("Limit", "Rating", "StudentYes", "Other coefficients", "Income"),
        labels = c("Limit", "Rating", "Student", "Other coefficients", "Income")
      )
  )

ggplot(
  coef_ridge_PCR_highlight, 
  aes(x = n_PC, y = Estimate, group = coef_name, color = color, lty = color)
) +
  geom_step(direction = "vh") +
  scale_color_manual(values = c("red", "blue", "orange", "grey", "black"), name = "Coefficient") +
  scale_linetype_manual(values = c(2, 3, 4, 1, 1), guide = "none") +
  theme_bw() +
  xlab("Number of components") +
  ylab("Standardized Coefficients")


k <- 10
nvmax <- 11
n <- nrow(standardized_Credit)
set.seed(1)

validation_methods <- tibble(n_predictors = 1:nvmax)
folds <- sample(rep(1:k, length = n)) # sample from k classes with equal probability
k_fold_cv_errors <- matrix(NA, k, nvmax, dimnames = list(NULL, 1:nvmax))

for (j in 1:k) {
  Credit_PCR_CV <- pcr(Balance ~ ., data = standardized_Credit %>% filter(folds != j))
  pred_i_array <- predict(Credit_PCR_CV, newdata = standardized_Credit %>% filter(folds == j), ncomp = 1:nvmax)
  pred_i <- apply(pred_i_array, 3, identity)
  k_fold_cv_errors[j, ] <- colMeans( (Credit$Balance[folds == j] - pred_i)^2 )
}
validation_methods[ , str_c("MSEP")] <- unname(colMeans(k_fold_cv_errors))

ggplot(validation_methods, aes(x = n_predictors, y = MSEP)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  ylab("Estimated test MSE") +
  xlab("Number of predictors") +
  theme_bw()



# We can also use an automated option for doing cross-validation to estimate the
# test MSE:
Credit_PCR_CV <- pcr(Balance ~ ., data = standardized_Credit, validation = "CV", segments = 10)
validationplot(Credit_PCR_CV, val.type = "MSEP")

Credit_PCR_CV$coefficients
a <- Credit_PCR_CV$validation$adj
plot(1:11, a)
lines(1:11, a)
as_tibble(apply(Credit_PCR_CV$coefficients, 3, identity))
