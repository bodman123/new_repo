# Figure 2.1 -----
library(tidyverse)

Advertising <- read_csv("C:/Users/Jerry Bodman/Desktop/Homework files/Advertising.csv")

Advertising_tidy <-
  Advertising %>%
  pivot_longer(
    cols = c(TV, radio, newspaper), 
    names_to = "Media", 
    values_to = "Budget"
    ) %>%
  mutate(
    Media = 
      factor(
        Media, 
        levels = c("TV", "radio", "newspaper"), 
        labels = c("TV", "Radio", "Newspaper"))
    )

(
  Fig2_1 <-
  Advertising_tidy %>%
  ggplot(aes(x = Budget, y = sales)) +
    geom_point(color = "red", shape = 1) +
    ylab("Sales") +
    facet_wrap( ~ Media, scales = "free_x") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    theme_bw()
)

library(plotly)
ggplotly(Fig2_1)


# Figure 2.9 -----

# The function (crude attempt to recreate the shape of f from Figure 2.9):
f_2_9 <- function(x) {
  -4.5 * ((x - 10) / 50)^3 + 9 * ((x - 10) / 50)^2 + 4.2
}

x_range_tibble <- tibble(x = c(0, 100))

(g0 <-  
  ggplot() + 
  stat_function(data = x_range_tibble, aes(x = x), fun = f_2_9) +
  theme_bw())

set.seed(1)
n_train <- 45
n_test <- 15
var_epsilon <- 2

# Here we SIMULATE a test set. This not what we do usually with applied problems.
# Usually, with applied problems we get a test set that is give to us. The column true_f_x
# is absent in applied problems. We only know it because we are simulating. 
test_data <- 
  tibble(
    x = runif(n = n_test, min = 0, max = 100),
    true_f_x = f_2_9(x),
    y = true_f_x + rnorm(n = n_test, mean = 0, sd = sqrt(var_epsilon)),
    type = "te"
  )

# Simulate training obs from the same "generative model": here the generative model 
# is y_i = f_2_9(x_i) + epsilon_i, epsilon_i ~ Normal(0, sigma = var_epsilon)

train_data <- 
  tibble(
    x = runif(n = n_train, min = 0, max = 100),
    true_f_x = f_2_9(x),
    y = true_f_x + rnorm(n = n_train, mean = 0, sd = sqrt(var_epsilon)),
    type = "tr"
  )

# Claim: linear model can be a good match here!
# Why?    y_i = beta_0 + beta_1 * x_i + beta_2 * x_i^2 + beta_3 * x_i^3 + epsilon_i 
# where epsilon_i ~iid Normal(0, sigma)

df_full <- bind_rows(train_data, test_data)

(g1 <- 
  g0 + 
  geom_point(data = df_full, aes(x = x, y = y, color = type), shape = 1))

# We fit the smooth spline for the training data:
smsp_2_9_df5 <- smooth.spline(x = train_data$x, y = train_data$y, df = 5)
smsp_2_9_df23 <- smooth.spline(x = train_data$x, y = train_data$y, df = 23)

g1 + 
  geom_line(aes(x = smsp_2_9_df5$x, y = smsp_2_9_df5$y), color = "blue") +
  geom_line(aes(x = smsp_2_9_df23$x, y = smsp_2_9_df23$y), color = "green") +
  geom_smooth(data = train_data, aes(x = x, y = y), color = "orange", method = "lm", se = FALSE)

# (the last orange curve, is a simple linear regression of y vs x)


# MSE (de facto MSEP, EPE) -----

par(mar = c(0,0,0,0))
plot(imager::load.image("Chapter2_img1.png"), axes = FALSE)

# We could compute the MSEP for the training data:
pred_train_df5 <- predict(smsp_2_9_df5, x = train_data$x)
(  MSEP_train_df5 <-  
    mean(
      (  train_data$y - pred_train_df5$y  ) ^ 2
    )
    )

pred_train_df23 <- predict(smsp_2_9_df23, x = train_data$x)
(  MSEP_train_df23 <- mean( (train_data$y - pred_train_df23$y) ^ 2 )  )
# In which case the MSEP for the more flexible model is lower.

# But we SHOULD compute the MSEP for the testing data:
pred_test_df5 <- predict(smsp_2_9_df5, x = test_data$x)
(  MSEP_test_df5 <- mean( (test_data$y - pred_test_df5$y) ^ 2 )  )

pred_test_df23 <- predict(smsp_2_9_df23, x = test_data$x)
(  MSEP_test_df23 <- mean( (test_data$y - pred_test_df23$y) ^ 2)  )
# In which case the MSEP for the LESS flexible model is lower.


# Recreate right panel of Figure 2.9, MSEP -----
df_seq <- seq(2, 25, 0.1)
MSEP <- tibble(df = rep(NA, length(df_seq)), train = NA, test = NA)

for (i in 1:length(df_seq)) {
  MSEP$df[i] <- df_seq[i]
  
  smsp_2_9 <- smooth.spline(x = train_data$x, y = train_data$y, df = df_seq[i])
  
  pred_train <- predict(smsp_2_9, x = train_data$x)
  MSEP$train[i] <- mean( (train_data$y - pred_train$y) ^ 2 )
  
  pred_test <- predict(smsp_2_9, x = test_data$x)
  MSEP$test[i] <- mean( (test_data$y - pred_test$y) ^ 2 )
} 

MSEP_long <- pivot_longer(MSEP, cols = c(train, test), names_to = "Subset", values_to = "MSEP")

(g_MSEP0 <-
  ggplot(MSEP_long) +
  geom_line(aes(x = df, y = MSEP, color = Subset)) +
  theme_bw())

min_test_df <-
  MSEP %>%
  filter(test == min(test)) %>%
  pull(df)

g_MSEP0 +
  xlab("Flexibility") +
  ylab("Mean Squared Error") +
  geom_point(data = MSEP_long %>% filter(df == 2), aes(x = df, y = MSEP), color = "orange", shape = "square") +
  geom_point(data = MSEP_long %>% filter(df == 5), aes(x = df, y = MSEP), color = "blue", shape = "square") +
  geom_point(data = MSEP_long %>% filter(df == 23), aes(x = df, y = MSEP), color = "green", shape = "square") +
  geom_point(data = MSEP_long %>% filter(df == min_test_df), aes(x = df, y = MSEP), color = "black", shape = "X", size = 3)


# Uncertainty quantification -----

# That's nice to do on a single dataset, but for more credible results, we
# we should compute the MSEP for each level of flexibility several times,
# and take the average, as well as a confidence interval.

rm(list = ls())
f_2_9 <- function(x) {
  -4.5 * ((x - 10) / 50)^3 + 9 * ((x - 10) / 50)^2 + 4.2
}

set.seed(1)
n_train <- 45
n_test <- 15
var_epsilon <- 2

test_data <- 
  tibble(
    x = runif(n = n_test, min = 0, max = 100),
    true_f_x = "",
    y = "",
    type = "te"
  )

df_seq <- seq(2, 25, 0.1)
# We'll repeat the simulation 100 times:
M <- 100

# Data frame for all results:
MSEP <- tibble(m = rep(NA, length(df_seq) * M), df = NA, train = NA, test = NA)

r <- 1
for (m in 1:M) {
  if (m%%5 == 0) print(m)
  # Sample a dataset:
  train_data <- 
    tibble(
      x = runif(n = n_train, min = 0, max = 100),
      true_f_x = f_2_9(x),
      y = true_f_x + rnorm(n_train, sd = sqrt(var_epsilon)),
      type = "tr"
    )
  
  # For each dataset, compute training and testing MSEP for each level of flexibility:
  for (i in 1:length(df_seq)) {
    MSEP$m[r] <- m
    MSEP$df[r] <- df_seq[i]
    
    smsp_2_9 <- smooth.spline(x = train_data$x, y = train_data$y, df = "")
    
    pred_train <- predict(smsp_2_9, x = "")
    MSEP$train[r] <- ""
    
    pred_test <- predict(smsp_2_9, x = "")
    MSEP$test[r] <- ""

    r <- r + 1
  } 
}

# Summarize the results:
MSEP_summary <- 
  MSEP %>%
  pivot_longer(cols = c(train, test), names_to = "Subset", values_to = "MSEP") %>%
  group_by(df, Subset) %>%
  summarize(
    mean_MSEP = mean(MSEP),
    lower = "",
    upper = "",
  )

ggplot(MSEP_summary) +
  geom_ribbon(aes(x = df, ymin = lower, ymax = upper, group = Subset), fill = rgb(0.7, 0.7, 0.7, 0.7)) +
  geom_line(aes(x = df, y = mean_MSEP, color = Subset)) +
  geom_hline(yintercept = var_epsilon, lty = 2) +
  theme_bw() +
  xlab("Flexibility") +
  ylab("Mean Squared Error")

# Wait, is there something wrong here?
# Why is the minimal MSEP less than var_epsilon?

# Answer:





# Bias-Variance Trade-off -----
f_2_9 <- function(x) {
  -4.5 * ((x - 10) / 50)^3 + 9 * ((x - 10) / 50)^2 + 4.2
}

n_train <- 45
n_test <- 15
var_epsilon <- 2

set.seed(1)
# This is our single one and only test set:
test_data <- 
  tibble(
    x = runif(n = n_test, min = 0, max = 100),
    true_f_x = f_2_9(x),
    y = true_f_x + rnorm(n_test, sd = sqrt(var_epsilon))
  )

df_seq <- seq(2, 25, 0.1)
# We'll repeat the simulation 100 times:
M <- 100

# Data frame for all results (we need to average over MSE / bias / var for each x over many datasets,
# so we need to store all the predictions as a first step, then compute point-wise MSE / bias / var 
# and average over those)

# Important difference between this and the previous simulation:
# Here, we need to compute the variance and bias for each point. So
# we can't just sample a training dataset, compute a statistic (MSE)
# save it, and repeat. We need to save ALL predictions from all training
# datasets, and post-hoc compute the variance for each point across all datasets.


predictions <- NULL
for (m in 1:M) {
  if (m%%5 == 0) print(m)
  # Sample a training dataset: (many times)
  train_data <- 
    tibble(
      x = runif(n = n_train, min = 0, max = 100),
      true_f_x = f_2_9(x),
      y = true_f_x + rnorm(n_train, sd = sqrt(var_epsilon)),
      type = "tr"
    )
  
  # For each dataset, compute avg. bias, avg. variance and test MSE for each level of flexibility:
  for (i in 1:length(df_seq)) {
    smsp_2_9 <- smooth.spline(x = train_data$x, y = train_data$y, df = df_seq[i])
    
    pred_test <- 
      as_tibble(predict(smsp_2_9, x = test_data$x)) %>%
      rename(pred_f_x = y) %>%
      mutate(
        true_f_x = test_data$true_f_x,
        true_y = test_data$y,
        test_obs_idx = factor(row_number()),
        m = m,
        df = df_seq[i]
      )
    
    predictions <- bind_rows(predictions, pred_test)
  }
}

bias_var_MSE <-
  predictions %>%
  group_by(test_obs_idx, df) %>%
  summarize(
    pt_wise_bias_sq = (mean(pred_f_x - true_f_x)) ^ 2,
    pt_wise_var = mean( (pred_f_x - mean(pred_f_x)) ^ 2 ),
    pt_wise_MSEE = mean( (pred_f_x - true_f_x) ^ 2 ),
    pt_wise_MSEP = mean( (pred_f_x - true_y) ^ 2 )
  ) %>%
  group_by(df) %>%
  summarize(
    bias_sq = mean(pt_wise_bias_sq),
    var = mean(pt_wise_var),
    MSEE = mean(pt_wise_MSEE),
    MSEP = mean(pt_wise_MSEP)
  )

# If we did this right, the summary statistic MSEP should equal the one we computed for Figure 2.9:
all.equal(
  MSEP_summary %>% filter(Subset == "test") %>% pull(mean_MSEP), 
  bias_var_MSE$MSEP
)


all.equal(bias_var_MSE$MSEE, "")
# Always true

est_var_epsilon <- ""

bias_var_MSE$MSEP - (bias_var_MSE$bias_sq + bias_var_MSE$var + est_var_epsilon)
# True only in the limit

(g <-
  bias_var_MSE %>%
  pivot_longer(c(bias_sq, var, MSEE, MSEP)) %>% 
  ggplot() +
  geom_line(aes(x = df, y = value, color = name)) +
  geom_hline(yintercept = est_var_epsilon, lty = 2) +
  theme_bw() +
  xlab("Flexibility") +
  ylab("Mean Squared Error") +
  theme(legend.title = element_blank()))

ggplotly(g)


# The classification setting -----

# The textbook uses a simulated dataset for illustrations of classification problems.
# The instructions are given in pages 16-17 of Elements of Statistical Learning (2nd ed.):

# 1.1. Generate 10 means m_k from a bivariate Gaussian distribution N((1, 0)^T, I) and label
#      this class BLUE. 
# 1.2. Generate 10 more means m_k from a bivariate Gaussian distribution N((0, 1)^T, I) and label
#      this class ORANGE. 
# 2.   For each class we generate 100 observations as follows:
#      i. for each observation, pick an m_k at random with probability 1/10, 
#         and then generate a N(m_k , I/5), thus leading to a mixture of Gaussian 
#         clusters for each class.

# 1.1 + 1.2:
library(mvtnorm)

set.seed(101)
n_m <- 20
stopifnot(!(n_m %% 2))
m_indices <- 1:(n_m / 2)

class_grand_means <- 
  tribble(
    ~x1,  ~x2,  ~class,
    1,    0,    "BLUE",
    0,    1,    "ORANGE",
  )

# Sample kernel means:
m_BLUE <- 
  rmvnorm(
    n_m / 2, 
    class_grand_means %>% filter(class == "BLUE") %>% select(x1, x2) %>% unlist(), 
    diag(2)
    )
colnames(m_BLUE) <- c("x1", "x2")
m_BLUE <- as_tibble(m_BLUE) %>% mutate(m_idx = m_indices, class = "BLUE")

m_ORANGE <- 
  rmvnorm(
    n_m / 2, 
    class_grand_means %>% filter(class == "ORANGE") %>% select(x1, x2) %>% unlist(), 
    diag(2)
  )
colnames(m_ORANGE) <- c("x1", "x2")
m_ORANGE <- as_tibble(m_ORANGE) %>% mutate(m_idx = m_indices, class = "ORANGE")

m <- bind_rows(m_BLUE, m_ORANGE)

# 2.

#      i. for each observation, pick an m_k at random with probability 1/10, 
#         and then generate a N(m_k , I/5), thus leading to a mixture of Gaussian 
#         clusters for each class.
n_obs_per_class <- 100

sample <- 
  tibble(
    i = rep(1:n_obs_per_class, 2), 
    m_idx = sample(1:(n_m / 2), 2 * n_obs_per_class, replace = TRUE),
    class = rep(c("BLUE", "ORANGE"), each = n_obs_per_class),
    x1 = NA,
    x2 = NA
    )

# Sample individual observations:
for (cl in c("BLUE", "ORANGE")) {
  # class_grand_mean <- class_grand_means %>% filter(class == cl) %>% select(x1, x2) %>% unlist()
  for (m_idx in m_indices) {
    m_counts <- table(sample %>% filter(class == cl) %>% pull(m_idx))
    sample[(sample$class == cl & sample$m_idx == m_idx), c("x1", "x2")] <- 
      rmvnorm(
        m_counts[m_idx],
        unlist(m[(m$class == cl & m$m_idx == m_idx), c("x1", "x2")]),
        diag(2) / 5)    # Play with denominator to demonstrate how each class is comprised of separate kernels.  
  }
}


(g0 <-
  ggplot(sample) +
  geom_point(aes(x = x1, y = x2, color = class), shape = 1) +
  scale_color_manual(values = c("BLUE", "ORANGE")) +
  theme_bw() +
  theme(legend.position = "none"))

# Compute (true) class probabilities on a fine grid:
x1_lim <- c(floor(min(sample$x1)), ceiling(max(sample$x1)))
x2_lim <- c(floor(min(sample$x2)), ceiling(max(sample$x2)))

x1_seq <- seq(x1_lim[1], x1_lim[2], length.out = 200)
x2_seq <- seq(x2_lim[1], x2_lim[2], length.out = 200)
x <- expand_grid(x1 = x1_seq, x2 = x2_seq)
x_mat <- as.matrix(x)

p <- tibble(BLUE = rep(0, nrow(x_mat)), ORANGE = 0)
for (cl in c("BLUE", "ORANGE")) {
  for (current_m_idx in m_indices) {
    current_m <- m %>% filter(m_idx == current_m_idx, class == cl) %>% select(x1, x2) %>% unlist()
    p[ , cl] <- p[ , cl] + ""
  }
}

xp <- 
  bind_cols(x, p) %>%
  mutate(
    classify_bayes = ifelse(BLUE > ORANGE, "BLUE", "ORANGE"),
    p_bayes_ORANGE = "",
    p_bayes_BLUE = ""
    )

# Show true densities:
g0 +
  stat_contour(data = xp, aes(x = x1, y = x2, z = ORANGE), color = "ORANGE", alpha = 0.35) +
  stat_contour(data = xp, aes(x = x1, y = x2, z = BLUE), color = "BLUE", alpha = 0.25)

# Classify grid points:
(g1 <- 
  g0 +
  geom_point(data = xp, aes(x = x1, y = x2, color = classify_bayes), pch = ".", alpha = 0.3))

# An easy way to plot the Bayes decision boundary:
(g2 <- 
    g1 +
    stat_contour(data = xp, aes(x = x1, y = x2, z = p_bayes_ORANGE), color = "purple", breaks = c(0, 0.5)))


# KNN -----
# Figure 2.15, K = 25:
library(class)
knn25 <- 
  knn(
    train = sample %>% select(x1, x2),
    test = xp %>% select(x1, x2),
    cl = sample$class,
    k = 25,
    prob = TRUE
    )
prop_win <- attr(knn25, "prob")  # Proportion of votes for winning class
prob <- ifelse(knn25 == "BLUE", prop_win, 1 - prop_win)

xp_knn <- xp %>% mutate(classify_KNN25 = knn25, p_KNN25_ORANGE = prob)

# Classify grid points with KNN 25:
g0 +
  geom_point(data = xp_knn, aes(x = x1, y = x2, color = classify_KNN25), pch = ".", alpha = 0.3) +
  stat_contour(data = xp_knn, aes(x = x1, y = x2, z = p_bayes_ORANGE), color = "purple", breaks = c(0, 0.5)) +
  stat_contour(data = xp_knn, aes(x = x1, y = x2, z = p_KNN25_ORANGE), color = "black", breaks = c(0, 0.5))

# Figure 2.16, left, K = 1:
library(class)
knn1 <- 
  knn(
    train = sample %>% select(x1, x2),
    test = xp %>% select(x1, x2),
    cl = sample$class,
    k = 1,
    prob = TRUE
  )
prop_win <- attr(knn1, "prob")  # Proportion of votes for winning class
prob <- ifelse(knn1 == "BLUE", prop_win, 1 - prop_win)

xp_knn <- xp %>% mutate(classify_KNN1 = knn1, p_KNN1_ORANGE = prob)

g0 +
  geom_point(data = xp_knn, aes(x = x1, y = x2, color = classify_KNN1), pch = ".", alpha = 0.3) +
  stat_contour(data = xp_knn, aes(x = x1, y = x2, z = p_bayes_ORANGE), color = "purple", breaks = c(0, 0.5)) +
  stat_contour(data = xp_knn, aes(x = x1, y = x2, z = p_KNN1_ORANGE), color = "black", breaks = c(0, 0.5))


# Figure 2.16, right, K = 100:
library(class)
knn100 <- 
  knn(
    train = sample %>% select(x1, x2),
    test = xp %>% select(x1, x2),
    cl = sample$class,
    k = 100,
    prob = TRUE
  )
prop_win <- attr(knn100, "prob")  # Proportion of votes for winning class
prob <- ifelse(knn100 == "BLUE", prop_win, 1 - prop_win)

xp_knn <- xp %>% mutate(classify_KNN100 = knn100, p_KNN100_ORANGE = prob)

g0 +
  geom_point(data = xp_knn, aes(x = x1, y = x2, color = classify_KNN100), pch = ".", alpha = 0.3) +
  stat_contour(data = xp_knn, aes(x = x1, y = x2, z = p_bayes_ORANGE), color = "purple", breaks = c(0, 0.5)) +
  stat_contour(data = xp_knn, aes(x = x1, y = x2, z = p_KNN100_ORANGE), color = "black", breaks = c(0, 0.5))


# Figure 2.17 -----
n_obs_per_class_test <- 2500

sample_test <- 
  tibble(
    i = rep(1:n_obs_per_class_test, 2), 
    m_idx = sample(1:(n_m / 2), 2 * n_obs_per_class_test, replace = TRUE),
    class = rep(c("BLUE", "ORANGE"), each = n_obs_per_class_test),
    x1 = NA,
    x2 = NA
  )

# Sample individual observations:
for (cl in c("BLUE", "ORANGE")) {
  for (m_idx in m_indices) {
    m_counts <- table(sample_test %>% filter(class == cl) %>% pull(m_idx))
    sample_test[(sample_test$class == cl & sample_test$m_idx == m_idx), c("x1", "x2")] <- 
      rmvnorm(
        m_counts[m_idx],
        unlist(m[(m$class == cl & m$m_idx == m_idx), c("x1", "x2")]),
        diag(2) / 5)
  }
}

K_seq <- 1:100
KNN_error_rate <- tibble(K = K_seq, train = NA, test = NA)

for (i in seq_along(K_seq)) {
  if (!(i %% 10)) { print(i) }
  knn_train <- 
    knn(
      train = sample %>% select(x1, x2),
      test = sample %>% select(x1, x2),
      cl = sample$class,
      k = K_seq[i],
      prob = TRUE
    )  
  
  KNN_error_rate$train[i] <- mean(sample$class != knn_train)
  
  knn_test <- 
    knn(
      train = sample %>% select(x1, x2),
      test = sample_test %>% select(x1, x2),
      cl = sample$class,
      k = K_seq[i],
      prob = TRUE
    )  
  
  KNN_error_rate$test[i] <- mean(sample_test$class != knn_test)
}

KNN_error_rate %>%
  pivot_longer(c(train, test), names_to = "Type", values_to = "Error rate") %>%
  ggplot(aes(x = 1 / K, y = `Error rate`, color = Type)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(trans = "log2", breaks = c(0.01, 0.02, 0.05, 0.10, 0.20, 0.50, 1.00))