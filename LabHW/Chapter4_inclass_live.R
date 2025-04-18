# The *Default* data set -----

Default <- ISLR2::Default
summary(Default)

library(tidyverse)
library(plotly)

set.seed(1)
g1 <-
  Default %>%
  group_by(default) %>%
  slice_sample(n = 333, replace = FALSE) %>%
  plot_ly() %>%
  add_markers(x = ~balance, y = ~income, color = ~default)

g2 <- plot_ly(Default) %>% add_boxplot(x = ~default, y = ~balance, color = ~default)
g3 <- plot_ly(Default) %>% add_boxplot(x = ~default, y = ~income, color = ~default, legendgroup = "box")

g23 <- subplot(g2, g3, margin = 0.15, titleX = TRUE, titleY = TRUE) %>% layout(showlegend = FALSE)

subplot(g1, g23, margin = 0.05, titleX = TRUE, titleY = TRUE)


# Figure 4.2 -----
Default %>%
  mutate(default01 = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(x = balance, y = default01)) +
  geom_point(color = "orange", shape = "|") +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Balance") +
  ylab("Probability of Default") +
  theme_bw()

Default %>%
  mutate(default01 = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(x = balance, y = default01)) +
  geom_point(color = "orange", shape = "|") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  xlab("Balance") +
  ylab("Probability of Default") +
  theme_bw()

# Simple Logistic regression (Table 4.1) -----
# Default = beta0 + beta1 * Balance
fit_simple_balance <- glm(default ~ balance, family = "binomial", data = Default)
(summary_fit_simple_balance <- summary(fit_simple_balance))

est_beta_0 <- coef(fit_simple_balance)["(Intercept)"]
est_beta_1 <- coef(fit_simple_balance)["balance"]

# Predict log odds:
new_balance <- 2000
est_beta_0 + est_beta_1 * new_balance
predict(fit_simple_balance, newdata = tibble(balance = new_balance))

# Predict probability:
exp(est_beta_0 + est_beta_1 * new_balance) / (1 + exp(est_beta_0 + est_beta_1 * new_balance))
predict(fit_simple_balance, newdata = tibble(balance = new_balance), type = "response")



# Multiple Logistic regression (Table 4.3) -----
fit_default_multiple <- glm(default ~ ., family = "binomial", data = Default)
(summary_fit_default_multiple <- summary(fit_default_multiple))


new_data <- tibble(balance = 1800, student = "Yes", income = 1000)

# Predict log odds:
predict(fit_default_multiple, newdata = new_data)

# Predict probability:
predict(fit_default_multiple, newdata = new_data, type = "response")

# Confounding (Figure 4.3) -----

# Default = beta0 + beta1 * Student
fit_simple_student <- glm(default ~ student, family = "binomial", data = Default)
(summary_fit_simple_student <- summary(fit_simple_student))

fit_balance_student <- glm(default ~ balance + student, family = "binomial", data = Default)
(summary_fit_balance_student <- summary(fit_balance_student))

coef(summary_fit_balance_student)

bin_centers <- round(seq(min(Default$balance), 2500, length.out = 10), 0)
bin_width <- (bin_centers[2] - bin_centers[1]) 
breaks <- c(bin_centers - bin_width, max(bin_centers) + bin_width)

Default_rate <-
  Default %>%
  group_by(student) %>%
  mutate(
    default_01 = ifelse(default == "Yes", 1, 0),
    bin_center = cut(balance, breaks = breaks, labels = bin_centers)
    ) %>%
  group_by(bin_center, student) %>%
  summarize(
    default_rate = mean(default_01),
    n = n()
  ) %>%
  mutate(balance = as.numeric(levels(bin_center)[bin_center]))

(Default_rate_overall <-
  Default %>%
  mutate(default_01 = ifelse(default == "Yes", 1, 0)) %>%
  group_by(student) %>%
  summarize(overall_default_rate = mean(default_01)))
    
# Compare to predictions from simple regression:
predict(fit_simple_student, newdata = tibble(student = c("No", "Yes")), type = "response")

ggplot(Default_rate_overall) +
  geom_hline(data = Default_rate_overall, aes(yintercept = overall_default_rate, color = student), lty = 2) +
  xlab("Balance") +
  ylab("Default rate") +
  xlim(c(0, 2500)) +
  ylim(c(0, 1)) +
  theme_bw()
  

g <-
  Default_rate %>%
  ggplot(aes(x = balance, y = default_rate, color = student)) +
  geom_point() +
  geom_line() +
  geom_hline(data = Default_rate_overall, aes(yintercept = overall_default_rate, color = student), lty = 2) +
  xlab("Balance") +
  ylab("Default rate") +
  xlim(c(0, 2500)) +
  ylim(c(0, 1)) +
  theme_bw()

ggplotly(g)

ggplot(Default) +
  geom_boxplot(aes(x = student, y = balance, fill = student)) +
  coord_flip() +
  theme_bw()

# Note, the fact that on these bins the non-students have higher default 
# rates doesn't necessarily means it is so for other bin sizes.
# Try for example half of the current bin size.

# Multinomial logit -----

BrainCancer <- read_csv("BrainCancer.csv")

table(BrainCancer$diagnosis)

library(nnet)
(multi_logit_brain <- multinom(diagnosis ~ ki + gtv + sex, data = BrainCancer))
# We can see that 3 response classes (LG glioma, Meningioma, Other) have coefficients, 
# hence the HG Glioma class is treated as the Kth category. Each of the 3 classes has its own set
# of coefficients (intercept, ki, gtv, sexMale)

# There's a `predict` method too:
predict(multi_logit_brain, newdata = tibble(ki = 80, gtv = 7, sex = "Female"))
predict(multi_logit_brain, newdata = tibble(ki = 80, gtv = 7, sex = "Female"), type = "probs")


# LDA, p = 1 -----

# One predictor (X), one dimension (p = 1), 2 classes (K = 2), 
# normal distributions for each class (N(-1.5, 1), N(1.5, 1))

K <- 2  # y is either 1 or 2
pi <- c(0.3, 0.7)   # prior proabilities

mu <- c(-1.25, 1.25)  # mu1 = -1.25, mu2 = 1.25
sigma <- 1            # sigma1 = sigma2 := sigma = 1

# To generate samples, we first sample classes:
# "Step 1" in the lecture notes:
set.seed(1)
n <- 100
df <- 
  tibble(
    y = sample(1:K, size = n, prob = pi, replace = TRUE),
    x = NA
    )

table(df$y)
n1 <- unname(table(df$y)["1"])
n2 <- unname(table(df$y)["2"])
  

# Step 2: sample from the distrbiutions of each class:
df$x[df$y == 1] <- rnorm(n1, mean = mu[1], sd = sigma)
df$x[df$y == 2] <- rnorm(n2, mean = mu[2], sd = sigma)

rng <- range(df$x)

ggplot(df) +
  geom_point(aes(x = x, y = 0, color = as.character(y)), shape = "|", alpha = 0.6, size = 10) +
  geom_function(
    fun = ~ pi[1] * dnorm(.x, mean = mu[1], sd = sigma), 
    xlim = c(floor(rng[1]), ceiling(rng[2])), color = "darkgreen") +
  geom_function(
    fun = ~ pi[2] * dnorm(.x, mean = mu[2], sd = sigma),
    xlim = c(floor(rng[1]), ceiling(rng[2])), color = "magenta") +
  scale_color_manual(values = c("darkgreen", "magenta")) +
  ylab(element_blank()) +
  ylim(c(0, 0.3)) +
  xlim(c(-3, 4)) +
  theme_bw() +
  theme(legend.position = "none")
  

# We can think of the above as the training data. 
# The testing data will look like:

n1_test <- 25
n2_test <- 50
df_test <-
  tibble(
    x = 
      c(
        rnorm(n1_test, mu[1], sigma),
        rnorm(n2_test, mu[2], sigma)
      ),
    y = c(rep(1, n1_test), rep(2, n2_test))
  )

ggplot(df_test) +
  geom_point(aes(x = x, y = 0), shape = "|", alpha = 0.6, size = 10) +
  ylab(element_blank()) +
  theme_bw() +
  ylim(c(0, 0.3)) +
  xlim(c(-3, 4))


# How do we classify the above with the Bayes rule (where we know the true parameters)?

rng <- range(df_test$x)
ggplot(df_test) +
  geom_function(fun = ~ pi[1] * dnorm(.x, mean = mu[1], sd = sigma), xlim = c(floor(rng[1]), ceiling(rng[2])), color = "darkgreen") +
  geom_function(fun = ~ pi[2] * dnorm(.x, mean = mu[2], sd = sigma), xlim = c(floor(rng[1]), ceiling(rng[2])), color = "magenta") +
  geom_point(aes(x = x, y = 0), shape = "|", alpha = 0.6, size = 10) +
  ylab(element_blank()) +
  ylim(c(0, 0.3)) +
  xlim(c(-3, 4)) +
  theme_bw()

# If we knew the true parameters:
par(mar = c(0, 0, 0, 0))
par(mfrow = c(1, 1))
plot(imager::load.image("Chapter4_img01.png"), axes = FALSE) 

Bayes_decision_boundary <- (mu[1] + mu[2]) / 2  - log(pi[1]/pi[2]) * ( sigma^2 / (mu[1] - mu[2]) )

df_test$bayes_classification <- as.integer(df_test$x > Bayes_decision_boundary) + 1

ggplot(df_test) +
  geom_function(
    fun = ~ pi[1] * dnorm(.x, mean = mu[1], sd = sigma), 
    xlim = c(floor(rng[1]), ceiling(rng[2])), color = "darkgreen") +
  geom_function(
    fun = ~ pi[2] * dnorm(.x, mean = mu[2], sd = sigma),
    xlim = c(floor(rng[1]), ceiling(rng[2])), color = "magenta") +
  geom_point(aes(x = x, y = 0, color = x > Bayes_decision_boundary), shape = "|", alpha = 0.6, size = 10) +
  scale_color_manual(values = c("darkgreen", "magenta")) +
  geom_vline(xintercept = Bayes_decision_boundary, lty = 2) +
  ylab(element_blank()) +
  ylim(c(0, 0.3)) +
  xlim(c(-3, 4)) +
  theme_bw() + 
  theme(legend.position = "none")

# When we don't know the true parameters:
# Estimate pi1, pi2, mu1, mu2, sigma from the data:
plot(imager::load.image("Chapter4_img02.png"), axes = FALSE) 

N <- nrow(df)

ep <-
  df %>%
  rename(k = y) %>%
  group_by(k) %>%
  summarize(
    est_pi = n() / N,
    est_mu = mean(x),
    est_sigma_sq_k = sum( (x - est_mu) ^ 2 )
  ) %>%
  mutate(est_sigma_sq = sum(est_sigma_sq_k) / (N - K))


LDA_decision_boundary <- 
  (ep$est_mu[1] + ep$est_mu[2]) / 2 - log(ep$est_pi[1] / ep$est_pi[2]) * 
  ep$est_sigma_sq[1] / (ep$est_mu[1] - ep$est_mu[2])

df_test$LDA <- as.integer(df_test$x > LDA_decision_boundary) + 1

ggplot(df_test) +
  geom_function(fun = ~ pi[1] * dnorm(.x, mean = mu[1], sd = sigma), xlim = c(floor(rng[1]), ceiling(rng[2])), color = "darkgreen") +
  geom_function(fun = ~ pi[2] * dnorm(.x, mean = mu[2], sd = sigma), xlim = c(floor(rng[1]), ceiling(rng[2])), color = "magenta") +
  scale_color_manual(values = c("darkgreen", "magenta")) +
  geom_vline(xintercept = Bayes_decision_boundary, lty = 2) +
  geom_function(fun = ~ ep$est_pi[1] * dnorm(.x, mean = ep$est_mu[1], sd = sqrt(ep$est_sigma_sq)), xlim = c(floor(rng[1]), ceiling(rng[2])), color = "darkgreen", lty = 2) +
  geom_function(fun = ~ ep$est_pi[2] * dnorm(.x, mean = ep$est_mu[2], sd = sqrt(ep$est_sigma_sq)), xlim = c(floor(rng[1]), ceiling(rng[2])), color = "magenta", lty = 2) +
  geom_vline(xintercept = LDA_decision_boundary, lty = 2, color = "blue") +
  geom_point(aes(x = x, y = 0, color = x > LDA_decision_boundary), shape = "|", alpha = 0.6, size = 10) +
  ylab(element_blank()) +
  ylim(c(0, 0.3)) +
  xlim(c(-3, 4)) +
  theme_bw() + 
  theme(legend.position="none")




# Compare error rates:
# Compare error rates:
(Bayes_error_rate <- mean(df_test$bayes_classification != df_test$y))
(LDA_error_rate <- mean(df_test$LDA != df_test$y))

# Estimate posterior probability from estimates of pi1, pi2, mu1, mu2, sigma:
posterior_prob_y <- function(x, estimated_kernel_parameters) {
  K <- nrow(estimated_kernel_parameters)
  p <- matrix(0, nrow = length(x), ncol = 1 + K + 1 + K + 1)
  colnames(p) <- c("x", str_c("f", 1:K), "const", str_c("p", 1:K), "tot_p")
  p <- as_tibble(p)
  p$x <- x
  for (k in 1:K) {
    p[ , paste0("f", k)] <-
      dnorm(
        x, 
        mean = estimated_kernel_parameters$est_mu[k],
        sd = sqrt(estimated_kernel_parameters$est_sigma_sq[k])
      )
    p$const <- 
      p$const + estimated_kernel_parameters$est_pi[k] * p[ , paste0("f", k), drop = TRUE]
  }
  for (k in 1:K) {
    p[ , paste0("p", k)] <- 
      estimated_kernel_parameters$est_pi[k] * p[ , paste0("f", k)] / p$const
    p$tot_p <- p$tot_p + p[ , paste0("p", k), drop = TRUE]
  }
  stopifnot(all.equal(p$tot_p, rep(1, nrow(p))))
  print(p)
  return(p %>% select(-tot_p))
}

estimated_kernel_parameters <- ep
p <- posterior_prob_y(x = c(-1, 1), estimated_kernel_parameters)

# MASS::lda function -----
lda_fit <- MASS::lda(y ~ x, data = df)
# See
# https://stackoverflow.com/questions/68307682/r-lda-linear-discriminant-analysis-how-to-get-compute-lda-scores-from-lda-co
# For an explanation of "Coefficients of linear discriminants"

plot(lda_fit)
predict(lda_fit, df)

# Illustrate multivariate normal distribution -----

mu1 <- matrix(c(-1, -1), ncol = 1)
mu2 <- matrix(c( 1,  1), ncol = 1)

Sigma <- matrix(c(3, 0.5, 0.5, 1), nrow = 2)

# Plotting the bivariate normal density -----

mu <- c(0, 0)
rho12 <- 0.75
sigma <- matrix(c(1, rho12, rho12, 1), nrow = 2)

# With plotly:
x1 <- seq(-3, 3, 0.05)
x2 <- seq(-3, 3, 0.05)
f <- function(x1, x2, mu, sigma) mvtnorm::dmvnorm(cbind(x1, x2), mu, sigma)
density <- outer(x1, x2, f, mu = mu, sigma = sigma)

plotly::plot_ly(x = ~x1, y = ~x2, z = ~density, type = "surface")

# Automatic contour:
plotly::plot_ly(x = ~x1, y = ~x2, z = ~density, type = "contour")

# Or with ggplot:
df <- expand_grid(x1 = x1, x2 = x2)
df$density <- mvtnorm::dmvnorm(df, mu, sigma)

(g <- ggplot(df, aes(x = x1, y = x2)) + geom_contour(aes(z = density)) + theme_bw() + theme(aspect.ratio = 1))


# The mvtnorm package includes the density and CDF functions of the multivariate normal
# distribution, as well as a function that generates samples from it.

library(mvtnorm)
bivariate_sample <- rmvnorm(500, mu, sigma = sigma)  # generate multivariate normal samples.
colnames(bivariate_sample) <- c("x1", "x2")

g + geom_point(data = as_tibble(bivariate_sample), aes(x = x1, y = x2), color = rgb(1, 0, 0, 0.5))


# LDA, p = 2 -----

K <- 2

mu1 <- matrix(c(-1, -1), ncol = 1)
mu2 <- matrix(c( 1,  1), ncol = 1)

Sigma <- matrix(c(3, 0.5, 0.5, 1), nrow = 2)

pi <- c(0.3, 0.7)

df <- 
  expand_grid(
    x1 = seq(-5, 5, 0.01),
    x2 = seq(-4, 4, 0.01)
  ) %>%
  mutate(
    lhs1 = mvtnorm::dmvnorm(x = cbind(x1, x2), mean = c(mu1), sigma = Sigma, log = TRUE),
    lhs2 = mvtnorm::dmvnorm(x = cbind(x1, x2), mean = c(mu2), sigma = Sigma, log = TRUE),
    R = factor(ifelse(lhs1 - lhs2 >= log((pi[2] / pi[1])), 1, 2))
  )

(g <- 
    ggplot(df) +
    geom_point(aes(x = x1, y = x2, color = R), pch = "."))


(g <- 
    ggplot(df) +
    geom_point(aes(x = x1, y = x2, color = R), pch = ".", alpha = 0.02) +
    geom_contour(aes(x = x1, y = x2, z = exp(lhs1)), color = "red", binwidth = 0.02) +
    geom_contour(aes(x = x1, y = x2, z = exp(lhs2)), color = "blue", binwidth = 0.02))


# In R we use %*% for matrix multiplication 
# Note that you could still multiply matrices of the same dimensions with *
# but this will simply be an element by element multiplication. 

a <- t(mu1 - mu2) %*% solve(Sigma)
m <- log((pi[2] / pi[1])) + 0.5 * a %*% (mu1 + mu2)

(g <-
    g + 
    geom_abline(slope = -a[1] / a[2], intercept = m / a[2]) + 
    theme(
      legend.position = "none",
      aspect.ratio = 1
    ))

# Sample bivariate data from two classes:
set.seed(2)

# First step: sample classes according to prior probabilities:
n <- 50
df_sample <- 
  tibble(
    y = sample(K, size = n, prob = pi, replace = TRUE),
    x1 = NA,
    x2 = NA
  )

table(df_sample$y)
n1 <- unname(table(df_sample$y)["1"])
n2 <- unname(table(df_sample$y)["2"])

library(mvtnorm)
df_sample[df_sample$y == 1, c("x1", "x2")] <- rmvnorm(n1, mean = mu1, sigma = Sigma)
df_sample[df_sample$y == 2, c("x1", "x2")] <- rmvnorm(n2, mean = mu2, sigma = Sigma)

# Estimate parameters:
est_pi <- c(n1 / n, n2 / n)

est_mu1 <- matrix(colMeans(df_sample[df_sample$y == 1, ])[c("x1", "x2")], ncol = 1)
est_mu2 <- matrix(colMeans(df_sample[df_sample$y == 2, ])[c("x1", "x2")], ncol = 1)

ssq_1 <- (n1 - 1) * var(df_sample %>% filter(y == 1) %>% select(x1, x2))
ssq_2 <- (n2 - 1) * var(df_sample %>% filter(y == 2) %>% select(x1, x2))

est_Sigma <- (ssq_1 + ssq_2) / (n - K)

LDA_a <- t(est_mu1 - est_mu2) %*% solve(est_Sigma)
LDA_m <- log((est_pi[2] / est_pi[1])) + 0.5 * LDA_a %*% (est_mu1 + est_mu2)

g + 
  geom_point(
    data = df_sample %>% filter(between(x1, -5, 5)),
    aes(x = x1, y = x2, color = as.character(y)),
    shape = "X"
  ) +
  geom_abline(slope = -LDA_a[1] / LDA_a[2], intercept = LDA_m / LDA_a[2], lty = 2) + 
  scale_color_manual(values = c("red", "blue"))

# MASS::lda function -----
lda_fit <- MASS::lda(y ~ x1 + x2, data = df_sample)
plot(lda_fit)
predict(lda_fit, df_sample)

# LDA for Default data -----
Default <- ISLR::Default
lda_default <- MASS::lda(default ~ balance + student, data = Default)
lda_default_predictions <- predict(lda_default)

# Confusion matrix -----
C <-
  bind_cols(
    true_class = Default$default,
    pred_class = lda_default_predictions$class
  )

C %>% 
  group_by(true_class) %>%
  count(pred_class)

# Or:
C %>% 
  group_by(true_class) %>%
  count(pred_class) %>%
  pivot_wider(names_from = "true_class", values_from = "n")

# Or:
confusion_matrix <- as.matrix(table(C$pred_class, C$true_class))
rowSums(confusion_matrix)
sum(rowSums(confusion_matrix))

colSums(confusion_matrix)
sum(colSums(confusion_matrix))

# Change threshold for classification:

# The posterior probabilities can be taken from:
post_prob <- as_tibble(lda_default_predictions$posterior)

# And then we can classify manually by, say:
classify_0.2 <- ifelse(post_prob$Yes > 0.2, "Yes", "No")

confusion_matrix0.2 <- as.matrix(table(classify_0.2, C$true_class))
false_positive <- 
  confusion_matrix0.2["Yes", "No"] / sum(confusion_matrix0.2[ , "No"])
false_negative <- 
  confusion_matrix0.2["No", "Yes"] / sum(confusion_matrix0.2[ , "Yes"])

# Compare error rates (Figure 4.7) -----

Default <- ISLR::Default
lda_default <- MASS::lda(default ~ balance + student, data = Default)
lda_default_predictions <- predict(lda_default)
post_prob <- as_tibble(lda_default_predictions$posterior)

thresholds <- seq(0, 1, 0.05)

error_rates <- 
  tibble(
    threshold = thresholds, 
    `Overall Error` = NA,
    `False Positive` = NA,
    `False Negative` = NA
  )

for(i in seq_along(thresholds)) {
  classify <- 
    factor(
      ifelse(post_prob$Yes > thresholds[i], "Yes", "No"), 
      levels = c("Yes", "No")
    )
  confusion_matrix <- as.matrix(table(classify, Default$default))
  error_rates$`Overall Error`[i] <-
    ( confusion_matrix["Yes", "No"] + confusion_matrix["No", "Yes"] ) / sum(confusion_matrix)
  error_rates$`False Positive`[i] <- 
    confusion_matrix["Yes", "No"] / sum(confusion_matrix[ , "No"])
  error_rates$`False Negative`[i] <- 
    confusion_matrix["No", "Yes"] / sum(confusion_matrix[ , "Yes"])  
}

min_overall_error_rate <- 
  error_rates %>% 
  filter(`Overall Error` == min(`Overall Error`)) %>%
  select(threshold, value = `Overall Error`) %>%
  mutate(`Error Type` = "Overall Error")

(g <-
    error_rates %>%
    pivot_longer(
      c(`Overall Error`, `False Positive`, `False Negative`),
      names_to = "Error Type"
    ) %>%
    ggplot() +
    geom_line(aes(x = threshold, y = value, color = `Error Type`)) +
    geom_point(
      data = min_overall_error_rate, 
      aes(x = threshold, y = value, color = `Error Type`)
    ) +
    theme_bw() +
    ylab(element_blank()))

ggplotly(g)

# ROC, AUC -----
library(pROC)
default_roc <- 
  roc(response = Default$default, predictor = post_prob %>% pull("Yes"))
auc(default_roc)
plot(default_roc, col = "blue")


# QDA, p = 2 -----

mu1 <- matrix(c(-1, -1), ncol = 1)
mu2 <- matrix(c( 1,  1), ncol = 1)

Sigma1 <- matrix(c(3, 0.5, 0.5, 1), nrow = 2)
Sigma2 <- matrix(c(5, -0.5, -0.5, 1), nrow = 2)

pi <- c(0.5, 0.5)

# l <- -5
# u <-  8
l <- -10
u <-  30

df <- 
  expand_grid(
    x1 = seq(l, u, length.out = 250),
    x2 = seq(l, u, length.out = 250)
  ) %>%
  mutate(
    lhs1 = mvtnorm::dmvnorm(x = cbind(x1, x2), mean = c(mu1), sigma = Sigma1, log = TRUE),
    lhs2 = mvtnorm::dmvnorm(x = cbind(x1, x2), mean = c(mu2), sigma = Sigma2, log = TRUE),
    R = factor(ifelse(lhs1 - lhs2 >= log(pi[2] / pi[1]), 1, 2))
  )


(g <- 
    ggplot(df) +
    geom_point(aes(x = x1, y = x2, color = R), pch = "."))

(g <- 
    ggplot(df) +
    geom_point(aes(x = x1, y = x2, color = R), pch = ".") +
    geom_contour(aes(x = x1, y = x2, z = exp(lhs1)), color = "red", alpha = 0.4) +
    geom_contour(aes(x = x1, y = x2, z = exp(lhs2)), color = "blue", alpha = 0.4))

# Compute the parabola of the Bayes decision boundary:
invSigma1 <- solve(Sigma1)
invSigma2 <- solve(Sigma2)

A <- invSigma1 - invSigma2
B <- t(mu1) %*% invSigma1 - t(mu2) %*% invSigma2
k <- c((1 / 2) * log(det(Sigma1) / det(Sigma2)) + (1 / 2) * (t(mu1) %*% invSigma1 %*% mu1 - t(mu2) %*% invSigma2 %*% mu2))
C <- log((pi[2] / pi[1])) + k

a <- -A[2, 2] / 2

df2 <- 
  tibble(x1 = seq(l, u, length.out = 100)) %>%
  mutate(
    b = B[2] - A[1, 2] * x1,
    c = - (1 / 2) * A[1, 1] * x1^2 + B[1] * x1 - C,
    d = sqrt(b^2 - 4 * a * c),
    x21 = (-b + d) / (2 * a),
    x22 = (-b - d) / (2 * a)
  )

g + 
  geom_line(data = df2, aes(x = x1, y = x21)) +
  geom_line(data = df2, aes(x = x1, y = x22)) +
  ylim(c(l, u)) +
  theme_bw() +
  theme(legend.position = "none")

# Now try this with l = -10, u = 30

# MASS::qda function -----
Default <- ISLR::Default
qda_default <- MASS::qda(default ~ balance + student, data = Default)
qda_default_predictions <- predict(qda_default)



# Naive Bayes -----

# Independent Gaussian predictors:
library(e1071)
nb_gaussian <- naiveBayes(default ~ balance + student, data = Default)
predict(nb_gaussian, newdata = Default)

# Kernel density estimators:
library(naivebayes)
Default_predictor_matrix <- 
  Default %>% 
  select(balance, student) %>%
  mutate(student = ifelse(student == "Yes", 1, 0)) %>%
  as.matrix()

nb_KDE <- 
  nonparametric_naive_bayes(
    y = Default %>% pull(default),
    x = Default_predictor_matrix
    )

predict(nb_KDE)
plot(nb_KDE, "balance", prob = "conditional")
plot(nb_KDE, "student", prob = "conditional")

# Change kernel (default is Gaussian):
nb_KDE_rectangular <- 
  nonparametric_naive_bayes(
    y = Default %>% pull(default),
    x = Default_predictor_matrix,
    kernel = "rectangular"
  )

predict(nb_KDE_rectangular)
plot(nb_KDE_rectangular, "balance", prob = "conditional")
plot(nb_KDE_rectangular, "student", prob = "conditional")

# An empirical comparison -----

# Generative model for Scenario 1:
set.seed(1)
K <- 2
p <- 2
pi <- rep(1 / p, p)

library(tidyverse)
library(mvtnorm)

sample_data_scenario_1 <- function(n, mu, Sigma) {
  # Sample number of observations per class:
  class_sizes <- c(rmultinom(1, n, pi))
  
  # Sample predictors for each class:
  df_sample <- NULL
  for (k in 1:K) {
    predictors_matrix_k <- rmvnorm(class_sizes[k], mean = mu[k, ], sigma = Sigma[[k]])
    colnames(predictors_matrix_k) <- str_c("x", 1:p)
    df_sample_k <- as_tibble(predictors_matrix_k) %>% mutate(y = k - 1)
    df_sample <- bind_rows(df_sample, df_sample_k)
  }
  
  return(df_sample)
}

mu <- 
  matrix(
    c(
      -0.25, -0.25,      # mu1
      0.75,  0.75       # mu2
    ),
    nrow = 2,
    byrow = TRUE
  )

shared_Sigma <- diag(2)
Sigma <- list(shared_Sigma, shared_Sigma)

# Testing data:
df_testing <- sample_data_scenario_1(1000, mu = mu, Sigma = Sigma)

# Training and fitting:
library(e1071)
library(naivebayes)
library(class)

M <- 200
n_train <- 20
classification_errors <- 
  tibble(
    m = 1:M, 
    LDA = NA, QDA = NA, 
    Logistic = NA, Logistic2 = NA,
    Naive_Bayes_G = NA, Naive_Bayes_KG = NA, Naive_Bayes_KR = NA,
    KNN1 = NA, KNN10 = NA
  )

for (m in 1:M) {
  df_training <- sample_data_scenario_1(n_train, mu = mu, Sigma = Sigma)
  
  lda_fit <- MASS::lda(y ~ ., data = df_training)
  pred_testing <- predict(lda_fit, newdata = df_testing)
  classification_errors$LDA[m] <- table(df_testing$y == pred_testing$class)["FALSE"] / nrow(df_testing)
  
  qda_fit <- MASS::qda(y ~ ., data = df_training)
  pred_testing <- predict(qda_fit, newdata = df_testing)
  classification_errors$QDA[m] <- table(df_testing$y == pred_testing$class)["FALSE"] / nrow(df_testing)
  
  logistic_fit <- glm(y ~ ., data = df_training, family = "binomial")
  pred_testing <- as.integer(predict(logistic_fit, newdata = df_testing, type = "response") > 0.5)
  classification_errors$Logistic[m] <- table(df_testing$y == pred_testing)["FALSE"] / nrow(df_testing)
  
  logistic2_formula <- 
    as.formula(
      str_c(
        "y",
        str_c(
          str_c("x", 1:p, collapse = " + "),
          str_c(apply(combn(str_c("x", 1:p), 2), 2, str_c, collapse = "*"), collapse = " + "),
          str_c("I(x", 1:p, "^2)", collapse = " + "),
          sep = " + "
        ),
        sep = " ~ "
      )
    )
  logistic2_fit <- glm(logistic2_formula, data = df_training, family = "binomial")
  pred_testing <- as.integer(predict(logistic2_fit, newdata = df_testing, type = "response") > 0.5)
  classification_errors$Logistic2[m] <- table(df_testing$y == pred_testing)["FALSE"] / nrow(df_testing)
  
  qda_fit <- MASS::qda(y ~ ., data = df_training)
  
  nb_gaussian <- naiveBayes(y ~ ., data = df_training)
  pred_testing <- predict(nb_gaussian, newdata = df_testing)
  classification_errors$Naive_Bayes_G[m] <- table(df_testing$y == pred_testing)["FALSE"] / nrow(df_testing)
  
  # Kernel density estimators:
  nb_KG <- 
    nonparametric_naive_bayes(
      y = as.character(df_training$y),
      x = as.matrix(df_training[ , str_c("x", 1:p)])
    )
  pred_testing <- predict(nb_KG, newdata = as.matrix(df_testing[ , str_c("x", 1:p)]))
  classification_errors$Naive_Bayes_KG[m] <- table(df_testing$y == pred_testing)["FALSE"] / nrow(df_testing)
  
  nb_KR <- 
    nonparametric_naive_bayes(
      y = as.character(df_training$y),
      x = as.matrix(df_training[ , str_c("x", 1:p)]),
      kernel = "rectangular"
    )
  pred_testing <- predict(nb_KR, newdata = as.matrix(df_testing[ , str_c("x", 1:p)]))
  classification_errors$Naive_Bayes_KR[m] <- table(df_testing$y == pred_testing)["FALSE"] / nrow(df_testing)
  
  pred_testing <- 
    knn(
      train = df_training[ , str_c("x", 1:p)], 
      test = df_testing[ , str_c("x", 1:p)], 
      cl = df_training$y,
      k = 1
    )
  classification_errors$KNN1[m] <- table(df_testing$y == pred_testing)["FALSE"] / nrow(df_testing)
  
  pred_testing <- 
    knn(
      train = df_training[ , str_c("x", 1:p)], 
      test = df_testing[ , str_c("x", 1:p)], 
      cl = df_training$y,
      k = 10
    )
  classification_errors$KNN10[m] <- table(df_testing$y == pred_testing)["FALSE"] / nrow(df_testing)
}

classification_errors_long <- 
  pivot_longer(
    classification_errors,
    cols = -m,
    names_to = "Classifier",
    values_to = "Error rate"
  ) %>%
  mutate(
    Classifier = 
      factor(
        Classifier, 
        levels = 
          c(
            "KNN1", "KNN10",
            "LDA", "Logistic",
            "Naive_Bayes_G", "Naive_Bayes_KG", "Naive_Bayes_KR",
            "QDA", "Logistic2"
          )
      ),
    group = 
      case_when(
        Classifier %in% c("LDA", "Logistic") ~ "Linear model",
        Classifier %in% c("QDA", "Logistic2") ~ "Quadratic model",
        Classifier %in% c("KNN1", "KNN10") ~ "KNN",
        Classifier %in% c("Naive_Bayes_G", "Naive_Bayes_KG", "Naive_Bayes_KR") ~ "Naive Bayes"
      ),
    group = factor(group, levels = c("KNN", "Linear model", "Naive Bayes", "Quadratic model"))
  )

classification_errors_long %>%
  ggplot() +
  geom_boxplot(aes(x = Classifier, y = `Error rate`, fill = group)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.title = element_blank()
    )

