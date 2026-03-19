## Problem 3c

# Data:
# Y1,.:
Y1 <- c(8.5, 8.0, 7.5, 9.0, 8.0, 8.0, 7.0, 8.5, 7.5)
# Y2,.:
Y2 <- c(6.5, 6.0, 5.5)

# We have the var. of each full cond. ddist. on mu1, mu2, and mu are const.
# so we specify them here (see 3a, 3b for derivations):
var_mu1 <- 1 / (1 / 0.1 + length(Y1) / 1)
var_mu2 <- 1 / (1 / 0.1 + length(Y2) / 1)
var_mu <- 1 / (1 / 0.1 + 1 / 0.1 + 1 / 4)
# We also precompute the standard deviation as that is what we use each iter.
sd_mu1 <- sqrt((var_mu1))
sd_mu2 <- sqrt((var_mu2))
sd_mu <- sqrt((var_mu))
# We then have rem. of params. on full cond. dists. are dep. on mu, mu1, mu2:
mean_mu1 <- function(mu) {(mu / 0.1 + sum(Y1) / 1) * var_mu1}
mean_mu2 <- function(mu) {(mu / 0.1 + sum(Y2) / 1) * var_mu2}
mean_mu <- function(mu1, mu2) {(mu1 / 0.1 + mu2 / 0.1 + 0 / 4) * var_mu}

# We now use vector mus = (mu1, mu2, mu) and define our iter functions.
# Input: mus - Old mus vector
# Output: New mu_1 sample
iter_mu1 <- function(mus) {rnorm(1, mean = mean_mu1(mus[3]), sd = sd_mu1)}
# Similar idea for other two
iter_mu2 <- function(mus) {rnorm(1, mean = mean_mu2(mus[3]), sd = sd_mu2)}
iter_mu <- function(mus) {rnorm(1, mean = mean_mu(mus[1], mus[2]), sd = sd_mu)}

# We first sample new mu_1 and new mu_2 with old mu then sample new mu with new
# mu_1 and mu_2 for no good reason. Maybe I should try swapping {mu_1, mu_2}
# sampling and mu sampling order? hm.
# Input: mus - Vector (mu_1, mu_2, mu) of old values
# Output: New sample of mus
iter <- function(mus){
    mus[1] <- iter_mu1(mus)
    mus[2] <- iter_mu2(mus)
    mus[3] <- iter_mu(mus)
    mus
}

# Initialize:
mus <- c(mean(Y1), mean(Y2), 0)
length <- 10000
mus_history <- matrix(0, nrow = 3, ncol = length * 2)
mus_history[, 1] <-  mus

# Iterate:
for (i in 2:(length * 2)) {
    mus_old <- mus_history[, i - 1]
    mus_new <- iter(mus_old)
    mus_history[, i] = mus_new
}

# Account for burnin:
mus_samples <- mus_history[, length:(length * 2)]

# Give credibility intervals:
namelist <- c("_1", "_2", "")
for (i in 1:3) {
    quant <- quantile(mus_samples[i, ], probs = c(0.025, 0.975))
    cat(sprintf("The empirical 95%% credibility interval for mu%s is (%f, %f)\n",
                namelist[i], quant[1], quant[2]))
}

# We have E[mu_1-mu]~1/n*\sum_i^n(mu_{1i}-\mu_i)=mean(mu_1)-mean(mu) and sim for
# E[mu_2-mu].
# Estimate posterior expectectations:
mu_mean <- mean(mus_samples[3, ])
for (i in 1:2) {
    mean <- mean(mus_samples[i, ]) - mu_mean
    cat(sprintf("E[mu%s-mu] is (%f)\n", namelist[i], mean))
}