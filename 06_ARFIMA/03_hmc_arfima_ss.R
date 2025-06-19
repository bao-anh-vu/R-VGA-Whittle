## HMC-Whittle for ARFIMA-state space model

rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(mvtnorm)
library(cmdstanr)
library(ggplot2)
library(gridExtra)
library(fracdiff)
# library(LSTS)

# source("./source/arfima_hmc.stan")
source("./source/compute_periodogram.R")
source("./source/compute_arfima_spec_dens.R")
source("./source/fit_mle_arfima_ss.R")
source("./source/find_optimal_nu.R")

## Flags
date <- "20250514"
noise_dist <- "t" # "t" or "gaussian"
save_hmc_results <- T

## Read data
data_dir <- "./data/"
n <- 3000
arfima_data <- readRDS(paste0(data_dir, "arfima_data_n", n, "_", noise_dist, ".rds"))
y <- arfima_data$y
phi <- arfima_data$phi
theta <- arfima_data$theta
d <- arfima_data$d
sigma_eta <- arfima_data$sigma_eta
nu <- arfima_data$nu

## Simulate ARFIMA(1, 0.25, 1) process
# set.seed(2025)
# n <- 5000
# phi <- 0.3
# theta <- 0.7
# d <- 0.25
# sigma_eta <- 1

# if (noise_dist == "t") {
#   nu <- 10 # degrees of freedom for t-distribution
# } else {
#   nu <- 0.2 # standard deviation for Gaussian noise
# }

# x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series
# y <- x + rt(n, df = nu) # ARFIMA + noise

# if (noise_dist == "t") {
#   y <- x + rt(n, df = nu) # ARFIMA + noise
# } else {
#   y <- x + rnorm(n, mean = 0, sd = nu) # ARFIMA + noise
# }

## Test spectral density
# test_x <- spectral.density(ar = phi, ma = theta, d = d, sd = sigma_eta, lambda = freq)
# test_eps <- 1/(2*pi) * (nu/(nu - 2)) # nu = 10
# head(test_x + test_eps)

## Result directory
result_dir <- "./results/"
hmc_filepath <- paste0(result_dir, "hmc_results_n", n, "_", noise_dist,
                       "_", date, ".rds")

  
## HMC-Whittle parameters 
n_chains <- 1
n_post_samples <- 10000
burn_in <- 5000

## Compute periodogram
pgram_output <- compute_periodogram(y)
freq <- pgram_output$freq
I <- pgram_output$periodogram

## MLE
# mle <- fit_mle_arfima_ss(pdg = I, freq = freq, noise_dist = noise_dist)

# ## Prior parameters
# mle_phi <- mle$par[1]
# mle_theta <- mle$par[2]
# mle_d <- mle$par[3]
# mle_sigma_eta <- mle$par[4]
# mle_nu <- mle$par[5]

# if (noise_dist == "t") {
#   prior_mean_nu <- log(mle_nu - 2)
#   prior_var_nu <- 0.5 # variance for nu
# } else {
#   prior_mean_nu <- log(mle_nu^2)
#   prior_var_nu <- 0.5 # variance for nu
# }
# # prior_mean <- c(tanh(mle_phi), tanh(mle_theta), 0.5*tanh(mle_d), sqrt(exp(mle_sigma_eta)), sqrt(exp(mle_nu)))
# prior_mean <- c(atanh(mle_phi), atanh(mle_theta), atanh(2*mle_d), log(mle_sigma_eta^2), prior_mean_nu)
# diag_prior_var <- c(rep(0.5, 4), prior_var_nu) # identity matrix

# mle <- find_optimal_nu(pdg = I, freq = freq, noise_dist = noise_dist)
mle <- fit_mle_arfima_ss1(pdg = I, freq = freq, noise_dist = noise_dist)$par

## Prior parameters
mle_phi <- mle[1]
mle_theta <- mle[2]
mle_d <- mle[3]
mle_sigma_eta <- mle[4]
mle_nu <- mle[5]

if (noise_dist == "t") {
prior_mean_nu <- log(mle_nu - 2)
prior_var_nu <- 0.5 # variance for nu
} else {
  prior_mean_nu <- log(mle_nu^2)
  prior_var_nu <- 0.5 # variance for nu
}

# prior_mean <- c(tanh(mle_phi), tanh(mle_theta), 0.5*tanh(mle_d), sqrt(exp(mle_sigma_eta)), sqrt(exp(mle_nu)))
prior_mean <- c(atanh(mle_phi), atanh(mle_theta), atanh(2*mle_d), log(mle_sigma_eta^2), prior_mean_nu)
diag_prior_var <- c(rep(0.5, 4), prior_var_nu) 


## Simulate from prior
prior_samples <- rmvnorm(10000, prior_mean, diag(diag_prior_var))
phi_samples <- tanh(prior_samples[, 1])
theta_samples <- tanh(prior_samples[, 2])
d_samples <- 0.5 * tanh(prior_samples[, 3]) 
sigma_eta_samples <- sqrt(exp(prior_samples[, 4]))

if (noise_dist == "t") {
  nu_samples <- 2 + exp(prior_samples[, 5])
} else {# gaussian
  nu_samples <- sqrt(exp(prior_samples[, 5]))
}

png("./plots/prior_hmc.png", width = 800, height = 600)
par(mfrow = c(2, 3))
plot(density(phi_samples), main = "phi", xlim = c(-1, 1))
abline(v = phi, col = "red", lty = 2)
plot(density(theta_samples), main = "theta", xlim = c(-1, 1))
abline(v = theta, col = "red", lty = 2)
plot(density(d_samples), main = "d", xlim = c(0, 1))
abline(v = d, col = "red", lty = 2)
plot(density(sigma_eta_samples), main = "sigma_eta")
abline(v = sigma_eta, col = "red", lty = 2) 
plot(density(nu_samples), main = "nu")
abline(v = nu, col = "red", lty = 2)
dev.off()

browser()

##########################
##          HMC         ##
##########################

hmc_stan_file <- "./source/arfima_hmc.stan"

hmc_arfima_model <- cmdstan_model(
    hmc_stan_file,
    cpp_options = list(stan_threads = TRUE)
)

hmc_arfima_data <- list(N = length(y), y = y, K = 20, #mu = 0,
                        use_t_noise = ifelse(noise_dist == "t", 1, 0),
                        prior_mean = prior_mean, diag_prior_var = diag_prior_var)

fit_hmc_arfima <- hmc_arfima_model$sample(
    hmc_arfima_data,
    chains = n_chains,
    threads = parallel::detectCores(),
    parallel_chains = n_chains,
    refresh = 500,
    iter_warmup = burn_in,
    iter_sampling = n_post_samples
)

hmc_results <- list(draws = fit_hmc_arfima$draws(variables = c("phi", "theta", "d", "sigma_eta", "nu")),
                    time = fit_hmc_arfima$time,
                    summary = fit_hmc_arfima$cmdstan_summary)

# fit_hmc_arfima$cmdstan_summary()
# fit_hmc_arfima$diagnostic_summary()

if (save_hmc_results) {
    saveRDS(hmc_results, hmc_filepath)
}


hmc.phi <- c(hmc_results$draws[,,1])
hmc.theta <- c(hmc_results$draws[,,2])
hmc.d <- c(hmc_results$draws[,,3])
hmc.sigma_eta <- c(hmc_results$draws[,,4])
hmc.nu <- c(hmc_results$draws[,,5])

hmc_df <- data.frame(
    phi = hmc.phi,
    theta = hmc.theta,
    d = hmc.d,
    sigma_eta = hmc.sigma_eta,
    nu = hmc.nu
)

## Plot posterior distribution for each parameter

plots <- list()

param_names <- c("phi", "theta", "d", "sigma_eta", "nu")
param_values <- c(phi, theta, d, sigma_eta, nu)
for (p in 1:length(param_names)) {
    true_vals_df <- data.frame(name = param_names[p], val = param_values[p])

    plot <- ggplot(hmc_df, aes(x = .data[[param_names[p]]])) +
        geom_density(col = "red", lwd = 1) +
        geom_density(data = hmc_df, col = "goldenrod", lwd = 1) +
        geom_vline(
            data = true_vals_df, aes(xintercept = val),
            color = "black", linetype = "dashed", linewidth = 1
        ) +
        # labs(x = vars) +
        theme_bw() +
        # theme(axis.title = element_blank(), text = element_text(size = 24)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 4))

    plots[[p]] <- plot
}


png(paste0("./plots/hmc_posterior_", noise_dist, ".png"), width = 1000, height = 600)
grid.arrange(grobs = plots, ncol = 3)
dev.off()

plot_range <- 1:length(hmc.phi) #10000
png(paste0("./plots/hmc_trace_", noise_dist, ".png"), width = 1000, height = 600)
par(mfrow = c(3, 2))
plot(hmc.phi[plot_range], type = "l", main = "phi", ylab = "phi")
abline(h = phi, col = "red", lwd = 2, lty = 2)
plot(hmc.theta[plot_range], type = "l", main = "theta", ylab = "theta")
abline(h = theta, col = "red", lwd = 2, lty = 2)
plot(hmc.d[plot_range], type = "l", main = "d", ylab = "d")
abline(h = d, col = "red", lwd = 2, lty = 2)
plot(hmc.sigma_eta[plot_range], type = "l", main = "sigma_eta", ylab = "sigma_eta")
abline(h = sigma_eta, col = "red", lwd = 2, lty = 2)
plot(hmc.nu[plot_range], type = "l", main = "nu", ylab = "nu")
abline(h = nu, col = "red", lwd = 2, lty = 2)
dev.off()
