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

## Flags
date <- "20250514"
save_hmc_results <- T

## Simulate ARFIMA(1, d, 1) process
set.seed(2025)

# Simulate ARMA(1,1) with ar = 0.9, ma = 0.5
n <- 10000
phi <- 0.3
theta <- 0.7
d <- 0.15
sigma_eta <- 1
nu <- 0.5
# x <- arfima.sim(n = n, model = list(ar = phi, ma = -theta, dfrac = 0))
x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series
y <- x + rnorm(n, 0, nu)

## Test spectral density
# test_x <- spectral.density(ar = phi, ma = theta, d = d, sd = sigma_eta, lambda = freq)
# test_eps <- 1/(2*pi) * (nu/(nu - 2)) # nu = 10
# head(test_x + test_eps)

## Result directory
result_dir <- "./results/"
hmc_filepath <- paste0(result_dir, "hmc_results_n", n, 
                       "_", date, ".rds")

  
## HMC-Whittle parameters 
n_chains <- 2
n_post_samples <- 10000
burn_in <- 5000

## Prior parameters
prior_mean <- c(0, 0, 0, 0, -1)
diag_prior_var <- c(1, 1, 1, 1, 1) # identity matrix

hmc_stan_file <- "./source/arfima_hmc.stan"

hmc_arfima_model <- cmdstan_model(
    hmc_stan_file,
    cpp_options = list(stan_threads = TRUE)
)

hmc_arfima_data <- list(N = length(x), y = y, K = 10, #mu = 0,
                        prior_mean = prior_mean, diag_prior_var = diag_prior_var)

fit_hmc_arfima <- hmc_arfima_model$sample(
    hmc_arfima_data,
    chains = n_chains,
    threads = parallel::detectCores(),
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


png("./plots/hmc_posterior.png", width = 1000, height = 600)
grid.arrange(grobs = plots, ncol = 3)
dev.off()

plot_range <- 1:10000
png("./plots/hmc_trace.png", width = 1000, height = 600)
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
