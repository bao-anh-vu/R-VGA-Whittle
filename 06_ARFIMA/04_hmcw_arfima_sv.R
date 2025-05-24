## HMC-Whittle for ARFIMA-state space model

rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(mvtnorm)
library(cmdstanr)
library(ggplot2)
library(gridExtra)
library(fracdiff)
# library(LSTS)

source("./source/compute_periodogram.R")

## Flags
date <- "20250514"
save_hmcw_results <- T

## Simulate ARFIMA(1, d, 1) process
set.seed(2025)

n <- 50000
phi <- -0.5
theta <- 0.5
d <- 0.25
sigma_eta <- 1
kappa <- 1
# x <- arfima.sim(n = n, model = list(ar = phi, ma = -theta, dfrac = 0))
x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series
eps <- rnorm(n, 0, 1)
y <- kappa * exp(x / 2) * eps

## Test spectral density
# test_x <- spectral.density(ar = phi, ma = theta, d = d, sd = sigma_eta, lambda = freq)
# test_eps <- 1/(2*pi) * (nu/(nu - 2)) # nu = 10
# head(test_x + test_eps)

## Result directory
result_dir <- "./results/"
hmcw_filepath <- paste0(result_dir, "hmcw_sv_results_n", n, 
                       "_", date, ".rds")

  
## HMC-Whittle parameters 
n_chains <- 1
n_post_samples <- 10000
burn_in <- 5000

## Prior parameters
prior_mean <- c(0, 0, 0, 0)
diag_prior_var <- c(1, 1, 1, 1) # identity matrix

## Simulate from prior
prior_samples <- rmvnorm(10000, prior_mean, diag(diag_prior_var))
phi_samples <- tanh(prior_samples[, 1])
theta_samples <- tanh(prior_samples[, 2])
d_samples <- 0.5 * tanh(prior_samples[, 3]) 
sigma_eta_samples <- sqrt(exp(prior_samples[, 4]))
# nu_samples <- 2 + exp(prior_samples[, 5])


png("./plots/prior_sv.png", width = 600, height = 600)
par(mfrow = c(2, 2))
plot(density(phi_samples), main = "phi", xlim = c(-1, 1))
abline(v = phi, col = "red", lty = 2)
plot(density(theta_samples), main = "theta", xlim = c(-1, 1))
abline(v = theta, col = "red", lty = 2)
plot(density(d_samples), main = "d", xlim = c(-0.5, 0.5))
abline(v = d, col = "red", lty = 2)
plot(density(sigma_eta_samples), main = "sigma_eta", xlim = c(0, 5))
abline(v = sigma_eta, col = "red", lty = 2) 
dev.off()

# Compute periodogram
y_tilde <- log(y^2) - mean(log(y^2))
pgram_output <- compute_periodogram(y_tilde)
freq <- pgram_output$freq
I <- pgram_output$periodogram

whittle_stan_file <- "./source/sv_arfima.stan"

whittle_arfima_model <- cmdstan_model(
    whittle_stan_file,
    cpp_options = list(stan_threads = TRUE)
)

whittle_arfima_data <- list(nfreq = length(freq), freqs = freq, periodogram = I,
                            prior_mean = prior_mean, diag_prior_var = diag_prior_var)

fit_stan_arfima_whittle <- whittle_arfima_model$sample(
    whittle_arfima_data,
    chains = n_chains,
    threads = parallel::detectCores(),
    refresh = 500,
    iter_warmup = burn_in,
    iter_sampling = n_post_samples
)

hmcw_results <- list(draws = fit_stan_arfima_whittle$draws(variables = c("phi", "theta", "d", "sigma_eta")),
                    time = fit_stan_arfima_whittle$time,
                    summary = fit_stan_arfima_whittle$cmdstan_summary)
# fit_stan_arfima_whittle$cmdstan_summary()
# fit_stan_arfima_whittle$diagnostic_summary()

if (save_hmcw_results) {
    saveRDS(hmcw_results, hmcw_filepath)
}


hmcw.phi <- c(hmcw_results$draws[,,1])
hmcw.theta <- c(hmcw_results$draws[,,2])
hmcw.d <- c(hmcw_results$draws[,,3])
hmcw.sigma_eta <- c(hmcw_results$draws[,,4])

hmcw_df <- data.frame(
    phi = hmcw.phi,
    theta = hmcw.theta,
    d = hmcw.d,
    sigma_eta = hmcw.sigma_eta
)

## Plot posterior distribution for each parameter

plots <- list()

param_names <- c("phi", "theta", "d", "sigma_eta")
param_values <- c(phi, theta, d, sigma_eta)
for (p in 1:length(param_names)) {
    true_vals_df <- data.frame(name = param_names[p], val = param_values[p])

    plot <- ggplot(hmcw_df, aes(x = .data[[param_names[p]]])) +
        geom_density(col = "red", lwd = 1) +
        geom_density(data = hmcw_df, col = "goldenrod", lwd = 1) +
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


png("./plots/hmcw_sv_posterior.png", width = 800, height = 800)
grid.arrange(grobs = plots, nrow = 2)
dev.off()

plot_range <- 1:10000
png("./plots/hmcw_sv_trace.png", width = 1000, height = 600)
par(mfrow = c(3, 2))
plot(hmcw.phi[plot_range], type = "l", main = "phi", ylab = "phi")
abline(h = phi, col = "red", lwd = 2, lty = 2)
plot(hmcw.theta[plot_range], type = "l", main = "theta", ylab = "theta")
abline(h = theta, col = "red", lwd = 2, lty = 2)
plot(hmcw.d[plot_range], type = "l", main = "d", ylab = "d")
abline(h = d, col = "red", lwd = 2, lty = 2)
plot(hmcw.sigma_eta[plot_range], type = "l", main = "sigma_eta", ylab = "sigma_eta")
abline(h = sigma_eta, col = "red", lwd = 2, lty = 2)
dev.off()
