## HMC-Whittle for ARFIMA-only

rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(cmdstanr)
library(ggplot2)
library(gridExtra)
library(LSTS)
# library(arfima)
library(fracdiff)
library(astsa)

source("./source/compute_periodogram.R")

## Flags
date <- "20250514"
save_hmcw_results <- T

## Simulate ARFIMA(1, d, 1) process
set.seed(2025)

# Simulate ARMA(1,1) with ar = 0.9, ma = 0.5
n <- 1000
phi <- 0.2
theta <- 0.5
d <- 0.25
sigma_eta <- 1
# x <- arfima.sim(n = n, model = list(ar = phi, ma = -theta, dfrac = 0))
x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series

# Fit the ARMA(1,1) model using arima()
# fit <- arima(x, order = c(1, 0, 1), include.mean = FALSE)

# Print true and estimated coefficients
# cat("True AR coefficient:", phi, "\n")
# cat("True MA coefficient:", theta, "\n\n")
# cat("Estimated coefficients:\n")
# print(fit$coef)

# x <- arima.sim(list(order = c(1, d, 1), ar = phi, ma = theta), n = n)
## Test spectral density
# test_x <- spectral.density(ar = phi, ma = theta, d = d, sd = sigma_eta, lambda = freq)
# test_eps <- 1/(2*pi) * (nu/(nu - 2)) # nu = 10
# head(test_x + test_eps)


## Test spectral density
# arma_spec_dens <- arma.spec(ar = phi, ma = theta, n.freq = 500)
# print(head(arma_spec_dens$spec))

# fit_whittle <- LS.whittle(
#   series = x, start = c(0.1, 0.1, 0.1), ar.order = 1, ma.order = 1, include.d = F
# )

## Result directory
result_dir <- "./results/"
hmcw_filepath <- paste0(result_dir, "hmcw_arfima_results_n", n, 
                       "_", date, ".rds")

  
## HMC-Whittle parameters 
n_chains <- 1
n_post_samples <- 5000
burn_in <- 1000#0

## Prior parameters
prior_mean <- c(0, 0, 0, 0)
diag_prior_var <- c(1, 1, 1, 1) # identity matrix

# Compute periodogram
pgram_output <- compute_periodogram(x)
freq <- pgram_output$freq
I <- pgram_output$periodogram

whittle_stan_file <- "./source/arfima_whittle.stan"

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
    iter_warmup = burn_in / n_chains,
    iter_sampling = n_post_samples / n_chains
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
        labs(x = vars) +
        theme_bw() +
        theme(axis.title = element_blank(), text = element_text(size = 24)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 4))

    plots[[p]] <- plot
}


png("./plots/hmcw_arfima_posterior.png", width = 1000, height = 600)
grid.arrange(grobs = plots, ncol = 3)
dev.off()

png("./plots/hmcw_arfima_trace.png", width = 1000, height = 600)
par(mfrow = c(2, 1))
plot(hmcw.phi, type = "l")
abline(h = phi, col = "red", lwd = 2, lty = 2)
plot(hmcw.theta, type = "l")
abline(h = theta, col = "red", lwd = 2, lty = 2)
plot(hmcw.d, type = "l")
abline(h = d, col = "red", lwd = 2)
plot(hmcw.sigma_eta, type = "l")
abline(h = sigma_eta, col = "red", lwd = 2)
dev.off()
