## HMC-Whittle for ARFIMA-state space model

rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(cmdstanr)
library(ggplot2)
library(gridExtra)
library(LSTS)
source("./source/compute_periodogram.R")

## Flags
date <- "20250514"
save_hmcw_results <- T

## Read data
data_dir <- "./data/"
arfima_data <- readRDS(paste0(data_dir, "arfima_data.rds"))
y <- arfima_data$y
phi <- arfima_data$phi
theta <- arfima_data$theta
d <- arfima_data$d
sigma_eta <- arfima_data$sigma_eta
nu <- arfima_data$nu
n <- length(y)

## Test spectral density
# test_x <- spectral.density(ar = phi, ma = theta, d = d, sd = sigma_eta, lambda = freq)
# test_eps <- 1/(2*pi) * (nu/(nu - 2)) # nu = 10
# head(test_x + test_eps)

## Result directory
result_dir <- "./results/"
hmcw_filepath <- paste0(result_dir, "hmcw_results_n", n, 
                       "_", date, ".rds")

  
## HMC-Whittle parameters 

n_chains <- 1
n_post_samples <- 10000
burn_in <- 1000

## Prior parameters
prior_mean <- c(0, 0, 0, 0, 2)
diag_prior_var <- c(1, 1, 1, 1, 0.5) # identity matrix

# Compute periodogram
pgram_output <- compute_periodogram(y)
freq <- pgram_output$freq
I <- pgram_output$periodogram

whittle_stan_file <- "./source/stan_arfima_whittle.stan"

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
    refresh = 1000,
    iter_warmup = burn_in / n_chains,
    iter_sampling = n_post_samples / n_chains
)

hmcw_results <- list(draws = fit_stan_arfima_whittle$draws(variables = c("phi", "theta", "sigma_eta", "d", "nu")),
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
hmcw.nu <- c(hmcw_results$draws[,,5])

hmcw_df <- data.frame(
    phi = hmcw.phi,
    theta = hmcw.theta,
    d = hmcw.d,
    sigma_eta = hmcw.sigma_eta,
    nu = hmcw.nu
)

## Plot posterior distribution for each parameter

plots <- list()

param_names <- c("phi", "theta", "d", "sigma_eta", "nu")
param_values <- c(phi, theta, d, sigma_eta, nu)
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


png("./plots/hmcw_posterior.png", width = 1000, height = 600)
grid.arrange(grobs = plots, ncol = 3)
dev.off()

par(mfrow = c(3, 2))
plot(hmcw.phi, type = "l")
plot(hmcw.theta, type = "l")
plot(hmcw.d, type = "l")
plot(hmcw.sigma_eta, type = "l")
plot(hmcw.nu, type = "l")
dev.off()
