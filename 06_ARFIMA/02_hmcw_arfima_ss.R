## HMC-Whittle for ARFIMA-only

rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(mvtnorm)
library(cmdstanr)
library(ggplot2)
library(gridExtra)
library(LSTS)
# library(arfima)
library(fracdiff)
library(astsa)

source("./source/compute_periodogram.R")
source("./source/compute_arfima_spec_dens.R")
source("./source/fit_mle_arfima_ss.R")
source("./source/find_optimal_nu.R")

## Flags
date <- "20250627_025" # the _2 version has 15k draws per chain, the og has 11k
noise_dist <- "t" # "t" or "gaussian"
save_hmcw_results <- T
fix_sigma <- F # If TRUE, sigma_eta is fixed to 1 in the model

## Directories
data_dir <- "./data/"
result_dir <- "./results/"

## Read data
n <- 50000
arfima_data <- readRDS(paste0(data_dir, "arfima_data_n", n, "_", noise_dist, ".rds"))
y <- arfima_data$y
phi <- arfima_data$phi
theta <- arfima_data$theta
d <- arfima_data$d
sigma_eta <- arfima_data$sigma_eta
nu <- arfima_data$nu

## Simulate ARFIMA(1, d, 1) process
# set.seed(2025)
# n <- 5000
# phi <- 0.3
# theta <- 0.7
# d <- 0.15
# sigma_eta <- 1
# nu <- 0.2 #20
# # x <- arfima.sim(n, model = list(phi = phi, dfrac = d, theta = theta, sigma2 = sigma_eta^2))
# x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series
# # y <- x + rt(n, df = nu) # ARFIMA + noise
# y <- x + rnorm(n, mean = 0, sd = nu) # ARFIMA + noise

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

#############################
##    Setting up priors    ##  
#############################

## Compute periodogram
pgram_output <- compute_periodogram(y)
freq <- pgram_output$freq
I <- pgram_output$periodogram

mle <- fit_mle_arfima_ss1(pdg = I, freq = freq, noise_dist = noise_dist)$par

# mle2 <- find_optimal_nu(pdg = I, freq = freq, noise_dist = noise_dist)

## Prior parameters
mle_phi <- mle[1]
mle_theta <- mle[2]
mle_d <- mle[3]
mle_sigma_eta <- mle[4]
mle_nu <- mle[5]

param_names <- c("phi", "theta", "d", "sigma_eta", "nu")
true_vals <- c(phi, theta, d, sigma_eta, nu)

mle_df <- data.frame(param = param_names,
                    true_vals = true_vals,
                    mle = mle)
print(mle_df)


if (noise_dist == "t") {
  prior_mean_nu <- log(mle_nu - 2)
  prior_var_nu <- 1 # variance for nu
} else {
  prior_mean_nu <- log(mle_nu^2)
  prior_var_nu <- 0.5 # variance for nu
}

# if (transform == "arctanh") {
  prior_mean <- c(atanh(mle_phi), atanh(mle_theta), atanh(2*mle_d), log(mle_sigma_eta^2), prior_mean_nu)
  diag_prior_var <- c(0.25, 0.25, 0.25, 1, prior_var_nu) 
# } else {
#   prior_mean <- c(logit(mle_phi), logit(mle_theta), atanh(2*mle_d), log(mle_sigma_eta^2), prior_mean_nu)
#   diag_prior_var <- c(0.5, 0.5, 0.5, 0.5, prior_var_nu) 
# }

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

png("./plots/prior_hmcw.png", width = 800, height = 600)
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

#############################
##       HMC-Whittle       ##
#############################

hmcw_filepath <- paste0(result_dir, "hmcw_arfima_ss_results_n", n, 
                       "_", noise_dist, "_", date, ".rds")

  
## HMC-Whittle parameters 
n_chains <- 2
n_post_samples <- 10000
burn_in <- 5000

## HMC-Whittle
whittle_stan_file <- "./source/arfima_ss_whittle.stan"

whittle_arfima_model <- cmdstan_model(
    whittle_stan_file,
    cpp_options = list(stan_threads = TRUE)
)

whittle_arfima_data <- list(nfreq = length(freq), freqs = freq, periodogram = I,
                            fix_sigma = ifelse(fix_sigma, 1, 0),
                            use_t_noise = ifelse(noise_dist == "t", 1, 0), # 1 for t noise, 0 for gaussian noise
                            prior_mean = prior_mean, 
                            diag_prior_var = diag_prior_var
                            )

init_list <- lapply(1:n_chains, function(i) {
  ini_vals <- rmvnorm(1, prior_mean, diag(diag_prior_var))
  list(
    tilde_phi = ini_vals[1],
    tilde_theta = ini_vals[2],
    tilde_d = ini_vals[3],
    tilde_sigma_eta = ini_vals[4],
    tilde_nu = ini_vals[5]
  )
})

fit_stan_arfima_whittle <- whittle_arfima_model$sample(
    whittle_arfima_data,
    chains = n_chains,
    parallel_chains = n_chains,
    threads = parallel::detectCores(),
    refresh = 250,
    iter_warmup = burn_in,
    iter_sampling = n_post_samples,
    save_warmup = TRUE,
    init = init_list
)

hmcw_results <- list(draws = fit_stan_arfima_whittle$draws(variables = c("phi", "theta", "d", "sigma_eta", "nu"), inc_warmup = TRUE),
                    time = fit_stan_arfima_whittle$time(),
                    summary = fit_stan_arfima_whittle$summary(variables = c("phi", "theta", "d", "sigma_eta", "nu")),
                    metadata = fit_stan_arfima_whittle$metadata())
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
        # labs(x = vars) +
        theme_bw() +
        # theme(axis.title = element_blank(), text = element_text(size = 24)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 4))

    plots[[p]] <- plot
}


png(paste0("./plots/hmcw_arfima_ss_posterior_n", n, ".png"), width = 1000, height = 600)
grid.arrange(grobs = plots, ncol = 3)
dev.off()

png(paste0("./plots/hmcw_arfima_ss_trace_n", n, ".png"), width = 1000, height = 600)
par(mfrow = c(2, 3))
plot_range <- 1:length(hmcw.phi)
plot(hmcw.phi[plot_range], type = "l")
abline(h = phi, col = "red", lwd = 2, lty = 2)
plot(hmcw.theta[plot_range], type = "l")
abline(h = theta, col = "red", lwd = 2, lty = 2)
plot(hmcw.d[plot_range], type = "l")
abline(h = d, col = "red", lwd = 2, lty = 2)
plot(hmcw.sigma_eta[plot_range], type = "l")
abline(h = sigma_eta, col = "red", lwd = 2, lty = 2)
plot(hmcw.nu[plot_range], type = "l")
abline(h = nu, col = "red", lwd = 2, lty = 2)
dev.off()
