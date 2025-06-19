## SSM with underlying ARFIMA model
setwd("~/R-VGA-Whittle/06_ARFIMA")

rm(list = ls())

# Load libraries
library(mvtnorm)
# library(arfima)
library(fracdiff)
library(tensorflow)
reticulate::use_condaenv("myenv", required = TRUE)
library(keras)
library(ggplot2)
library(gridExtra)
# library(parallel)

source("./source/fit_mle_arfima_ss.R")
source("./source/find_optimal_nu.R")
source("./source/run_rvgaw_arfima_ss.R")
source("./source/find_cutoff_freq.R")
source("./source/compute_periodogram.R")
source("./source/compute_arfima_spec_dens.R")
source("./source/compute_grad_ss.R")

################## Some code to limit tensorflow memory usage ##################

# List physical devices
gpus <- tf$config$experimental$list_physical_devices('GPU')

if (length(gpus) > 0) {
  tryCatch({
    # Restrict TensorFlow to only allocate 4GB of memory on the first GPU
    tf$config$experimental$set_virtual_device_configuration(
      gpus[[1]],
      list(tf$config$experimental$VirtualDeviceConfiguration(memory_limit=4096))
    )
    
    logical_gpus <- tf$config$experimental$list_logical_devices('GPU')
    
    print(paste0(length(gpus), " Physical GPUs,", length(logical_gpus), " Logical GPUs"))
  }, error = function(e) {
    # Virtual devices must be set before GPUs have been initialized
    print(e)
  })
}

################## End of code to limit tensorflow memory usage ##################


## Flags
date <- "20250514"
noise_dist <- "t" # "t" or "gaussian"
save_rvgaw_results <- T
fix_nu <- F # whether to fix nu in the MLE estimation

## Directories
data_dir <- "./data/"
result_dir <- "./results/"

## Read data
# n <- 3000
# arfima_data <- readRDS(paste0(data_dir, "arfima_data_n", n, "_", noise_dist, ".rds"))
# y <- arfima_data$y
# phi <- arfima_data$phi
# theta <- arfima_data$theta
# d <- arfima_data$d
# sigma_eta <- arfima_data$sigma_eta
# nu <- arfima_data$nu

## Simulate ARFIMA(1, 0.25, 1) process
# set.seed(2025)         
n <- 50000
phi <- 0.3
theta <- 0.5 #-0.5
d <- 0.25
sigma_eta <- 1

if (noise_dist == "t") {
  nu <- 4 # degrees of freedom for t-distribution
} else {
  nu <- 0.2 # standard deviation for Gaussian noise
}

x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series

if (noise_dist == "t") {
  y <- x + rt(n, df = nu) # ARFIMA + noise
} else {
  y <- x + rnorm(n, mean = 0, sd = nu) # ARFIMA + noise
}


## Compute periodogram
pgram_output <- compute_periodogram(y)
freq <- pgram_output$freq
I <- pgram_output$periodogram

# mle <- fit_mle_arfima_ss1(pdg = I, freq = freq, noise_dist = noise_dist)$par

mle <- find_optimal_nu(pdg = I, freq = freq, noise_dist = noise_dist)


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

browser()

if (noise_dist == "t") {
prior_mean_nu <- log(mle_nu - 2)
prior_var_nu <- 0.5 # variance for nu
} else {
  prior_mean_nu <- log(mle_nu^2)
  prior_var_nu <- 0.5 # variance for nu
}

logit_fun <- function(x) {
  return(log(x / (1 - x)))
}

prior_mean <- c(atanh(mle_phi), logit_fun(mle_theta), atanh(2*mle_d), log(mle_sigma_eta^2), prior_mean_nu)
# prior_mean <- c(atanh(mle_phi), atanh(mle_theta), atanh(2*mle_d), log(mle_sigma_eta^2), prior_mean_nu)
diag_prior_var <- c(rep(0.5, 4), prior_var_nu) 

inv_logit <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

## Simulate from prior
prior_samples <- rmvnorm(10000, prior_mean, diag(diag_prior_var))
phi_samples <- tanh(prior_samples[, 1])
# theta_samples <- tanh(prior_samples[, 2])
theta_samples <- inv_logit(prior_samples[, 2])

d_samples <- 0.5 * tanh(prior_samples[, 3]) 
sigma_eta_samples <- sqrt(exp(prior_samples[, 4]))
# nu_samples <- 2 + exp(prior_samples[, 5])

if (!fix_nu) {
  if (noise_dist == "t") {
    nu_samples <- 2 + exp(prior_samples[, 5])
  } else {# gaussian
    nu_samples <- sqrt(exp(prior_samples[, 5]))
  }
}

png(paste0("./plots/prior_", noise_dist, ".png"), width = 800, height = 600)
par(mfrow = c(2, 3))
plot(density(phi_samples), main = "phi", xlim = c(-1, 1))
abline(v = phi, col = "red", lty = 2)
plot(density(theta_samples), main = "theta", xlim = c(0, 1))
abline(v = theta, col = "red", lty = 2)
plot(density(d_samples), main = "d", xlim = c(0, 1))
abline(v = d, col = "red", lty = 2)
plot(density(sigma_eta_samples), main = "sigma_eta", xlim = c(0, 5))
abline(v = sigma_eta, col = "red", lty = 2) 
if (!fix_nu) {
  plot(density(nu_samples), main = "nu", xlim = c(0, 20))
  abline(v = nu, col = "red", lty = 2)
}
dev.off()
browser()
##########################################
##            R-VGA-Whittle             ##
##########################################
S <- 1000L
use_tempering <- TRUE
temper_first <- T
reorder <- 0 #"decreasing"
blocksize <- 50L
# n_indiv <- 20L
n_indiv <- find_cutoff_freq(y, nsegs = 25, power_prop = 1/2)$cutoff_ind #100
n_post_samples <- 10000

if (use_tempering) {
  n_temper <- 100
  K <- 100
  temper_schedule <- rep(1/K, K)
  temper_info <- ""
  if (temper_first) {
    temper_info <- paste0("_temperfirst", n_temper)
  } else {
    temper_info <- paste0("_temperlast", n_temper)
  }
} else {
  temper_info <- ""
}

if (reorder == "random") {
  reorder_info <- paste0("_", reorder, reorder_seed)
} else if (reorder == "decreasing") {
  reorder_info <- paste0("_", reorder)
} else if (reorder > 0) {
  reorder_info <- paste0("_reorder", reorder)
} else {
  reorder_info <- ""
}

# if (!is.null(nblocks)) {
if (!is.null(blocksize)) {
  block_info <- paste0("_", "blocksize", blocksize, "_", n_indiv, "indiv")
} else {
  block_info <- ""
}

rvgaw_filepath <- paste0(result_dir, "rvga_whittle_results_n", n, "_", noise_dist,
                        temper_info, reorder_info, block_info, "_", date, ".rds")

rvgaw_results <- run_rvgaw_arfima(data = y, 
                                  noise_dist = noise_dist,
                                  prior_mean = prior_mean, 
                                  prior_var = diag(diag_prior_var), 
                                  deriv = "tf", 
                                  S = S, n_post_samples = n_post_samples,
                                  use_tempering = use_tempering, 
                                  temper_first = temper_first,
                                  temper_schedule = temper_schedule,
                                  reorder = reorder,
                                  reorder_seed = reorder_seed,
                                  n_temper = n_temper,
                                  # nblocks = nblocks,
                                  blocksize = blocksize,
                                  n_indiv = n_indiv
                                  )
saveRDS(rvgaw_results, rvgaw_filepath)

rvgaw.phi <- rvgaw_results$post_samples$phi
rvgaw.theta <- rvgaw_results$post_samples$theta
rvgaw.d <- rvgaw_results$post_samples$d
rvgaw.sigma_eta <- rvgaw_results$post_samples$sigma_eta
rvgaw.nu <- rvgaw_results$post_samples$nu

rvgaw_df <- data.frame(
    phi = rvgaw.phi,
    theta = rvgaw.theta,
    d = rvgaw.d,
    sigma_eta = rvgaw.sigma_eta,
    nu = rvgaw.nu
)

plots <- list()

param_names <- c("phi", "theta", "d", "sigma_eta", "nu")
param_values <- c(phi, theta, d, sigma_eta, nu)
for (p in 1:length(param_names)) {
    true_vals_df <- data.frame(name = param_names[p], val = param_values[p])

    plot <- ggplot(rvgaw_df, aes(x = .data[[param_names[p]]])) +
        geom_density(col = "red", lwd = 1) +
        geom_density(data = rvgaw_df, col = "goldenrod", lwd = 1) +
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

png(paste0("./plots/rvgaw_posterior_arfima_ss_", noise_dist, ".png"), width = 800, height = 600)
grid.arrange(grobs = plots, ncol = 3)
dev.off()


# png("./plots/rvgaw_posterior.png", width = 800, height = 600)
# par(mfrow = c(2, 3))
# plot(density(rvgaw.phi), main = "phi", xlim = c(-1, 1))
# abline(v = phi, col = "red", lty = 2)
# plot(density(rvgaw.theta), main = "theta", xlim = c(-1, 1))
# abline(v = theta, col = "red", lty = 2)
# plot(density(rvgaw.d), main = "d", xlim = c(0, 1))
# abline(v = d, col = "red", lty = 2)
# plot(density(rvgaw.sigma_eta), main = "sigma_eta", xlim = c(0, 5))
# abline(v = sigma_eta, col = "red", lty = 2)
# plot(density(rvgaw.nu), main = "nu", xlim = c(0, 20))
# abline(v = nu, col = "red", lty = 2)
# dev.off()

## Plot trajectory of the variational means
precs <- rvgaw_results$prec
vars <- lapply(precs, solve)
test <- lapply(1:length(rvgaw_results$mu), function(i) {
  rmvnorm(1000, rvgaw_results$mu[[i]], vars[[i]])
})

transform_to_og_space <- function(scaled_params, noise_dist) {
  phi <- tanh(scaled_params[, 1])
  theta <- tanh(scaled_params[, 2])
  d <- 0.5 * tanh(scaled_params[, 3])
  sigma_eta <- sqrt(exp(scaled_params[, 4]))

  if (noise_dist == "t") {
    nu <- 2 + exp(scaled_params[, 5])
  } else {
    nu <- sqrt(exp(scaled_params[, 5]))
  }
  return(cbind(phi, theta, d, sigma_eta, nu))
}

og_params <- lapply(test, transform_to_og_space, noise_dist = noise_dist) 
og_param_means <- lapply(og_params, colMeans)
phi_means <- sapply(og_param_means, function(x) x[1])
theta_means <- sapply(og_param_means, function(x) x[2]) 
d_means <- sapply(og_param_means, function(x) x[3])
sigma_eta_means <- sapply(og_param_means, function(x) x[4])
nu_means <- sapply(og_param_means, function(x) x[5])

png("./plots/rvgaw_arfima_means.png", width = 800, height = 600)
par(mfrow = c(2, 3))
plot(phi_means, type = "l", main = "phi", xlab = "Iteration", ylab = "Value")
abline(h = phi, col = "red", lty = 2)
plot(theta_means, type = "l", main = "theta", xlab = "Iteration", ylab = "Value")
abline(h = theta, col = "red", lty = 2)
plot(d_means, type = "l", main = "d", xlab = "Iteration", ylab = "Value")
abline(h = d, col = "red", lty = 2)
plot(sigma_eta_means, type = "l", main = "sigma_eta", xlab = "Iteration", ylab = "Value")
abline(h = sigma_eta, col = "red", lty = 2)
plot(nu_means, type = "l", main = "nu", xlab = "Iteration", ylab = "Value")
abline(h = nu, col = "red", lty = 2)
dev.off()
