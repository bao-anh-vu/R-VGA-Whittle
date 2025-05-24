## SSM with underlying ARFIMA model
rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(mvtnorm)
library(arfima)
library(fracdiff)
library(tensorflow)
reticulate::use_condaenv("myenv", required = TRUE)
library(keras)
library(ggplot2)
library(gridExtra)

source("./source/run_rvgaw_arfima.R")
source("./source/compute_periodogram.R")
source("./source/compute_grad.R")
source("./source/compute_arfima_spec_dens.R")

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
save_rvgaw_results <- T

## Read data
data_dir <- "./data/"
# n <- 10000
# arfima_data <- readRDS(paste0(data_dir, "arfima_data_n", n, ".rds"))
# y <- arfima_data$y
# phi <- arfima_data$phi
# theta <- arfima_data$theta
# d <- arfima_data$d
# sigma_eta <- arfima_data$sigma_eta
# nu <- arfima_data$nu

set.seed(2025)
n <- 2000
phi <- 0.3
theta <- 0.7
d <- 0.25
sigma_eta <- 1
# nu <- 5 #20
# x <- arfima.sim(n, model = list(phi = phi, dfrac = d, theta = theta, sigma2 = sigma_eta^2))
x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series
# y <- x + rt(n, df = nu) # ARFIMA + noise
# y <- x + rnorm(n, mean = 0, sd = nu) # ARFIMA + noise

# png("sim.png", width = 800, height = 600)
# plot(y[1:200], type = "l", main = paste0("ARFIMA(1, ", d, ", 1)"))
# dev.off()

## Result directory
result_dir <- "./results/"

## Flags
date <- "20250514"

## Common settings for all methods
n_post_samples <- 10000
prior_mean <- c(0, 0, 0, 0)
prior_var <- diag(c(1, 1, 1, 1)) # identity matrix

## Simulate from prior
prior_samples <- rmvnorm(10000, prior_mean, prior_var)
phi_samples <- tanh(prior_samples[, 1])
theta_samples <- tanh(prior_samples[, 2])
d_samples <- 0.5 * tanh(prior_samples[, 3]) 
sigma_eta_samples <- sqrt(exp(prior_samples[, 4]))

png("./plots/prior_arfima.png", width = 800, height = 600)
par(mfrow = c(2, 3))
plot(density(phi_samples), main = "phi", xlim = c(-1, 1))
abline(v = phi, col = "red", lty = 2)
plot(density(theta_samples), main = "theta", xlim = c(-1, 1))
abline(v = theta, col = "red", lty = 2)
plot(density(d_samples), main = "d", xlim = c(0, 1))
abline(v = d, col = "red", lty = 2)
plot(density(sigma_eta_samples), main = "sigma_eta", xlim = c(0, 5))
abline(v = sigma_eta, col = "red", lty = 2) 
dev.off()

##########################################
##            R-VGA-Whittle             ##
##########################################
S <- 200L
use_tempering <- TRUE
temper_first <- T
reorder <- 0 #"decreasing"
blocksize <- 10L #20L
n_indiv <- 0L

if (use_tempering) {
  n_temper <- 10
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

rvgaw_filepath <- paste0(result_dir, "rvga_whittle_results_n", n,
                        temper_info, reorder_info, block_info, "_", date, ".rds")

rvgaw_results <- run_rvgaw_arfima(data = x, 
                                  prior_mean = prior_mean, prior_var = prior_var, 
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

rvgaw_df <- data.frame(
    phi = rvgaw.phi,
    theta = rvgaw.theta,
    d = rvgaw.d,
    sigma_eta = rvgaw.sigma_eta
)

plots <- list()

param_names <- c("phi", "theta", "d", "sigma_eta")
param_values <- c(phi, theta, d, sigma_eta)
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

png("./plots/rvgaw_arfima_posterior.png", width = 800, height = 600)
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
# dev.off()

