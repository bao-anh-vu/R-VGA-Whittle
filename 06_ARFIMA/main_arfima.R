## SSM with underlying ARFIMA model
rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(mvtnorm)
library(arfima)
library(tensorflow)
reticulate::use_condaenv("myenv", required = TRUE)
library(keras)

source("./source/run_rvgaw_arfima.R")
source("./source/compute_periodogram.R")
source("./source/compute_grad.R")

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


## Simulate ARFIMA(1, 0.25, 1) process
set.seed(2025)
n <- 2000
phi <- 0.5
theta <- 0.9
d <- 0.25
sigma_eta <- 1
nu <- 5
x <- arfima.sim(n, model = list(phi = phi, dfrac = d, theta = theta))
y <- x + rt(n, df = nu) # ARFIMA + noise

png("sim.png", width = 800, height = 600)
# par(mfrow = c(2, 1))
plot(y[1:200], type = "l", main = paste0("ARFIMA(1, ", d, ", 1)"))
# plot(sim2[1:200], type = "l", main = "ARFIMA(2,0,1), dfrac = 0")
dev.off()

## MLE fit using arfima package
# fit <- arfima(y, order = c(1, 0, 1), back=TRUE)


## Result directory
result_directory <- "./results/"

## Flags
date <- "20230525"

## Common settings for all methods
n_post_samples <- 10000
prior_mean <- rep(0, 5)
prior_var <- diag(c(1, 1, 1, 1, 1)) # identity matrix

## Simulate from prior
prior_samples <- rmvnorm(10000, prior_mean, prior_var)
phi_samples <- tanh(prior_samples[, 1])
theta_samples <- tanh(prior_samples[, 2])
d_samples <- 0.5 * tanh(prior_samples[, 3]) 
sigma_eta_samples <- sqrt(exp(prior_samples[, 4]))
nu_samples <- 2 + exp(prior_samples[, 5])

png("./plots/prior.png", width = 800, height = 600)
par(mfrow = c(2, 3))
plot(density(phi_samples), main = "phi", xlim = c(-1, 1))
abline(v = phi, col = "red", lty = 2)
plot(density(theta_samples), main = "theta", xlim = c(-1, 1))
abline(v = theta, col = "red", lty = 2)
plot(density(d_samples), main = "d", xlim = c(0, 1))
abline(v = d, col = "red", lty = 2)
plot(density(sigma_eta_samples), main = "sigma_eta", xlim = c(0, 5))
abline(v = sigma_eta, col = "red", lty = 2) 
plot(density(nu_samples), main = "nu", xlim = c(0, 20))
abline(v = nu, col = "red", lty = 2)
dev.off()

##########################################
##            R-VGA-Whittle             ##
##########################################
S <- 200L
use_tempering <- TRUE
temper_first <- T
reorder <- 0 #"decreasing"
blocksize <- 1L
n_indiv <- 1L

if (use_tempering) {
  n_temper <- 5
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

rvgaw_filepath <- paste0(result_directory, "rvga_whittle_results_n", n,
                        temper_info, reorder_info, block_info, "_", date, ".rds")

rvgaw_results <- run_rvgaw_arfima(y = y, #sigma_eta = sigma_eta, sigma_eps = sigma_eps, 
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


rvgaw.phi <- rvgaw_results$post_samples$phi
rvgaw.theta <- rvgaw_results$post_samples$theta
rvgaw.d <- rvgaw_results$post_samples$d
rvgaw.sigma_eta <- rvgaw_results$post_samples$sigma_eta
rvgaw.nu <- rvgaw_results$post_samples$nu

png("./plots/rvgaw_posterior.png", width = 800, height = 600)
par(mfrow = c(2, 3))
plot(density(rvgaw.phi), main = "phi", xlim = c(-1, 1))
abline(v = phi, col = "red", lty = 2)
plot(density(rvgaw.theta), main = "theta", xlim = c(-1, 1))
abline(v = theta, col = "red", lty = 2)
plot(density(rvgaw.d), main = "d", xlim = c(0, 1))
abline(v = d, col = "red", lty = 2)
plot(density(rvgaw.sigma_eta), main = "sigma_eta", xlim = c(0, 5))
abline(v = sigma_eta, col = "red", lty = 2)
plot(density(rvgaw.nu), main = "nu", xlim = c(0, 20))
abline(v = nu, col = "red", lty = 2)
dev.off()


mu <- rvgaw_results$mu
