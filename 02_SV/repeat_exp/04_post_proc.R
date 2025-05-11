## Post-processing

setwd("~/R-VGA-Whittle/02_SV/repeat_exp/")

rm(list = ls())

library(mvtnorm)

## Read results
# rep <- as.numeric(commandArgs(trailingOnly = TRUE))
# cat("Rep:", rep, "\n")

reps <- 1:40

## Result directory
date <- "20250508" #"20230918" #the 20230918 version has sigma_eta = sqrt(0.1)
phi <- 0.9
n <- 2000
phi_string <- sub("(\\d+)\\.(\\d+)", "\\1\\2", toString(phi)) ## removes decimal point fron the number

data_dir <- paste0("./data/phi", phi_string)
result_dir <- paste0("./results/phi", phi_string, "/")
plot_dir <- paste0("./plots/phi", phi_string, "/")

## R-VGA flags
use_tempering <- T
temper_first <- T
reorder <- 0 #"decreasing" # or decreasing # or a number
reorder_seed <- 2024
prior_type <- ""
transform <- "arctanh"
blocksize <- 100 
n_indiv <- 0

## HMC flags
burn_in <- 5000

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

if (!is.null(blocksize)) {
  block_info <- paste0("_", "blocksize", blocksize, "_", n_indiv, "indiv")
} else {
  block_info <- ""
}

## Read true parameters
sv_data <- readRDS(paste0(data_dir, "/sv_data_n", n, "_phi", phi_string, 
                                  "_", formatC(1, digits = 2, flag = "0"), "_", date, ".rds"))
phi <- sv_data$phi
sigma_eta <- sv_data$sigma_eta
sigma_eps <- sv_data$sigma_eps

rvgaw_results <- list()
hmc_results <- list()

for (rep in reps) {
  rvgaw_filepath <- paste0(result_dir, "rvga_whittle_results_n", n, 
                           temper_info, reorder_info, block_info,
                           "_", formatC(rep, digits = 2, flag = "0"), "_", date, ".rds")
  
  hmc_filepath <- paste0(
    result_dir, "hmc_results_n", n, "_phi", phi_string, 
    "_", formatC(rep, digits = 2, flag = "0"), "_", date, ".rds"
)
  if (file.exists(hmc_filepath)) {
    hmc_results[[rep]] <- readRDS(hmc_filepath)
  } else {
    cat("File not found:", hmc_filepath, "\n")
  }

  if (file.exists(rvgaw_filepath)) {
    rvgaw_results[[rep]] <- readRDS(rvgaw_filepath)
  } else {
    cat("File not found:", rvgaw_filepath, "\n")
  }
}

hmc_phi <- lapply(hmc_results, function(x) c(x$draws[-(1:burn_in), , 1])) # tanh(hmc.theta_phi)
hmc_sigma_eta <- lapply(hmc_results, function(x) c(x$draws[-(1:burn_in), , 2])) # sqrt(exp(hmc.theta_sigma))

thin_interval <- 100
inds <- seq(1, length(hmc_phi[[1]]), by = thin_interval)
hmc_phi_thin <- lapply(hmc_phi, function(x) x[inds])
hmc_sigma_eta_thin <- lapply(hmc_sigma_eta, function(x) x[inds])

hmc_phi_means <- sapply(hmc_phi_thin, function(x) mean(x))
hmc_sigma_means <- sapply(hmc_sigma_eta_thin, function(x) mean(x))

## Compare the means of the posterior samples to the true parameters
rvgaw_means <- lapply(rvgaw_results, function(x) x$mu[[length(x$mu)]])
rvgaw_precs <- lapply(rvgaw_results, function(x) x$prec[[length(x$prec)]])
chols <- lapply(rvgaw_precs, function(Sigma) solve(chol(Sigma)))
rvgaw_post_samples <- lapply(reps, function(r) rmvnorm(n = 10000, mean = rvgaw_means[[r]], sigma = t(chols[[r]]) %*% chols[[r]]))

rvgaw_phi_means <- sapply(rvgaw_post_samples, function(x) mean(tanh(x[, 1])))
rvgaw_sigma_means <- sapply(rvgaw_post_samples, function(x) mean(sqrt(exp(x[, 2]))))


png(paste0(plot_dir, "compare_means_n", n, 
            temper_info, reorder_info, block_info,
            "_", date, ".png"), width = 800, height = 1200)
par(mfrow = c(2, 1))
plot(hmc_phi_means, rvgaw_phi_means, 
    # xlim = c(phi - 0.1, 1), ylim = c(phi - 0.1, 1),
    main = "R-VGA-Whittle: phi means")
abline(a = 0, b = 1, col = "red", lty = 2)

plot(hmc_sigma_means, rvgaw_sigma_means, 
    # xlim = c(sigma_eta - 0.05, sigma_eta + 0.05), 
    # ylim = c(sigma_eta - 0.05, sigma_eta + 0.05),
    main = "R-VGA-Whittle: sigma means")
abline(a = 0, b = 1, col = "red", lty = 2)

dev.off()

# trouble <- which(hmc_phi_means < 0.5)

# png(paste0(plot_dir, "trouble_n", n, 
#             temper_info, reorder_info, block_info,
#             "_", date, ".png"), width = 800, height = 1200)
# par(mfrow = c(2, 1))

# ind <- 2
# plot(density(hmc_phi_thin[[trouble[ind]]]), main = "HMC phi")
# lines(density(tanh(rvgaw_post_samples[[trouble[ind]]][, 1])), col = "red")
# legend("topright", legend = c("HMC", "R-VGA-Whittle"), col = c("black", "red"), lty = 1)

# plot(density(hmc_sigma_eta_thin[[trouble[ind]]]), main = "HMC sigma")
# lines(density(sqrt(exp(rvgaw_post_samples[[trouble[ind]]][, 2]))), col = "red")
# legend("topright", legend = c("HMC", "R-VGA-Whittle"), col = c("black", "red"), lty = 1)
# dev.off()

## Compare the means of the posterior samples from R-VGAW and HMC

## Plot the ratio of R-VGA-Whittle vs HMC/stochvol standar devs