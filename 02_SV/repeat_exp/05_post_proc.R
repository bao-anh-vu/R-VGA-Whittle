## Post-processing

setwd("~/R-VGA-Whittle/02_SV/repeat_exp/")

rm(list = ls())

library(mvtnorm)
library(dplyr)
library(ggplot2)
library(gridExtra)

source("source/plot_ci.R")

## Read results
# rep <- as.numeric(commandArgs(trailingOnly = TRUE))
# cat("Rep:", rep, "\n")

reps <- 1:50

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
hmcw_results <- list()

for (rep in reps) {
  rvgaw_filepath <- paste0(result_dir, "rvga_whittle_results_n", n, 
                           temper_info, reorder_info, block_info,
                           "_", formatC(rep, digits = 2, flag = "0"), "_", date, ".rds")
  
  hmc_filepath <- paste0(
    result_dir, "hmc_results_n", n, "_phi", phi_string, 
    "_", formatC(rep, digits = 2, flag = "0"), "_", date, ".rds"
  )
    hmcw_filepath <- paste0(
    result_dir, "hmcw_results_n", n, "_phi", phi_string, 
    "_", formatC(rep, digits = 2, flag = "0"), "_", date, ".rds"
)

 if (file.exists(rvgaw_filepath)) {
    rvgaw_results[[rep]] <- readRDS(rvgaw_filepath)
  } else {
    cat("File not found:", rvgaw_filepath, "\n")
  }

  if (file.exists(hmc_filepath)) {
    hmc_results[[rep]] <- readRDS(hmc_filepath)
  } else {
    cat("File not found:", hmc_filepath, "\n")
  }

  if (file.exists(hmcw_filepath)) {
    hmcw_results[[rep]] <- readRDS(hmcw_filepath)
  } else {
    cat("File not found:", hmcw_filepath, "\n")
  }

 
}

# HMC
hmc_phi <- lapply(hmc_results, function(x) c(x$draws[-(1:burn_in), , 1])) # tanh(hmc.theta_phi)
hmc_sigma_eta <- lapply(hmc_results, function(x) c(x$draws[-(1:burn_in), , 2])) # sqrt(exp(hmc.theta_sigma))

thin_interval <- 100
inds <- seq(1, length(hmc_phi[[1]]), by = thin_interval)
hmc_phi_thin <- lapply(hmc_phi, function(x) x[inds])
hmc_sigma_eta_thin <- lapply(hmc_sigma_eta, function(x) x[inds])

hmc_phi_means <- sapply(hmc_phi_thin, function(x) mean(x))
hmc_sigma_means <- sapply(hmc_sigma_eta_thin, function(x) mean(x))

hmc_phi_ci <- lapply(hmc_phi_thin, function(x) quantile(x, probs = c(0.025, 0.975)))
hmc_sigma_ci <- lapply(hmc_sigma_eta_thin, function(x) quantile(x, probs = c(0.025, 0.975)))



# HMCW
hmcw_phi <- lapply(hmcw_results, function(x) c(x$draws[-(1:burn_in), , 1])) # tanh(hmc.theta_phi)
hmcw_sigma_eta <- lapply(hmcw_results, function(x) c(x$draws[-(1:burn_in), , 2])) # sqrt(exp(hmc.theta_sigma))

thin_interval <- 100
inds <- seq(1, length(hmcw_phi[[1]]), by = thin_interval)
hmcw_phi_thin <- lapply(hmcw_phi, function(x) x[inds])
hmcw_sigma_eta_thin <- lapply(hmcw_sigma_eta, function(x) x[inds])

hmcw_phi_means <- sapply(hmcw_phi_thin, function(x) mean(x))
hmcw_sigma_means <- sapply(hmcw_sigma_eta_thin, function(x) mean(x))

hmcw_phi_ci <- lapply(hmcw_phi_thin, function(x) quantile(x, probs = c(0.025, 0.975)))
hmcw_sigma_ci <- lapply(hmcw_sigma_eta_thin, function(x) quantile(x, probs = c(0.025, 0.975)))

## Compare the means of the posterior samples to the true parameters
rvgaw_means <- lapply(rvgaw_results, function(x) x$mu[[length(x$mu)]])
rvgaw_precs <- lapply(rvgaw_results, function(x) x$prec[[length(x$prec)]])
chols <- lapply(rvgaw_precs, function(Sigma) solve(chol(Sigma)))
rvgaw_post_samples <- lapply(reps, function(r) rmvnorm(n = 10000, mean = rvgaw_means[[r]], sigma = t(chols[[r]]) %*% chols[[r]]))

rvgaw_phi <- lapply(rvgaw_post_samples, function(x) tanh(x[, 1]))
rvgaw_sigma_eta <- lapply(rvgaw_post_samples, function(x) sqrt(exp(x[, 2])))

rvgaw_phi_means <- sapply(rvgaw_phi, mean)
rvgaw_sigma_means <- sapply(rvgaw_sigma_eta, mean)

rvgaw_phi_ci <- lapply(rvgaw_phi, function(x) quantile(x, probs = c(0.025, 0.975)))
rvgaw_sigma_ci <- lapply(rvgaw_sigma_eta, function(x) quantile(x, probs = c(0.025, 0.975)))



## Plot 95% credible intervals

hmc_phi_df <- data.frame(
    lower = sapply(hmc_phi_ci, function(x) x[1]),
    upper = sapply(hmc_phi_ci, function(x) x[2]),
    mean = hmc_phi_means,
    true = rep(phi, length(hmc_phi_means)),
    method = "HMC"
)

hmc_sigma_df <- data.frame(
    lower = sapply(hmc_sigma_ci, function(x) x[1]),
    upper = sapply(hmc_sigma_ci, function(x) x[2]),
    mean = hmc_sigma_means,
    true = rep(sigma_eta, length(hmc_sigma_means)),
    method = "HMC"
)

hmcw_phi_df <- data.frame(
    lower = sapply(hmcw_phi_ci, function(x) x[1]),
    upper = sapply(hmcw_phi_ci, function(x) x[2]),
    mean = hmcw_phi_means,
    true = rep(phi, length(hmcw_phi_means)),
    method = "HMCW"
)

hmcw_sigma_df <- data.frame(
    lower = sapply(hmcw_sigma_ci, function(x) x[1]),
    upper = sapply(hmcw_sigma_ci, function(x) x[2]),
    mean = hmcw_sigma_means,
    true = rep(sigma_eta, length(hmcw_sigma_means)),
    method = "HMCW"
)

rvgaw_phi_df <- data.frame(
    lower = sapply(rvgaw_phi_ci, function(x) x[1]),
    upper = sapply(rvgaw_phi_ci, function(x) x[2]),
    mean = rvgaw_phi_means,
    true = rep(phi, length(rvgaw_phi_means)),
    method = "R-VGAW"
)

rvgaw_sigma_df <- data.frame(
    lower = sapply(rvgaw_sigma_ci, function(x) x[1]),
    upper = sapply(rvgaw_sigma_ci, function(x) x[2]),
    mean = rvgaw_sigma_means,
    true = rep(sigma_eta, length(rvgaw_sigma_means)),
    method = "R-VGAW"
)

phi_df <- rbind(hmc_phi_df, hmcw_phi_df, rvgaw_phi_df)
sigma_df <- rbind(hmc_sigma_df, hmcw_sigma_df, rvgaw_sigma_df)

## Compare the means of the posterior samples to the true parameters
png(paste0(plot_dir, "compare_means_hmc_n", n, 
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

png(paste0(plot_dir, "compare_means_hmcw_n", n, 
            temper_info, reorder_info, block_info,
            "_", date, ".png"), width = 800, height = 1200)
par(mfrow = c(2, 1))
plot(hmcw_phi_means, rvgaw_phi_means, 
    # xlim = c(phi - 0.1, 1), ylim = c(phi - 0.1, 1),
    main = "R-VGA-Whittle: phi means")
abline(a = 0, b = 1, col = "red", lty = 2)

plot(hmcw_sigma_means, rvgaw_sigma_means, 
    # xlim = c(sigma_eta - 0.05, sigma_eta + 0.05), 
    # ylim = c(sigma_eta - 0.05, sigma_eta + 0.05),
    main = "R-VGA-Whittle: sigma means")
abline(a = 0, b = 1, col = "red", lty = 2)

dev.off()

## 95% credible intervals
hmc_plots <- plot_ci(hmc_phi_df, hmc_sigma_df)
hmcw_plots <- plot_ci(hmcw_phi_df, hmcw_sigma_df)
rvgaw_plots <- plot_ci(rvgaw_phi_df, rvgaw_sigma_df)

R <- length(reps)

phi_plot_df <- phi_df %>% filter(method != "HMC") # plot R-VGAW vs HMCW first
sigma_plot_df <- sigma_df %>% filter(method != "HMC") # plot R-VGAW vs HMCW first

phi_plot <- ggplot() + 
    geom_errorbar(data = phi_plot_df, 
                  aes(x = rep(1:R, 2), color = method, ymin = lower, ymax = upper), 
                  position = position_dodge(width = .75), 
                  width = 0.5) +
    geom_point(data = phi_plot_df, aes(x = rep(1:R, 2), y = mean, color = method)) + 
    geom_hline(yintercept = phi, lty = 2) +
    xlab("Replicate") +
    ylab(bquote(phi)) +
    theme_bw() +
    theme(text = element_text(size = 20)) 

sigma_plot <- ggplot() +
    geom_errorbar(data = sigma_plot_df, 
                  aes(x = rep(1:R, 2), color = method, ymin = lower, ymax = upper), 
                  position = position_dodge(width = .75), 
                  width = 0.5) +
    geom_point(data = sigma_plot_df, aes(x = rep(1:R, 2), y = mean, color = method),
              position = position_dodge(width = .75)) + 
    geom_hline(yintercept = sigma_eta, lty = 2) +
    xlab("Replicate") +
    ylab(bquote(sigma[eta])) +
    theme_bw() +
    theme(text = element_text(size = 20))

png(paste0(plot_dir, "ci_plots_n", n, 
            temper_info, reorder_info, block_info,
            "_", date, ".png"), width = 800, height = 600)
# print(phi_plot)
grid.arrange(phi_plot, sigma_plot, nrow = 2)
# grid.arrange(grobs = c(hmc_plots, hmcw_plots, rvgaw_plots), ncol = 2)
# grid.arrange(hmc_plots, ncol = 2)
dev.off()

## Compute coverage probabilities
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