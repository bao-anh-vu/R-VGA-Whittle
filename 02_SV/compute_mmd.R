# Maximum Mean Discrepancy (MMD) for R-VGA-Whittle
setwd("~/R-VGA-Whittle/02_SV/")

rm(list = ls())

library(mvtnorm)
library(coda)
# library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
# library(gtable)
# library(latex2exp)
library(stochvol)
library(eummd)
library(crossmatch)

source("./source/compute_periodogram.R")
source("./source/find_cutoff_freq.R")
source("./source/hmc_diagnostics.R")

## Flags
date <- "20240214" # "20230918" #the 20230918 version has sigma_eta = sqrt(0.1)
# date <- "20230918"

## R-VGA flags
use_tempering <- T
temper_first <- T
reorder <- 0 # "decreasing" # or decreasing # or a number
reorder_seed <- 2024
plot_prior <- F
plot_likelihood_surface <- F
prior_type <- ""
transform <- "arctanh"
plot_trajectories <- F
save_plots <- F


n <- 2000 # 0
phi <- 0.99

## Read data
phi_string <- sub("(\\d+)\\.(\\d+)", "\\1\\2", toString(phi)) ## removes decimal point fron the number
print("Reading saved data...")
sv_data <- readRDS(file = paste0("./data/sv_data_n", n, "_phi", phi_string, "_", date, ".rds"))

y <- sv_data$y
x <- sv_data$x
phi <- sv_data$phi
sigma_eta <- sv_data$sigma_eta
sigma_eps <- sv_data$sigma_eps

## Read results
print("Reading saved results...")
result_directory <- paste0("./results/", transform, "/")

S <- 1000L
# nblocks <- 100
blocksize <- 100
n_indiv <- find_cutoff_freq(y, nsegs = 5, power_prop = 1 / 2)$cutoff_ind # 100

if (use_tempering) {
    n_temper <- 5
    K <- 100
    temper_schedule <- rep(1 / K, K)
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
    # block_info <- paste0("_", nblocks, "blocks", n_indiv, "indiv")
    block_info <- paste0("_", "blocksize", blocksize, "_", n_indiv, "indiv")
} else {
    block_info <- ""
}

rvgaw_filepath <- paste0(
    result_directory, "rvga_whittle_results_n", n,
    "_phi", phi_string, temper_info, reorder_info, block_info,
    prior_type, "_", date, ".rds"
)

stv_filepath <- paste0(
    result_directory, "stv_results_n", n,
    "_phi", phi_string, "_", date, ".rds"
)

hmc_filepath <- paste0(
    result_directory, "hmc_results_n", n,
    "_phi", phi_string, "_", date, ".rds"
)
hmcw_filepath <- paste0(
    result_directory, "hmcw_results_n", n,
    "_phi", phi_string, "_", date, ".rds"
)

rvgaw_results <- readRDS(rvgaw_filepath)
stv_results <- readRDS(stv_filepath)
hmc_results <- readRDS(hmc_filepath)
hmcw_results <- readRDS(hmcw_filepath)

## Extract posterior samples
rvgaw_samples <- rvgaw_results$post_samples
hmc_samples <- hmc_results$draws
hmcw_samples <- hmcw_results$draws

stv_params <- para(stv_results, chain = "all")[, sampled_parameters(stv_results)]
stv_samples_mat <- as.matrix(stv_params)[, 2:3] # take only samples of phi and sigma 
# stv_samples_ls <- lapply(1:ncol(stv_samples_mat), function(i) {
# params_df_long <- pivot_longer(params_df, cols = everything(), names_to = "param", values_to = "val")

## Convert samples to matrices
rvgaw_samples_mat <- matrix(unlist(rvgaw_samples), ncol = length(rvgaw_samples), byrow = F)

burn_in <- 1000
hmc_samples <- hmc_samples[-(1:burn_in), , ]
hmcw_samples <- hmcw_samples[-(1:burn_in), , ]

## Flatten to a N x 2 matrix
hmc_samples_mat <- matrix(hmc_samples, ncol = 2)
hmcw_samples_mat <- matrix(hmcw_samples, ncol = 2)

system.time({
    rvgaw_mmd <- mmd(rvgaw_samples_mat, hmc_samples_mat, kernel = "Gaussian")
    hmcw_mmd <- mmd(hmcw_samples_mat, hmc_samples_mat, kernel = "Gaussian")
})

mmd_df <- cbind(rvgaw_mmd, hmcw_mmd)

write.csv(
    mmd_df,
    file = paste0(result_directory, "mmd_results_n", n, "_phi", phi_string, "_", date, ".csv"),
    row.names = T
)

## Cross-match non-bipartite statistic
# Combine into one dataset
# Z <- rbind(X, Y)
# # Labels: 0 for X, 1 for Y
# group <- c(rep(0, 100), rep(1, 100))

# # Run the cross-match test
# result <- crossmatch.test(Z, group)
