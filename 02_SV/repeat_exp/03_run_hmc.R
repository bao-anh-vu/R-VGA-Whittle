setwd("~/R-VGA-Whittle/02_SV/repeat_exp/")

rm(list = ls())

library(cmdstanr)
source("./source/run_hmc_sv.R")

## Read data
date <- "20250508" #"20230918" #the 20230918 version has sigma_eta = sqrt(0.1)
phi <- 0.99
n <- 2000
transform <- "arctanh"

## For the result filename
phi_string <- sub("(\\d+)\\.(\\d+)", "\\1\\2", toString(phi)) ## removes decimal point fron the number

## Directories
result_dir <- paste0("./results/phi", phi_string, "/")
data_dir <- paste0("./data/phi", phi_string)

## Read data
rep <- as.numeric(commandArgs(trailingOnly = TRUE))
cat("Rep:", rep, "\n")
sv_data <- readRDS(paste0(data_dir, "/sv_data_n", n, "_phi", phi_string, 
                                  "_", formatC(rep, digits = 2, flag = "0"), "_", date, ".rds"))
y <- sv_data$y
x <- sv_data$x
phi <- sv_data$phi
sigma_eta <- sv_data$sigma_eta
sigma_eps <- sv_data$sigma_eps

## Run HMC
prior_mean <- c(2, -3) 
prior_var <- diag(c(0.5, 0.5)) 

n_post_samples <- 10000 # per chain 
burn_in <- 5000 # per chain
n_chains <- 2

hmc_filepath <- paste0(
    result_dir, "hmc_results_n", n, "_phi", phi_string, 
    "_", formatC(rep, digits = 2, flag = "0"), "_", date, ".rds"
)

hmc_results <- run_hmc_sv(data = y, transform = transform,
                            prior_mean = prior_mean, prior_var = prior_var,
                            iters = n_post_samples, 
                            burn_in = burn_in,
                            n_chains = n_chains)
  
saveRDS(hmc_results, hmc_filepath)