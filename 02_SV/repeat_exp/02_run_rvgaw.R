setwd("~/R-VGA-Whittle/02_SV/repeat_exp/")

rm(list = ls())

# library(coda)
# library(Deriv)
# library(cmdstanr)
library(mvtnorm)
library(tensorflow)
reticulate::use_condaenv("myenv", required = TRUE)
library(keras)
# library(stats)
# library(bspec)
# library(tidyr)
# library(ggplot2)
# library(grid)
# library(gridExtra)
# library(gtable)
# library(stochvol)

source("./source/compute_whittle_likelihood_sv.R")
# source("./source/run_rvgaw_sv_tf.R")
source("./source/run_rvgaw_sv_block.R")
# source("./source/run_mcmc_sv.R")
# source("./source/run_hmc_sv.R")
source("./source/compute_periodogram.R")
source("./source/find_cutoff_freq.R")

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

date <- "20250508" #"20230918" #the 20230918 version has sigma_eta = sqrt(0.1)
phi <- 0.99
n <- 2000

## For the result filename
phi_string <- sub("(\\d+)\\.(\\d+)", "\\1\\2", toString(phi)) ## removes decimal point fron the number

## Directories
result_directory <- paste0("./results/phi", phi_string, "/")
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

#############################
##      R-VGA-Whittle      ##
#############################

## R-VGA flags
use_tempering <- T
temper_first <- T
reorder <- 0 #"decreasing" # or decreasing # or a number
reorder_seed <- 2024
prior_type <- ""
transform <- "arctanh"

prior_mean <- c(2, -3) 
prior_var <- diag(c(0.5, 0.5)) 

S <- 1000L
blocksize <- 100 # set to 0 for no blocking
n_indiv <- 0 #find_cutoff_freq(y, nsegs = 25, power_prop = 1/2)$cutoff_ind #100

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

rvgaw_filepath <- paste0(result_directory, "rvga_whittle_results_n", n, 
                         temper_info, reorder_info, block_info,
                         "_", formatC(rep, digits = 2, flag = "0"), "_", date, ".rds")

rvgaw_results <- run_rvgaw_sv(y = y, 
                            prior_mean = prior_mean, prior_var = prior_var, 
                            deriv = "tf", 
                            n_post_samples = 10000,
                            S = S, use_tempering = use_tempering, 
                            temper_first = temper_first,
                            reorder = reorder,
                            n_temper = n_temper,
                            temper_schedule = temper_schedule, 
                            transform = transform,
                            blocksize = blocksize,
                            n_indiv = n_indiv)
  
saveRDS(rvgaw_results, rvgaw_filepath)
