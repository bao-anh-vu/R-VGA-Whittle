## Stochastic volatility model -- Repeated experiments
setwd("~/R-VGA-Whittle/02_SV/repeat_exp/")

rm(list = ls())

source("./source/gen_sv_data.R")

date <- "20250508" #"20230918" #the 20230918 version has sigma_eta = sqrt(0.1)

reps <- 11:100 # number of repetitions

## Generate data
mu <- 0
phi <- 0.99
sigma_eta <- 0.1 #sqrt(0.1)
sigma_eps <- 1
kappa <- 2
n <- 2000

## Data filename
phi_string <- sub("(\\d+)\\.(\\d+)", "\\1\\2", toString(phi)) ## removes decimal point fron the number
data_dir <- paste0("./data/phi", phi_string)

## Generate data
print("Generating data...")
for (r in reps) {
  set.seed(r)
  sv_data <- gen_sv_data(n, phi, sigma_eta, sigma_eps, kappa)

  saveRDS(sv_data, file = paste0(data_dir, "/sv_data_n", n, "_phi", phi_string, 
                                  "_", formatC(r, digits = 2, flag = "0"), "_", date, ".rds"))
}
