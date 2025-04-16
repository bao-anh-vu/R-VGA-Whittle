## Stochvol package

setwd("~/R-VGA-Whittle/02_SV/")

rm(list = ls())

library(mvtnorm)
library(stochvol)

## Flags
date <- "20240214" # "20230918" #the 20230918 version has sigma_eta = sqrt(0.1)

n <- 2000#0
phi <- 0.9

## Read data
phi_string <- sub("(\\d+)\\.(\\d+)", "\\1\\2", toString(phi)) ## removes decimal point fron the number
print("Reading saved data...")
sv_data <- readRDS(file = paste0("./data/sv_data_n", n, "_phi", phi_string, "_", date, ".rds"))

y <- sv_data$y
x <- sv_data$x
phi <- sv_data$phi
sigma_eta <- sv_data$sigma_eta
sigma_eps <- sv_data$sigma_eps

plot(y, type = "l", main = "Simulated data", ylab = "y", xlab = "t")

## Estimate kappa, then estimate mu based on kappa
kappa_est <- sqrt(exp(mean(log(y^2)) - (digamma(1/2) + log(2))))
mu_fixed <- 2 * log(kappa_est)

## Then plug the fixed value of mu into the model
res_sv <- svsample(y, designmatrix = "ar1") #,
                    # priormu = c(mu_fixed, 1e-8))

summary(res_sv)

png(filename = paste0("./plots/stochvol_phi", phi_string, "_n", n, "_", date, ".png"), width = 800, height = 600)
plot(res_sv, showobs = FALSE)
dev.off()
