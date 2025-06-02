## Plot Whittle likelihood for ARFIMA(1, d, 1) model
rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(mvtnorm)
library(arfima)
library(fracdiff)
library(parallel)
library(ggplot2)
library(reshape2)

source("./source/compute_periodogram.R")
source("./source/compute_arfima_spec_dens.R")

## Generate ARFIMA data
set.seed(2025)
n <- 10000
phi <- 0.3
theta <- 0.7
d <- 0.25
sigma_eta <- 1
# nu <- 0.5
kappa <- 1

x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series
eps <- rnorm(n, 0, 1)
y <- kappa * exp(x / 2) * eps

# Compute the periodogram
y_tilde <- log(y^2) - mean(log(y^2))
pgram_output <- compute_periodogram(y_tilde)
freq <- pgram_output$freq
I <- pgram_output$periodogram

## Compute Whittle log likelihood
sigma_vals <- seq(0.2, 2, length.out = 100)
nu_vals <- seq(0.2, 2, length.out = 100)

vals <- expand.grid(sigma = sigma_vals, nu = nu_vals)

vals_ls <- lapply(1:nrow(vals), function(i) {
    list(sigma = vals$sigma[i], nu = vals$nu[i])
})

llh <- mclapply(vals_ls, function(params) {
    sigma_i <- params$sigma
    nu_i <- params$nu
    out <- arfima_spec_dens(n = length(y), 
                            phi = phi, 
                            d = d, theta = theta, 
                            sigma = sigma_i, 
                            nu = nu_i,
                            I = I,
                            freq = freq)
    out$log_likelihood
}, mc.cores = 10L)

vals$llh <- unlist(llh)

## Surface plot of log likelihood

# llh_matrix <- matrix(vals$llh, nrow = length(phi_vals), ncol = length(nu_vals))
# llh_df <- melt(llh_matrix)
# colnames(llh_df) <- c("phi", "nu", "llh")
ind_max <- which.max(vals$llh)
opt_sigma <- unique(vals$sigma[vals$sigma == vals$sigma[ind_max]])
opt_nu <- unique(vals$nu[vals$nu == vals$nu[ind_max]])

llh_surf <- ggplot(vals, aes(x = sigma, y = nu, fill = llh)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = paste("Whittle Log Likelihood for ARFIMA(1, d, 1) with d =", d),
       x = "Process noise sd (sigma)", y = "Measurement Noise sd (nu)",
       fill = "Log Likelihood") +
       geom_point(aes(x = opt_sigma, y = opt_nu),
                    color = "black", size = 3) + 
  theme_minimal()

png("./plots/whittle_llh_arfima_sv.png", width = 800, height = 600)
# plot(nu_vals, llh, type = "l", 
#      xlab = "Frequency", ylab = "Log Likelihood",
#      main = paste("Whittle Log Likelihood for ARFIMA(1, d, 1) with d =", d))
# abline(v = nu_vals[which.max(llh)], col = "red", lty = 2)
# legend("topright", legend = paste("Max LLH at freq =", round(freq[which.max(llh)], 4)),
#        col = "red", lty = 2, bty = "n")
print(llh_surf)
dev.off()
