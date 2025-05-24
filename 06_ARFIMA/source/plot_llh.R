## Plot Whittle likelihood for ARFIMA(1, d, 1) model
rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(mvtnorm)
library(arfima)
library(fracdiff)

source("./source/compute_periodogram.R")
source("./source/compute_arfima_spec_dens.R")

## Generate ARFIMA data
set.seed(2025)
n <- 2000
phi <- 0.3
theta <- 0.7
d <- 0.25
sigma_eta <- 1
nu <- 0.5

x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series
y <- x + rnorm(n, mean = 0, sd = nu) # ARFIMA + noise

# Compute the periodogram
pgram_output <- compute_periodogram(data = y)
freq <- pgram_output$freq
I <- pgram_output$periodogram

## Compute Whittle log likelihood
nu_vals <- seq(0.01, 10, length.out = 500)

llh <- c()
for (i in 1:length(nu_vals)) {
  nu_i <- nu_vals[i]

  out <- arfima_spec_dens(n = length(data), phi = phi, 
                            d = d, theta = theta, 
                            sigma = sigma_eta, 
                            nu = nu_i,
                            I = I,
                            freq = freq)

    llh[i] <- out$log_likelihood
}

png("./plots/whittle_llh_arfima.png", width = 800, height = 600)
plot(nu_vals, llh, type = "l", 
     xlab = "Frequency", ylab = "Log Likelihood",
     main = paste("Whittle Log Likelihood for ARFIMA(1, d, 1) with d =", d))
abline(v = nu_vals[which.max(llh)], col = "red", lty = 2)
legend("topright", legend = paste("Max LLH at freq =", round(freq[which.max(llh)], 4)),
       col = "red", lty = 2, bty = "n")
dev.off()
