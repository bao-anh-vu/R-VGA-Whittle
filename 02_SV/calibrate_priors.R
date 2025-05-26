setwd("~/R-VGA-Whittle/02_SV/")

rm(list = ls())

library(mvtnorm)
library(stochvol)

## Calibrate the priors using optim()
### First calibrate the prior for phi
### Find the closest possible Beta(a, b) distribution to the R-VGAW prior for phi
### Calculate 3 quantiles c(0.025, 0.5, 0.975) and match it to those from the R-VGAW prior

N <- 50000
qs <- c(0.05, 0.5, 0.95)
rvgaw_phi <- tanh(rnorm(N, 2, sqrt(0.5)))
rvgaw_phi_q <- quantile(rvgaw_phi, probs = qs)
    
phi_quantiles <- function(params, N, rvgaw_quantiles) {
    a <- exp(params[1])
    b <- exp(params[2])
    
    stv_theta_phi <- rbeta(N, a, b)
    stv_phi <- 2*stv_theta_phi - 1

    stv_quantiles <- quantile(stv_phi, probs = qs)
    
    diff <- sum((stv_quantiles - rvgaw_quantiles)^2)
    cat("a = ", a, "b = ", b, "stv_quantiles = ", stv_quantiles, "\n")

    return(diff) # Return the sum of absolute differences between quantiles
}

optim_phi <- optim(par = c(10, 1), fn = phi_quantiles, N = N, rvgaw_quantiles = rvgaw_phi_q)
phi_hpars <- exp(optim_phi$par)

## Do the same for sigma_eta
rvgaw_sigma <- sqrt(exp(rnorm(N, -3, sqrt(0.5))))
rvgaw_sigma_q <- quantile(rvgaw_sigma, probs = qs)

sigma_quantiles <- function(params, N, rvgaw_quantiles) {
    r <- exp(params[1])
    
    ## stochvol priors
    stv_sigma <- sqrt(rgamma(N, shape = 1/2, rate = r/2))

    stv_quantiles <- quantile(stv_sigma, probs = qs)
    
    diff <- sum((stv_quantiles - rvgaw_quantiles)^2)
    cat("r = ", r, "diff = ", diff, "\n")

    return(diff) # Return the sum of absolute differences between quantiles
}
optim_sigma <- optim(par = 10, fn = sigma_quantiles, N = N, 
                    rvgaw_quantiles = rvgaw_sigma_q,
                    method = "Brent", lower = 0.001, upper = 100)
sigma_hpars <- exp(optim_sigma$par)

## Plot the priors for comparison
stv_theta_phi <- rbeta(N, phi_hpars[1], phi_hpars[2])
stv_phi <- 2 * stv_theta_phi - 1
stv_sigma <- sqrt(rgamma(N, shape = 1/2, rate = sigma_hpars/2))

stochvol_phi_q <- quantile(stv_phi, probs = c(0.025, 0.50, 0.975))
stochvol_sigma_q <- quantile(stv_sigma, probs = c(0.025, 0.50, 0.975))

# qs <- c(0.025, 0.5, 0.975)
mat <- cbind(stochvol_phi_q, rvgaw_phi_q, stochvol_sigma_q, rvgaw_sigma_q)
print(mat)

print(paste0("stochvol_phi: a = ", round(phi_hpars[1], 3), 
            ", b = ", round(phi_hpars[2], 3)))
print(paste0("stochvol_sigma: r = ", round(sigma_hpars, 3)))

png(filename = paste0("./plots/stv_rvgaw_priors.png"), width = 1000, height = 400)
par(mfrow = c(1, 2))
plot(density(stv_phi), main = "phi")
lines(density(rvgaw_phi), col = "red")
legend("topleft", legend = c("stochvol", "R-VGAW"), col = c("black", "red"), lty = 1)
plot(density(stv_sigma), main = "sigma_eta")
lines(density(rvgaw_sigma), col = "red")
legend("topright", legend = c("stochvol", "R-VGAW"), col = c("black", "red"), lty = 1)
dev.off()

# [1] "stochvol_phi: a = 11.081, b = 0.461"
# [1] "stochvol_sigma: r = 23.134"