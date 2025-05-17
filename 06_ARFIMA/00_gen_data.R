## SSM with underlying ARFIMA model
rm(list = ls())
setwd("~/R-VGA-Whittle/06_ARFIMA")

library(mvtnorm)
library(fracdiff)

# library(LSTS)
# library(fracdiff)
# library(polynom)
# source("./source/run_rvgaw_arfima.R")
# source("./source/compute_periodogram.R")
# source("./source/compute_grad_ss.R")

## Simulate ARFIMA(1, 0.25, 1) process
set.seed(2025)
n <- 2000
phi <- 0.2
theta <- 0.5
d <- 0.25
sigma_eta <- 1
nu <- 0.5 #20
# x <- arfima.sim(n, model = list(phi = phi, dfrac = d, theta = theta, sigma2 = sigma_eta^2))
x <- fracdiff.sim(n = n, ar = phi, ma = -theta, d = d, sd = sigma_eta)$series
# y <- x + rt(n, df = nu) # ARFIMA + noise
y <- x + rnorm(n, mean = 0, sd = nu) # ARFIMA + noise

png("sim.png", width = 800, height = 600)
# par(mfrow = c(2, 1))
plot(y[1:200], type = "l", main = paste0("ARFIMA(1, ", d, ", 1)"))
# plot(sim2[1:200], type = "l", main = "ARFIMA(2,0,1), dfrac = 0")
dev.off()

data <- list(
  x = x,
  y = y,
  phi = phi,
  theta = theta,
  d = d,
  sigma_eta = sigma_eta,
  nu = nu
)

## Save data
saveRDS(data, file = "./data/arfima_data.rds")

## MLE fit using arfima package
# fit <- arfima(x, order = c(1, 0, 1), back=TRUE)
# print(fit)

# Call:
# arfima(z = x, order = c(1, 0, 1), back = TRUE)

# Coefficients for fits:
#              Coef.1:     SE.1:     
# phi(1)        0.187559    0.0333757
# theta(1)      0.471483    0.0428043
# d.f           0.241706    0.0218733
# Fitted mean   0.0153795   0.061372 
# logl         -18.5211              
# sigma^2       1.00405  

#----------------------------------------------------------------
## Whittle MLE fit
# fit_whittle <- LS.whittle(
#   series = x, start = c(0.2, 0.2, 0.2), order = c(p = 1, q = 1)
# )

## Whittle MLE fit using afmtools package
# source("./afmtools/R/arfima.whittle.loglik.R")
# source("./afmtools/R/arfima.whittle.R")
# source("./afmtools/R/check.parameters.arfima.R")
# source("./afmtools/R/per.arfima.R")
# source("./afmtools/R/spectrum.arma.R")
# source("./afmtools/R/spectrum.arfima.R")

# fit_whittle <- arfima.whittle(
#   series = x, 
#   nar = 1, 
#   nma = 1, 
#   fixed = NA
# )
