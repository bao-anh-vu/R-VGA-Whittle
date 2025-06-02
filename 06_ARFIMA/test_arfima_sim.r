# Load necessary package
rm(list = ls())

library(arfima)
library(fracdiff)
# Set seed for reproducibility
set.seed(2025)

# Simulate ARMA(1,1) with ar = 0.7, ma = 0.5
n <- 10000
true_ar <- 0.9
true_ma <- 0.5
# x <- arfima.sim(n = n, model = list(ar = true_ar, ma = true_ma, dfrac = 0))
x <- fracdiff.sim(n = n, ar = true_ar, ma = true_ma, d = 0)
x2 <- arima.sim(n = n, model = list(ar = true_ar, ma = true_ma, dfrac = 0))

# Fit the ARMA(1,1) model using arima()
fit <- arima(x$series, order = c(1, 0, 1), include.mean = FALSE)
fit2 <- arima(x2, order = c(1, 0, 1), include.mean = FALSE)

# Print true and estimated coefficients
cat("True AR coefficient:", true_ar, "\n")
cat("True MA coefficient:", true_ma, "\n\n")
cat("Estimated coefficients:\n")
print(fit$coef)
