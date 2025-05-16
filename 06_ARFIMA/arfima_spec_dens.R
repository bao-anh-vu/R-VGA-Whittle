## Compute spectral density of an ARFIMA process
setwd("~/R-VGA-Whittle/06_ARFIMA")

rm(list = ls())

library(mvtnorm)
library(arfima)
library(LSTS)
library(tensorflow)
reticulate::use_condaenv("myenv", required = TRUE)
library(keras)

source("./source/run_rvgaw_arfima.R")
source("./source/compute_grad_ss.R")

################## Some code to limit tensorflow memory usage ##################

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

################## End of code to limit tensorflow memory usage ##################

arfima_spec_dens <- function(n, phi, d, theta, noise_var, nu) {
  # Compute the spectral density of an ARFIMA process
  # phi: AR coefficients
  # d: fractional differencing parameter
  # theta: MA coefficients
  # nfreq: number of frequency points to compute
  
  # Create the frequency grid
  ## Fourier frequencies
    k <- seq(-ceiling(n/2)+1, floor(n/2), 1)
    k_in_likelihood <- k[k >= 1 & k <= floor((n-1)/2)]
    freq <- 2 * pi * k_in_likelihood / n
  
  # Compute the spectral density using the ARFIMA parameters

    arg <- exp(-1i * freq) 

    powers <- lapply(1:length(phi), function(i) phi[i] * arg^i)
    Phi_p <- 1 - Reduce(`+`, powers)
   
    powers <- lapply(1:length(theta), function(i) theta[i] * arg^i)
    Theta_q <- 1 + Reduce(`+`, powers)
    
    # cat("T1 = ", noise_var/(2*pi), "\n")
    # cat("T2 = ", Mod(1 - exp(-1i * freq[1]))^(-2 * d), "\n")
    # cat("T3 = ", head(Mod(Theta_q / Phi_p)^2, 1), "\n")
    # cat("T4 = ", Mod(Theta_q)[1], ", T5 = ", Mod(Phi_p)[1], "\n")

  # Compute the spectral density

    spec_dens_x <- noise_var/(2*pi) * Mod(1 - exp(-1i * freq))^(-2 * d) * 
                                    Mod(Theta_q / Phi_p)^2

    spec_dens_eps <- 1/(2*pi) * (nu/(nu - 2)) # nu = 10
  
  return(list(spec_dens_x = spec_dens_x, 
              spec_dens_eps = spec_dens_eps))
} 

phi <- 0.7
theta <- 0.3
d <- 0.15
sigma_eta <- 1
n <- 1000
nu <- 10

test1 <- arfima_spec_dens(n = n, phi = phi, d = d, theta = theta, 
                          noise_var = sigma_eta^2, nu = nu)


k <- seq(-ceiling(n/2)+1, floor(n/2), 1)
k_in_likelihood <- k[k >= 1 & k <= floor((n-1)/2)]
freq <- 2 * pi * k_in_likelihood / n

test2 <- spectral.density(ar = phi, ma = theta, d = d, sd = sigma_eta, lambda = freq)
png("~/R-VGA-Whittle/06_ARFIMA/plots/arfima_spec_dens.png", width = 800, height = 600)
plot(freq, test2, type = "l", main = paste0("ARFIMA(1, ", d, ", 1) with phi = ", phi,
     ", theta = ", theta, ", sigma_eta = ", sigma_eta), 
     xlab = "Frequency", ylab = "Spectral Density", ylim = c(0, max(test2)))
lines(freq, test1$spec_dens_x, col = "red")
legend("topright", legend = c("LSTS", "manual"), col = c("black", "red"), lty = 1)
dev.off()



## Tensorflow version

s1 <- c(atanh(phi), atanh(theta), atanh(2*d), log(sigma_eta^2), log(nu-2))
s2 <- c(atanh(phi), atanh(theta), atanh(2*d), log(sigma_eta^2), log(nu-2))
# s2 <- c(atanh(0.3), atanh(0.7), 0.2, log(1), log(nu-2))

S <- 2L
samples_tf <- tf$Variable(t(matrix(c(s1, s2), nrow = length(s1), ncol = S)), dtype = "float64")

# S <- 100L
# prior_mean <- c(0, 0, 0, 0, 0)
# prior_var <- diag(5) # identity matrix
# samples <- rmvnorm(S, prior_mean, prior_var)
# samples_tf <- tf$Variable(samples, dtype = "float64")

# freq_i <- tf$cast(freq[1], dtype = "complex128") # 1 x 1 x 1
# blocksize <- 1L


# freq_i <- tf$reshape(freq_i, c(1L, blocksize, 1L)) # 1 x blocksize x 1

# # nfreq <- as.integer(length(freq_i))
# phi_s <- tf$math$tanh(samples_tf[, 1])
# phi_s <- tf$reshape(phi_s, c(length(phi_s), 1L, 1L)) # S x 1 x 1

# theta_s <- tf$math$tanh(samples_tf[, 2])
# theta_s <- tf$reshape(theta_s, c(length(theta_s), 1L, 1L)) # S x 1 x 1

# d_s <- 0.5 * tf$tanh(samples_tf[, 3]) # fixed for now

# sigma_eta2_s <- tf$math$exp(samples_tf[, 4])
# sigma_eta2_s <- tf$reshape(sigma_eta2_s, c(dim(sigma_eta2_s), 1L, 1L))
# sigma_eta2_tiled <- tf$tile(sigma_eta2_s, c(1L, blocksize, 1L))

# nu_s <- tf$math$exp(samples_tf[, 5]) + 2

# term1 <- tf$multiply(tf$constant(1 / (2*pi), dtype = "float64"), sigma_eta2_tiled)

# arg <- tf$math$exp(-1i * freq_i)
# # base <- tf$cast(tf$math$abs(1 - arg), "float64")
# term2 <- tf$transpose(tf$math$abs(1 - arg)^(-2 * d_s))

# term3_num <- 1 + tf$multiply(tf$cast(theta_s, "complex128"), arg)
# term3_den <- 1 - tf$multiply(tf$cast(phi_s, "complex128"), arg)
# term3 <- tf$math$square(tf$math$abs(tf$divide(term3_num, term3_den)))

# ## Calculate the spectral density of x
# spec_dens_x_tf <- tf$multiply(tf$multiply(term1, term2), term3)

# spec_dens_eps_tf <- tf$multiply(tf$constant(1 / (2*pi), dtype = "float64"), tf$divide(nu_s, nu_s - 2))


# Maybe feed this through the compute_grad function itself?
I_i <- 1
spec_dens <- test1$spec_dens_x + test1$spec_dens_eps
log_likelihood <- - log(spec_dens) - I_i / spec_dens
print(head(log_likelihood))

tf_out <- compute_grad(samples_tf = samples_tf, I_i = I_i, 
                    freq_i = freq[2], blocksize = 1L)
head(tf_out$spec_dens_x_tf)
head(test1$spec_dens_x)

head(tf_out$spec_dens_eps_tf)
head(test1$spec_dens_eps)

## Log likelihood

tf_out$log_likelihood
