arfima_spec_dens <- function(n, phi, d, theta, sigma, nu = 1, freq, I) {
  # Compute the spectral density of an ARFIMA process
  # phi: AR coefficients
  # d: fractional differencing parameter
  # theta: MA coefficients
  # nfreq: number of frequency points to compute
  # nu: actually not used, but meant to be the measurement noise sd (might add later)
  
  
  # Create the frequency grid
  ## Fourier frequencies
    # k <- seq(-ceiling(n/2)+1, floor(n/2), 1)
    # k_in_likelihood <- k[k >= 1 & k <= floor((n-1)/2)]
    # freq <- 2 * pi * k_in_likelihood / n
  
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

    spec_dens_x <- sigma^2 * Mod(1 - exp(-1i * freq))^(-2 * d) * 
                                    Mod(Theta_q / Phi_p)^2

    if (!is.null(nu)) {
      spec_dens_eps <- nu^2 # nu = 10
    } else {
      spec_dens_eps <- 0 # no measurement noise
    }
    
    # Maybe feed this through the compute_grad function itself?
    spec_dens <- spec_dens_x + spec_dens_eps
    log_likelihood <- - log(spec_dens) - I / spec_dens
    log_likelihood <- sum(log_likelihood)

  return(list(freq = freq,
              spec_dens_x = spec_dens_x, 
              spec_dens_eps = spec_dens_eps,
              log_likelihood = log_likelihood))
} 