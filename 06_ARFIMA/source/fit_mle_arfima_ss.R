fit_mle_arfima_ss <- function(pdg, freq, noise_dist = "gaussian") {

    I <- pdg

    ## MLE
    if (noise_dist == "t") {
    ini_params <- c(0, 0, 0, 0.001, 3)
    lower_bounds <- c(-0.9999, -0.9999, -0.4999, 0.001, 3)
    upper_bounds <- c(0.9999, 0.9999, 0.4999, 5, 100)
    } else { # gaussian noise
    ini_params <- c(0, 0, 0, 0.001, 0.001)
    lower_bounds <- c(-0.9999, -0.9999, -0.4999, 0.001, 0.001)
    upper_bounds <- c(0.9999, 0.9999, 0.4999, 10, 10)
    }

    mle <- optim(par = ini_params, 
                fn = function(params) {
                    phi_i <- params[1]
                    theta_i <- params[2]
                    d_i <- params[3]
                    sigma_i <- params[4]
                    nu_i <- params[5]
                    out <- arfima_spec_dens(phi = phi_i, 
                                            theta = theta_i, 
                                            d = d_i, 
                                            sigma = sigma_i, 
                                            nu = nu_i,
                                            noise_dist = noise_dist,
                                            I = I,
                                            freq = freq)
                    - out$log_likelihood # minimise the negative log likelihood
                }, 
                method = "L-BFGS-B", 
                lower = lower_bounds, 
                upper = upper_bounds)

    return(mle)
}



