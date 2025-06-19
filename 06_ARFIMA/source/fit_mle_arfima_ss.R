fit_mle_arfima_ss <- function(pdg, freq, noise_dist = "gaussian", nu) {

    I <- pdg

    ## MLE initial conditions
    if (noise_dist == "t") {
    ini_params <- c(0, 0, 0, 0.5, 3)
    lower_bounds <- c(-1, -1, -0.5, 0.001, 3)
    upper_bounds <- c(1, 1, 0.5, 5, 100)
    } else { # gaussian noise
    ini_params <- c(0, 0, 0, 0.1, 0.001)
    lower_bounds <- c(-1, -1, -0.5, 0.001, 0.001)
    upper_bounds <- c(1, 1, 0.5, 10, 10)
    }

    if (!is.null(nu)) {
        ini_params <- ini_params[-length(ini_params)] # remove nu from initial params
        lower_bounds <- lower_bounds[-length(lower_bounds)] # remove nu from lower bounds
        upper_bounds <- upper_bounds[-length(upper_bounds)] # remove nu from upper bounds
    }

    mle <- optim(par = ini_params, 
                fn = function(params, noise_dist, nu) {
                    phi_i <- params[1]
                    theta_i <- params[2]
                    d_i <- params[3]
                    sigma_i <- params[4]

                    if (!is.null(nu)) {
                        nu_i <- nu # fixed value for nu
                    } else {
                        nu_i <- params[5]
                    }
                    
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
                nu = nu,
                noise_dist = noise_dist,
                method = "L-BFGS-B", 
                lower = lower_bounds, 
                upper = upper_bounds,
                control = list(maxit = 10000))

    return(mle)
}


fit_mle_arfima_ss1 <- function(pdg, freq, noise_dist = "gaussian") {

    ### This function does NOT use parameter transformations 
    I <- pdg

    ## MLE initial conditions
    if (noise_dist == "t") {
    ini_params <- c(0, 0, 0, 0.001, 3)
    lower_bounds <- c(-1, -1, -0.5, 0.001, 3)
    upper_bounds <- c(1, 1, 0.5, 5, 100)
    } else { # gaussian noise
    ini_params <- c(0, 0, 0, 0.1, 0.001)
    lower_bounds <- c(-1, -1, -0.5, 0.001, 0.001)
    upper_bounds <- c(1, 1, 0.5, 10, 10)
    }

    mle <- optim(par = ini_params, 
                fn = function(params, noise_dist) {
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
                noise_dist = noise_dist,
                method = "L-BFGS-B", 
                lower = lower_bounds, 
                upper = upper_bounds,
                control = list(maxit = 10000))

    return(mle)
}

fit_mle_arfima_ss2 <- function(pdg, freq, noise_dist = "t") {

    ### This function uses transformations to ensure parameters are within bounds
    I <- pdg

    ## MLE initial conditions
    # if (noise_dist == "t") {
    ini_params <- c(atanh(0.3), atanh(0.7), atanh(2*0.25), log(1^2), 3)
    lower_bounds <- rep(-10, 5)
    upper_bounds <- rep(10, 5)
    # } else { # gaussian noise
    # ini_params <- c(0, 0, 0, 0.1, 0.001)
    # lower_bounds <- c(-1, -1, -0.5, 0.001, 0.001)
    # upper_bounds <- c(1, 1, 0.5, 10, 10)
    # }

    mle <- optim(par = ini_params, 
                fn = function(params, noise_dist, nu = NULL) {
                    phi_i <- tanh(params[1])
                    theta_i <- tanh(params[2])
                    d_i <- 0.5*tanh(params[3])
                    sigma_i <- sqrt(exp(params[4]))
                    nu_i <- ifelse(noise_dist == "t", 2 + exp(params[5]), sqrt(exp(params[5])))
                    
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
                noise_dist = noise_dist,
                method = "L-BFGS-B", 
                lower = lower_bounds, 
                upper = upper_bounds,
                control = list(maxit = 10000))

    mle_par <- mle$par
    mle_og_scale <- c(tanh(mle_par[1]), tanh(mle_par[2]), 0.5*tanh(mle_par[3]), sqrt(exp(mle_par[4])))

    mle_nu <- ifelse(noise_dist == "t", 2 + exp(mle_par[5]), sqrt(exp(mle_par[5])))
    mle_og_scale <- c(mle_og_scale, mle_nu)

    return(mle_og_scale)
}





