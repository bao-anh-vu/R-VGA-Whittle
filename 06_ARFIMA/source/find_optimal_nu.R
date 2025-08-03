find_optimal_nu <- function(pdg, freq, noise_dist = "t") {
    
    ## MLE
    nu_vals <- seq(3, 30, by = 0.1)
    mle <- parallel::mclapply(nu_vals, function(nu_val) {
        fit_mle_arfima_ss(pdg = I, freq = freq, noise_dist = noise_dist, nu = nu_val)
    },
    mc.cores = 10L)
    # mle <- fit_mle_arfima_ss(pdg = I, freq = freq, noise_dist = noise_dist, nu = nu_val)



    mle_pars <- lapply(mle, function(x) x$par)
    mle_llh <- sapply(mle, function(x) -x$value)
    ind <- which.max(mle_llh)
    mle <- c(mle_pars[[ind]], nu_vals[[ind]])

    png(paste0("./plots/mle_arfima_ss_", noise_dist, ".png"), width = 500, height = 300)
    plot(nu_vals, mle_llh, type = "l", xlab = "nu", ylab = "max log-likelihood")
    abline(v = nu, col = "black", lty = 2)
    abline(v = nu_vals[[ind]], col = "red", lty = 2)
    dev.off()

    return(mle)
}

