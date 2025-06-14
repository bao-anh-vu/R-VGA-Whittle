## MCMC diagnostics for the HMC model

hmc_diagnostics <- function(hmc_samples) {

    # R-hat
    hmc.Rhat <- sapply(hmc_samples, posterior::rhat)
    
    # ACF
    acf_lags <- c(0, 1, 5, 10, 20, 50, 100)
    hmc.acf <- lapply(hmc_samples, autocorr, lags = acf_lags, relative = F)

    # ESS
    hmc.ESS <- sapply(hmc_samples, effectiveSize)

    # Inefficiency factor (IF)
    hmc.IF <- sapply(hmc_samples, function(x) {
        if (is.null(x)) {
            return(NA)
        } else {
           return(length(x) / effectiveSize(x))
        }
    })

    return(list(
        Rhat = hmc.Rhat,
        acf = hmc.acf,
        ESS = hmc.ESS,
        IF = hmc.IF
    ))
}

