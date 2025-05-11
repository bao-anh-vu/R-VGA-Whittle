## Generate data for the univariate SV example

gen_sv_data <- function(n, phi, sigma_eta, sigma_eps, kappa) {
    
    # Generate data for the univariate SV example
    # n: number of observations
    # phi: AR coefficient
    # sigma_eta: standard deviation of the latent process
    # sigma_eps: standard deviation of the observation noise
    # kappa: scale parameter

    x <- c()

    x[1] <- rnorm(1, 0, sigma_eta^2 / (1 - phi^2))

    for (t in 2:n) {
        x[t] <- phi * x[t - 1] + sigma_eta * rnorm(1, 0, 1)
    }

    eps <- rnorm(n, 0, sigma_eps)
    y <- kappa * exp(x / 2) * eps

    sv_data <- list(
        x = x, y = y, phi = phi, sigma_eta = sigma_eta,
        sigma_eps = sigma_eps, kappa = kappa
    )

    return(sv_data)
}
