run_hmc_lgss <- function(data, iters = 10000, burn_in = 5000, n_chains = 1) {
  
  y <- data
  
  stan_file <- "./source/stan_lgss.stan"
  
  lgss_model <- cmdstan_model(
    stan_file,
    cpp_options = list(stan_threads = TRUE)
  )
  
  lgss_data <- list(Tfin = length(y), y = y)
  
  fit_stan_lgss <- lgss_model$sample(
    lgss_data,
    chains = n_chains,
    parallel_chains = n_chains,
    threads = parallel::detectCores(),
    refresh = 100,
    iter_warmup = burn_in,
    iter_sampling = iters,
    save_warmup = TRUE
  )
  
  stan_results <- list(draws = fit_stan_lgss$draws(variables = c("phi", "sigma_eta", "sigma_eps"), inc_warmup = TRUE),
                       time = fit_stan_lgss$time(),
                       summary = fit_stan_lgss$summary(variables = c("phi", "sigma_eta", "sigma_eps")))
  return(stan_results)
}