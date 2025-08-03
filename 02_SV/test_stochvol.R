## Stochvol package

setwd("~/R-VGA-Whittle/02_SV/")

rm(list = ls())

library(mvtnorm)
library(stochvol)
library(coda)
library(dplyr)
library(ggplot2)

## Flags
date <- "20240214" # "20230918" #the 20230918 version has sigma_eta = sqrt(0.1)

n <- 2000#0
phi <- 0.7

## Read data
phi_string <- sub("(\\d+)\\.(\\d+)", "\\1\\2", toString(phi)) ## removes decimal point fron the number
print("Reading saved data...")
sv_data <- readRDS(file = paste0("./data/sv_data_n", n, "_phi", phi_string, "_", date, ".rds"))

y <- sv_data$y
x <- sv_data$x
phi <- sv_data$phi
sigma_eta <- sv_data$sigma_eta
sigma_eps <- sv_data$sigma_eps

plot(y, type = "l", main = "Simulated data", ylab = "y", xlab = "t")

## Estimate kappa, then estimate mu based on kappa
kappa_est <- sqrt(exp(mean(log(y^2)) - (digamma(1/2) + log(2))))
mu_fixed <- 2 * log(kappa_est)

## Then plug the fixed value of mu into the model
stv_results <- svsample(y, designmatrix = "ar1",
                    priormu = c(mu_fixed, 1),
                    priorphi = c(20, 1.5),
                    priorsigma = 5)

summary(stv_results)

png(filename = paste0("./plots/stochvol_phi", phi_string, "_n", n, "_", date, ".png"), width = 800, height = 600)
plot(stv_results, showobs = FALSE)
dev.off()

## Plot just the posterior distributions

params <- para(stv_results, chain = "all")[, sampled_parameters(stv_results)]
params_df <- as.data.frame(as.matrix(params))
params_df_long <- pivot_longer(params_df, cols = everything(), names_to = "param", values_to = "val")

## Plot posterior distributions for all parameters
posterior_plot <- ggplot() +
  geom_density(data = params_df_long, aes(x = val)) +
  facet_wrap(~param, scales = "free") +
#   labs(title = "Posterior distribution of", x = , y = "Density") +
  theme_bw()

png(filename = paste0("./plots/stochvol_posterior_phi", phi_string, "_n", n, "_", date, "_params.png"), width = 800, height = 400)
print(posterior_plot)
dev.off()

stv_params <- para(stv_results, chain = "all")[, sampled_parameters(stv_results)]
stv_df <- as.data.frame(as.matrix(stv_params))

stv.df_long <- stv_df %>% mutate(n = row_number()) %>% 
        pivot_longer(
            cols = !n,
            names_to = "param", values_to = "value"
        )

stv.traceplot <- stv.df_long %>% ggplot() + geom_line(aes(x = n, y = value), linewidth = 1) +
        geom_hline(data = true_df, aes(yintercept = value), col = "red", 
                    linetype = "dashed", linewidth = 1.5) +
        facet_wrap(~param, scales = "free", labeller = label_parsed) +
        theme_bw() +
        theme(text = element_text(size = 28)) +
        xlab("Iterations") +
        ylab("Value")

png(paste0("./plots/sv_sim_stv_traceplot_phi", phi_string, ".png"), width = 1500, height = 500)
print(stv.traceplot)
dev.off()
