# Post-processing results for SV model with simulated data
setwd("~/R-VGA-Whittle/02_SV/")

rm(list = ls())

library(mvtnorm)
library(coda)
# library(Deriv)
# library(cmdstanr)
# library(tensorflow)
# reticulate::use_condaenv("myenv", required = TRUE)
# library(keras)
# library(stats)
# library(bspec)
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(latex2exp)
library(stochvol)

source("./source/compute_periodogram.R")
source("./source/find_cutoff_freq.R")
source("./source/hmc_diagnostics.R")

## Flags
date <- "20240214" # "20230918" #the 20230918 version has sigma_eta = sqrt(0.1)
# date <- "20230918"

## R-VGA flags
use_tempering <- T
temper_first <- T
reorder <- 0 # "decreasing" # or decreasing # or a number
reorder_seed <- 2024
plot_prior <- F
plot_likelihood_surface <- F
prior_type <- ""
transform <- "arctanh"
plot_trajectories <- F
save_plots <- F


n <- 2000 # 0
phi <- 0.99

## Read data
phi_string <- sub("(\\d+)\\.(\\d+)", "\\1\\2", toString(phi)) ## removes decimal point fron the number
print("Reading saved data...")
sv_data <- readRDS(file = paste0("./data/sv_data_n", n, "_phi", phi_string, "_", date, ".rds"))

y <- sv_data$y
x <- sv_data$x
phi <- sv_data$phi
sigma_eta <- sv_data$sigma_eta
sigma_eps <- sv_data$sigma_eps

## Read results
print("Reading saved results...")
result_directory <- paste0("./results/", transform, "/")

S <- 1000L
# nblocks <- 100
blocksize <- 100
n_indiv <- find_cutoff_freq(y, nsegs = 5, power_prop = 1 / 2)$cutoff_ind # 100

if (use_tempering) {
    n_temper <- 5
    K <- 100
    temper_schedule <- rep(1 / K, K)
    temper_info <- ""
    if (temper_first) {
        temper_info <- paste0("_temperfirst", n_temper)
    } else {
        temper_info <- paste0("_temperlast", n_temper)
    }
} else {
    temper_info <- ""
}

if (reorder == "random") {
    reorder_info <- paste0("_", reorder, reorder_seed)
} else if (reorder == "decreasing") {
    reorder_info <- paste0("_", reorder)
} else if (reorder > 0) {
    reorder_info <- paste0("_reorder", reorder)
} else {
    reorder_info <- ""
}

# if (!is.null(nblocks)) {
if (!is.null(blocksize)) {
    # block_info <- paste0("_", nblocks, "blocks", n_indiv, "indiv")
    block_info <- paste0("_", "blocksize", blocksize, "_", n_indiv, "indiv")
} else {
    block_info <- ""
}

rvgaw_filepath <- paste0(
    result_directory, "rvga_whittle_results_n", n,
    "_phi", phi_string, temper_info, reorder_info, block_info,
    prior_type, "_", date, ".rds"
)

stv_filepath <- paste0(
    result_directory, "stv_results_n", n,
    "_phi", phi_string, "_", date, ".rds"
)

hmc_filepath <- paste0(
    result_directory, "hmc_results_n", n,
    "_phi", phi_string, "_", date, ".rds"
)
hmcw_filepath <- paste0(
    result_directory, "hmcw_results_n", n,
    "_phi", phi_string, "_", date, ".rds"
)

rvgaw_results <- readRDS(rvgaw_filepath)
stv_results <- readRDS(stv_filepath)
hmc_results <- readRDS(hmc_filepath)
hmcw_results <- readRDS(hmcw_filepath)

## Extract posterior samples
rvgaw_samples <- rvgaw_results$post_samples
hmc_samples <- hmc_results$draws
hmcw_samples <- hmcw_results$draws

stv_params <- para(stv_results, chain = "all")[, sampled_parameters(stv_results)]
stv.df <- as.data.frame(as.matrix(stv_params))
stv.df <- stv.df %>% select(phi, sigma) # sigma is the same as sigma_eta
# params_df_long <- pivot_longer(params_df, cols = everything(), names_to = "param", values_to = "val")

## Trace plots
png(paste0("./plots/hmc_sv_trace_n", n, ".png"), width = 1200, height = 600)
bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_trace(hmc_samples)
dev.off()

png(paste0("./plots/hmcw_sv_trace_n", n, ".png"), width = 1200, height = 600)
bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_trace(hmcw_samples)
dev.off()

# png(filename = paste0("./plots/stochvol_phi", phi_string, "_n", n, "_", date, ".png"), width = 800, height = 600)
# plot(stv_results, showobs = FALSE)
# dev.off()

########################################
##          MCMC diagnostics          ##
########################################

param_names <- c("phi", "sigma[eta]")
param_values <- c(phi, sigma_eta)
param_dim <- length(param_names)

burn_in <- 1000
hmc_samples <- hmc_samples[-(1:burn_in), , ]
hmcw_samples <- hmcw_samples[-(1:burn_in), , ]

## HMC
hmc_samples_ls <- lapply(1:param_dim, function(i) {
    c(hmc_samples[, , i])
})
hmc_dns <- hmc_diagnostics(hmc_samples_ls)

## HMCW
hmcw_samples_ls <- lapply(1:param_dim, function(i) {
    c(hmcw_samples[, , i])
})
hmcw_dns <- hmc_diagnostics(hmcw_samples_ls)

## Stochvol
stv_samples_ls <- list(
    stv.df$phi,
    stv.df$sigma
)
stv_dns <- hmc_diagnostics(stv_samples_ls)

hmc.Rhat <- round(hmc_dns$Rhat, 4)
hmcw.Rhat <- round(hmcw_dns$Rhat, 4)
hmc.ESS <- round(hmc_dns$ESS, 2)
hmcw.ESS <- round(hmcw_dns$ESS, 2)

methods <- c("HMC-Whittle", "HMC-exact")
metrics <- c("Rhat", "ESS")
dns <- data.frame(
    params = rep(param_names, times = 2),
    metrics = rep(metrics, each = 4),
    method = rep(methods, each = 2),
    value = c(hmcw.Rhat, hmc.Rhat, hmcw.ESS, hmc.ESS)
)

write.csv(dns, file = paste0("./results/sv_sim_diagnostics", block_info, ".csv"), row.names = F)


## Turning the samples into data frames
rvgaw.df <- data.frame(do.call(cbind, rvgaw_samples))
hmc.df <- data.frame(do.call(cbind, hmc_samples_ls))
hmcw.df <- data.frame(do.call(cbind, hmcw_samples_ls))

names(rvgaw.df) <- param_names
names(hmc.df) <- param_names
names(hmcw.df) <- param_names
names(stv.df) <- param_names

## Thinning if needed
thin_interval <- 1
inds <- seq(1, nrow(hmc.df), by = thin_interval)
hmc_thin.df <- hmc.df[inds, ]

##############################
##      Posterior plots     ##
##############################

plots <- list()

xlims <- list(
    c(phi - 0.015, phi + 0.01),
    c(sigma_eta - 0.1, sigma_eta + 0.15)
)
for (p in 1:param_dim) {
    true_vals.df <- data.frame(name = param_names[p], val = param_values[p])

    plot <- ggplot(rvgaw.df, aes(x = .data[[param_names[p]]])) +
        geom_density(col = "red", lwd = 1) +
        geom_density(data = stv.df, col = "black", lwd = 1) +
        geom_density(data = hmcw.df, col = "goldenrod", lwd = 1) +
        geom_density(data = hmc_thin.df, col = "deepskyblue", lwd = 1) +
        geom_vline(
            data = true_vals.df, aes(xintercept = val),
            color = "black", linetype = "dashed", linewidth = 1
        ) +
        xlim(xlims[[p]]) +
        labs(x = vars) +
        theme_bw() +
        theme(axis.title = element_blank(), text = element_text(size = 24)) #+
    # scale_x_continuous(breaks = scales::pretty_breaks(n = 4))

    plots[[p]] <- plot
}

## Arrange bivariate plots in lower off-diagonals
n_lower_tri <- (param_dim^2 - param_dim) / 2 # number of lower triangular elements

index_to_i_j_colwise_nodiag <- function(k, n) {
    kp <- n * (n - 1) / 2 - k
    p <- floor((sqrt(1 + 8 * kp) - 1) / 2)
    i <- n - (kp - p * (p + 1) / 2)
    j <- n - 1 - p
    c(i, j)
}

cov_plots <- list()
for (ind in 1:n_lower_tri) {
    mat_ind <- index_to_i_j_colwise_nodiag(ind, param_dim)
    p <- mat_ind[1]
    q <- mat_ind[2]

    param_df <- data.frame(x = param_values[q], y = param_values[p])

    cov_plot <- ggplot(rvgaw.df, aes(x = .data[[param_names[q]]], y = .data[[param_names[p]]])) +
        stat_ellipse(col = "red", type = "norm", lwd = 1) +
        stat_ellipse(data = stv.df, col = "black", type = "norm", lwd = 1) +
        stat_ellipse(data = hmcw.df, col = "goldenrod", type = "norm", lwd = 1) +
        stat_ellipse(data = hmc_thin.df, col = "deepskyblue", type = "norm", lwd = 1) +
        geom_point(
            data = param_df, aes(x = x, y = y),
            shape = 4, color = "black", size = 5
        ) +
        theme_bw() +
        theme(axis.title = element_blank(), text = element_text(size = 24)) + # Assign pretty axis ticks
        scale_x_continuous(breaks = scales::pretty_breaks(n = 3))

    cov_plots[[ind]] <- cov_plot
}

m <- matrix(NA, param_dim, param_dim)
m[lower.tri(m, diag = F)] <- 1:n_lower_tri
gr <- grid.arrange(grobs = cov_plots, layout_matrix = m)
gr2 <- gtable_add_cols(gr, unit(1, "null"), -1)
gr3 <- gtable_add_grob(gr2, grobs = lapply(plots, ggplotGrob), t = 1:param_dim, l = 1:param_dim)

grid.draw(gr3)

# A list of text grobs - the labels
vars <- list(textGrob(bquote(phi)), textGrob(bquote(sigma[eta])), textGrob(bquote(sigma[epsilon])))
vars <- lapply(vars, editGrob, gp = gpar(col = "black", fontsize = 24))

# m <- matrix(1:param_dim, 1, param_dim, byrow = T)
# gr <- grid.arrange(grobs = plots, layout_matrix = m)
# gp <- gtable_add_rows(gr, unit(1.5, "lines"), -1) #0 adds on the top
# gtable_show_layout(gp)
#
# gp <- gtable_add_grob(gp, vars[1:param_dim], t = 2, l = 1:3)

# So that there is space for the labels,
# add a row to the top of the gtable,
# and a column to the left of the gtable.
gp <- gtable_add_cols(gr3, unit(1.5, "lines"), 0)
gp <- gtable_add_rows(gp, unit(1.5, "lines"), -1) # 0 adds on the top

gtable_show_layout(gp)

# Add the label grobs.
# The labels on the left should be rotated; hence the edit.
# t and l refer to cells in the gtable layout.
# gtable_show_layout(gp) shows the layout.
gp <- gtable_add_grob(gp, lapply(vars[1:param_dim], editGrob, rot = 90), t = 1:param_dim, l = 1)
gp <- gtable_add_grob(gp, vars[1:param_dim], t = param_dim + 1, l = 2:(param_dim + 1))

# grid.newpage()
# grid.draw(gp)

if (save_plots) {
    plot_file <- paste0(
        "sv_sim_posterior", "_phi", phi, "_n", n, temper_info, reorder_info, block_info,
        prior_type, "_", transform, "_thinned_", date, ".png"
    )
    filepath <- paste0("./plots/", plot_file)
    png(filepath, width = 800, height = 600)
    grid.draw(gp)
    dev.off()
}


## Timing comparison
rvgaw.time <- rvgaw_results$time_elapsed[3]
stv.time <- stv_results$runtime[3]
hmcw.time <- hmcw_results$time()$total
hmc.time <- hmc_results$time$total
print(data.frame(
    method = c("R-VGA-Whittle", "stochvol", "HMC-Whittle", "HMC-exact"),
    time = c(rvgaw.time, stv.time, hmcw.time, hmc.time)
))

## HMC/HMC-Whittle trace plots
true_df <- data.frame(
        param = c("phi", "sigma[eta]"),
        value = c(phi, sigma_eta)
    )
    
hmc.df_long <- hmc.df %>%
    mutate(n = row_number()) %>%
    pivot_longer(
        cols = !n,
        names_to = "param", values_to = "value"
    )

hmc_thin.df_long <- hmc_thin.df %>%
    mutate(n = row_number()) %>%
    pivot_longer(
        cols = !n,
        names_to = "param", values_to = "value"
    )

hmcw.df_long <- hmcw.df %>%
    mutate(n = row_number()) %>%
    pivot_longer(
        cols = !n,
        names_to = "param", values_to = "value"
    )

stv.df_long <- stv.df %>%
    mutate(n = row_number()) %>%
    pivot_longer(
        cols = !n,
        names_to = "param", values_to = "value"
    )

## Traceplots
hmc.traceplot <- hmc.df_long %>% ggplot() +
    geom_line(aes(x = n, y = value), linewidth = 1) +
    geom_hline(
        data = true_df, aes(yintercept = value), col = "red",
        linetype = "dashed", linewidth = 1.5
    ) +
    facet_wrap(~param, scales = "free", labeller = label_parsed) +
    theme_bw() +
    theme(text = element_text(size = 28)) +
    xlab("Iterations") +
    ylab("Value")

# print(hmc.traceplot)

hmc_thin.traceplot <- hmc_thin.df_long %>% ggplot() +
    geom_line(aes(x = n, y = value), linewidth = 1) +
    geom_hline(
        data = true_df, aes(yintercept = value), col = "red",
        linetype = "dashed", linewidth = 1.5
    ) +
    facet_wrap(~param, scales = "free", labeller = label_parsed) +
    theme_bw() +
    theme(text = element_text(size = 28)) +
    xlab("Iterations") +
    ylab("Value")

# print(hmc_thin.traceplot)

hmcw.traceplot <- hmcw.df_long %>% ggplot() +
    geom_line(aes(x = n, y = value), linewidth = 1) +
    geom_hline(
        data = true_df, aes(yintercept = value), col = "red",
        linetype = "dashed", linewidth = 1.5
    ) +
    facet_wrap(~param, scales = "free", labeller = label_parsed) +
    theme_bw() +
    theme(text = element_text(size = 28)) +
    xlab("Iterations") +
    ylab("Value")

stv.traceplot <- stv.df_long %>% ggplot() +
    geom_line(aes(x = n, y = value), linewidth = 1) +
    geom_hline(
        data = true_df, aes(yintercept = value), col = "red",
        linetype = "dashed", linewidth = 1.5
    ) +
    facet_wrap(~param, scales = "free", labeller = label_parsed) +
    theme_bw() +
    theme(text = element_text(size = 28)) +
    xlab("Iterations") +
    ylab("Value")

# print(hmcw.traceplot)

if (save_plots) {
    png("./plots/sv_sim_hmc_traceplot.png", width = 1500, height = 500)
    print(hmc.traceplot)
    dev.off()

    png("./plots/sv_sim_hmc_traceplot_thin.png", width = 1500, height = 500)
    print(hmc_thin.traceplot)
    dev.off()

    png("./plots/sv_sim_hmcw_traceplot.png", width = 1500, height = 500)
    print(hmcw.traceplot)
    dev.off()

    png("./plots/sv_sim_stv_traceplot.png", width = 1500, height = 500)
    print(stv.traceplot)
    dev.off()
}

## Trajectories/Trace plots
if (plot_trajectories) {
    mu_theta <- rvgaw_results$mu
    var_theta <- lapply(rvgaw_results$prec, function(Q) {
        chol_Q <- chol(Q)
        Q_inv <- chol2inv(chol_Q)
    })

    theta_sample_ls <- lapply(1:length(mu_theta), function(i) {
        rmvnorm(10000, mu_theta[[i]], var_theta[[i]])
    })

    if (transform == "arctanh") {
        mu_phi <- sapply(theta_sample_ls, function(x) mean(tanh(x[, 1])))
    } else { # logit transform
        mu_phi <- sapply(theta_sample_ls, function(x) mean(exp(x[, 1]) / (1 + exp(x[, 1]))))
    }

    mu_sigma_eta <- sapply(theta_sample_ls, function(x) mean(sqrt(exp(x[, 2]))))

    # mu_theta_phi <- sapply(rvgaw_results$mu, function(x) x[1])
    # mu_theta_sigma <- sapply(rvgaw_results$mu, function(x) x[2])

    # var_theta_phi <- sapply(rvgaw_results$prec, function(x) 1/x[1,1])
    # var_theta_sigma <- sapply(rvgaw_results$prec, function(x) 1/x[2,2])

    # phi_sample_ls <- lapply(1:length(mu_theta_phi), function(i) {
    #     tanh(rnorm(10000, mu_theta_phi[i], sqrt(var_theta_phi[i])))
    # })

    # sigma_eta_sample_ls <- lapply(1:length(mu_theta_sigma), function(i) {
    #     sqrt(exp(rnorm(10000, mu_theta_sigma[i], sqrt(var_theta_sigma[i]))))
    # })

    # mu_phi <- sapply(phi_sample_ls, mean)
    # mu_sigma_eta <- sapply(sigma_eta_sample_ls, mean)

    ####
    # mu_phi <- sapply(rvgaw_results$mu, function(x) x[1])
    # mu_sigma_eta <- sapply(rvgaw_results$mu, function(x) x[2])

    # if (transform == "arctanh") {
    #     mu_phi <- tanh(mu_phi)
    # } else { # logit transform
    #     mu_phi <- exp(mu_phi) / (1 + exp(mu_phi))
    # }
    # mu_sigma_eta <- sqrt(exp(mu_sigma_eta))

    block_df <- data.frame(cutoff = n_indiv)

    trajectory_df <- data.frame(phi = mu_phi, sigma_eta = mu_sigma_eta)
    names(trajectory_df) <- c("phi", "sigma[eta]")
    trajectory_df$iter <- 1:nrow(trajectory_df)

    trajectory_df_long <- trajectory_df %>% pivot_longer(
        cols = !iter,
        names_to = "param", values_to = "value"
    )
    trajectory_plot <- trajectory_df_long %>% ggplot() +
        geom_line(aes(x = iter, y = value), linewidth = 1) +
        facet_wrap(~param, scales = "free", labeller = label_parsed) +
        geom_hline(data = true_df, aes(yintercept = value), linetype = "dashed", linewidth = 1.5) +
        geom_vline(data = block_df, aes(xintercept = cutoff), linetype = "dotted", linewidth = 1.5) +
        theme_bw() +
        theme(text = element_text(size = 34)) +
        xlab(TeX("Iteration ($\\tilde{k}$)")) +
        ylab("Value")

    png(paste0("./plots/trajectories_sv_sim", block_info, ".png"), width = 1200, height = 500)
    print(trajectory_plot)

    dev.off()
}

## Convergence statistics
# names(hmc.df) <- c("phi", "sigma_eta")
# names(hmcw.df) <- c("phi", "sigma_eta")
# converge_df <- data.frame(
#     method = c("stochvol", "HMC", "HMCW"),
#     rhat_phi = c(posterior::rhat(stv.df$phi), posterior::rhat(hmc.df$phi), posterior::rhat(hmcw.df$phi)),
#     rhat_sigma_eta = c(posterior::rhat(stv.df$sigma), posterior::rhat(hmc.df$sigma_eta), posterior::rhat(hmcw.df$sigma_eta)),
#     ess_bulk_phi = c(posterior::ess_bulk(stv.df$phi), posterior::ess_bulk(hmc.df$phi), posterior::ess_bulk(hmcw.df$phi)),
#     ess_bulk_sigma_eta = c(posterior::ess_bulk(stv.df$sigma), posterior::ess_bulk(hmc.df$sigma_eta), posterior::ess_bulk(hmcw.df$sigma_eta)),
#     ess_tail_phi = c(posterior::ess_tail(stv.df$phi), posterior::ess_tail(hmc.df$phi), posterior::ess_tail(hmcw.df$phi)),
#     ess_tail_sigma_eta = c(posterior::ess_tail(stv.df$sigma), posterior::ess_tail(hmc.df$sigma_eta), posterior::ess_tail(hmcw.df$sigma_eta))
# )
# print(converge_df)

