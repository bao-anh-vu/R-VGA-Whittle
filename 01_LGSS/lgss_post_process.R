# Post-processing results for LGSS model with simulated data
setwd("~/R-VGA-Whittle/01_LGSS/")

rm(list = ls())

library(mvtnorm)
library(coda)
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(latex2exp)

source("./source/compute_periodogram.R")
source("./source/find_cutoff_freq.R")
source("./source/hmc_diagnostics.R")

## Flags
date <- "20230525" # "20230918" #the 20230918 version has sigma_eta = sqrt(0.1)
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
plot_trajectories <- T
save_plots <- T

n <- 10000
phi <- 0.9

## Read data
phi_string <- sub("(\\d+)\\.(\\d+)", "\\1\\2", toString(phi)) ## removes decimal point fron the number
print("Reading saved data...")
lgss_data <- readRDS(file = paste0("./data/lgss_data_n", n, "_phi", phi_string, "_", date, ".rds"))

y <- lgss_data$y
x <- lgss_data$x
phi <- lgss_data$phi
sigma_eps <- lgss_data$sigma_eps
sigma_eta <- lgss_data$sigma_eta

## Read results
print("Reading saved results...")
result_directory <- "./results/"

## R-VGA-Whittle settings

S <- 1000L
# nblocks <- 100
blocksize <- 100
n_indiv <- find_cutoff_freq(y, nsegs = 25, power_prop = 1 / 2)$cutoff_ind # 100

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
    result_directory, "rvga_whittle_results_", transform, "_n", n,
    "_phi", phi_string, temper_info, reorder_info, block_info, "_", date, ".rds"
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
hmc_results <- readRDS(hmc_filepath)
hmcw_results <- readRDS(hmcw_filepath)

param_names <- c("phi", "sigma[eta]", "sigma[epsilon]")
param_values <- c(phi, sigma_eta, sigma_eps)
param_dim <- length(param_names)

## Extract posterior samples
rvgaw_samples <- rvgaw_results$post_samples
hmc_samples <- hmc_results$draws
hmcw_samples <- hmcw_results$draws

## Trace plots
png(paste0("./plots/hmc_lgss_trace_n", n, ".png"), width = 1200, height = 600)
bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_trace(hmc_samples)
dev.off()

png(paste0("./plots/hmcw_lgss_trace_n", n, ".png"), width = 1200, height = 600)
bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_trace(hmcw_samples)
dev.off()

###################################
##        MCMC diagnostics       ##
###################################

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

hmc.Rhat <- hmc_dns$Rhat
hmcw.Rhat <- hmcw_dns$Rhat
hmc.ESS <- hmc_dns$ESS
hmcw.ESS <- hmcw_dns$ESS

methods <- c("HMC-Whittle", "HMC-exact")
metrics <- c("Rhat", "ESS")
dns <- data.frame(
    params = rep(param_names, times = 2),
    metrics = rep(metrics, each = length(param_names) * 2),
    method = rep(methods, each = length(param_names)),
    value = formatC(c(hmcw.Rhat, hmc.Rhat, hmcw.ESS, hmc.ESS),
        format = "f", width = 5
    )
)

write.csv(dns, file = paste0("./results/lgss_diagnostics", block_info, ".csv"), row.names = F)

## Turning the samples into data frames
rvgaw.df <- data.frame(do.call(cbind, rvgaw_samples))
hmc.df <- data.frame(do.call(cbind, hmc_samples_ls))
hmcw.df <- data.frame(do.call(cbind, hmcw_samples_ls))

names(rvgaw.df) <- param_names
names(hmc.df) <- param_names
names(hmcw.df) <- param_names

## Thinning if needed
thin_interval <- 1
inds <- seq(1, nrow(hmc.df), by = thin_interval)
hmc_thin.df <- hmc.df[inds, ]

########################################
##          Posterior plots           ##
########################################

plots <- list()

for (p in 1:param_dim) {
    true_vals.df <- data.frame(name = param_names[p], val = param_values[p])

    plot <- ggplot(rvgaw.df, aes(x = .data[[param_names[p]]])) +
        geom_density(col = "red", lwd = 1) +
        geom_density(data = hmcw.df, col = "goldenrod", lwd = 1) +
        geom_density(data = hmc_thin.df, col = "deepskyblue", lwd = 1) +
        geom_vline(
            data = true_vals.df, aes(xintercept = val),
            color = "black", linetype = "dashed", linewidth = 1
        ) +
        labs(x = vars) +
        theme_bw() +
        theme(axis.title = element_blank(), text = element_text(size = 24)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 4))

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
        stat_ellipse(data = hmcw.df, col = "goldenrod", type = "norm", lwd = 1) +
        stat_ellipse(data = hmc.df, col = "deepskyblue", type = "norm", lwd = 1) +
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

grid.newpage()
grid.draw(gp)

if (save_plots) {
    plot_file <- paste0(
        "lgss_posterior", "_", n, temper_info, reorder_info, block_info,
        "_", transform, "_thinned_", date, ".png"
    )
    filepath <- paste0("./plots/", plot_file)
    png(filepath, width = 800, height = 600)
    grid.draw(gp)
    dev.off()
}


## Timing comparison
rvgaw.time <- rvgaw_results$time_elapsed[3]
hmcw.time <- hmcw_results$time$total
hmc.time <- hmc_results$time$total
print(data.frame(
    method = c("R-VGA-Whittle", "HMC-Whittle", "HMC-exact"),
    time = c(rvgaw.time, hmcw.time, hmc.time)
))

## HMC/HMC-Whittle trace plots
true_df <- data.frame(
        param = param_names,
        value = param_values
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

print(hmc.traceplot)

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

print(hmc_thin.traceplot)

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

print(hmcw.traceplot)

if (save_plots) {
    png("./plots/lgss_hmc_traceplot.png", width = 1500, height = 500)
    print(hmc.traceplot)
    dev.off()

    png("./plots/lgss_hmc_traceplot_thin.png", width = 1500, height = 500)
    print(hmc_thin.traceplot)
    dev.off()

    png("./plots/lgss_hmcw_traceplot.png", width = 1500, height = 500)
    print(hmcw.traceplot)
    dev.off()
}

## R-VGA-Whittle Trajectories/Trace plots
if (plot_trajectories) {
    # mu_theta_phi <- sapply(rvgaw_results$mu, function(x) x[1])
    # mu_theta_sigma_eta <- sapply(rvgaw_results$mu, function(x) x[2])
    # mu_theta_sigma_eps <- sapply(rvgaw_results$mu, function(x) x[3])

    # var_theta_phi <- sapply(rvgaw_results$prec, function(x) 1/x[1,1])
    # var_theta_sigma <- sapply(rvgaw_results$prec, function(x) 1/x[2,2])

    mu_theta <- rvgaw_results$mu
    var_theta <- lapply(rvgaw_results$prec, function(Q) {
        chol_Q <- chol(Q)
        Q_inv <- chol2inv(chol_Q)
    })

    # phi_sample_ls <- lapply(1:length(mu_theta_phi), function(i) {
    #     tanh(rnorm(10000, mu_theta_phi[i], sqrt(var_theta_phi[i])))
    # })

    # sigma_eta_sample_ls <- lapply(1:length(mu_theta_sigma), function(i) {
    #     sqrt(exp(rnorm(10000, mu_theta_sigma[i], sqrt(var_theta_sigma[i]))))
    # })

    theta_sample_ls <- lapply(1:length(mu_theta), function(i) {
        rmvnorm(10000, mu_theta[[i]], var_theta[[i]])
    })

    if (transform == "arctanh") {
        mu_phi <- sapply(theta_sample_ls, function(x) mean(tanh(x[, 1])))
    } else { # logit transform
        mu_phi <- sapply(theta_sample_ls, function(x) mean(exp(x[, 1]) / (1 + exp(x[, 1]))))
    }

    mu_sigma_eta <- sapply(theta_sample_ls, function(x) mean(sqrt(exp(x[, 2]))))
    mu_sigma_eps <- sapply(theta_sample_ls, function(x) mean(sqrt(exp(x[, 3]))))

    # if (transform == "arctanh") {
    #     mu_phi <- tanh(mu_phi)
    # } else { # logit transform
    #     mu_phi <- exp(mu_phi) / (1 + exp(mu_phi))
    # }
    # mu_sigma_eta <- sqrt(exp(mu_sigma_eta))
    # mu_sigma_eps <- sqrt(exp(mu_sigma_eps))

    block_df <- data.frame(cutoff = n_indiv)

    trajectory_df <- data.frame(mu_phi, mu_sigma_eta, mu_sigma_eps)
    names(trajectory_df) <- param_names
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
        theme(text = element_text(size = 28)) +
        xlab(TeX("Iteration ($\\tilde{k}$)")) +
        ylab("Value")

    png(paste0("plots/trajectories_lgss", block_info, ".png"), width = 1500, height = 400)
    print(trajectory_plot)

    dev.off()
}
