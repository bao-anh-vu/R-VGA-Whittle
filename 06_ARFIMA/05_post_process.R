## Post-processing script for the ARFIMA model

## SSM with underlying ARFIMA model
setwd("~/R-VGA-Whittle/06_ARFIMA")

rm(list = ls())

# Load libraries
library(mvtnorm)
library(dplyr)
library(tidyr)
# library(arfima)
# library(fracdiff)
# library(tensorflow)
# reticulate::use_condaenv("myenv", required = TRUE)
# library(keras)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(coda)

## Load custom functions
source("./source/compute_periodogram.R")
source("./source/compute_arfima_spec_dens.R")
source("./source/find_cutoff_freq.R")
source("./source/hmc_diagnostics.R")

## Flags
date <- "20250627" #"20250514"
noise_dist <- "t" # "t" or "gaussian"
save_plots <- T

## HMC-Whittle settings
n_chains <- 2
burn_in <- 1000 # number of burn-in iterations
hmcw_iters <- 10000 # number of HMC iterations per chain

## Directories
data_dir <- "./data/"
result_dir <- "./results/"

## Read data
n <- 50000
arfima_data <- readRDS(paste0(data_dir, "arfima_data_n", n, "_", noise_dist, ".rds"))
y <- arfima_data$y
phi <- arfima_data$phi
theta <- arfima_data$theta
d <- arfima_data$d
sigma_eta <- arfima_data$sigma_eta
nu <- arfima_data$nu

########################
##    Read results    ##
########################

### R-VGA-Whittle results
S <- 1000L
use_tempering <- TRUE
temper_first <- T
reorder <- 0 #"decreasing"
blocksize <- 100L
# n_indiv <- 20L
n_indiv <- find_cutoff_freq(y, nsegs = 25, power_prop = 1/2)$cutoff_ind #100
n_post_samples <- 10000

if (use_tempering) {
  n_temper <- 100
  K <- 100
  temper_schedule <- rep(1/K, K)
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
  block_info <- paste0("_", "blocksize", blocksize, "_", n_indiv, "indiv")
} else {
  block_info <- ""
}

rvgaw_filepath <- paste0(result_dir, "rvga_whittle_results_n", n, "_", noise_dist,
                        temper_info, reorder_info, block_info, "_", date, ".rds")
hmcw_filepath <- paste0(result_dir, "hmcw_arfima_ss_results_n", n, "_" , noise_dist,
                       "_", date, "_1.rds")

rvgaw_results <- readRDS(rvgaw_filepath)
hmcw_results <- readRDS(hmcw_filepath)

## Extract posterior samples
rvgaw_samples_ls <- rvgaw_results$post_samples
hmcw_samples <- hmcw_results$draws[1:(burn_in + hmcw_iters),,]

# rvgaw.phi <- rvgaw_results$post_samples$phi
# rvgaw.theta <- rvgaw_results$post_samples$theta
# rvgaw.d <- rvgaw_results$post_samples$d
# rvgaw.sigma_eta <- rvgaw_results$post_samples$sigma_eta
# rvgaw.nu <- rvgaw_results$post_samples$nu

# rvgaw.df <- data.frame(
#     phi = rvgaw.phi,
#     theta = rvgaw.theta,
#     d = rvgaw.d,
#     sigma_eta = rvgaw.sigma_eta,
#     nu = rvgaw.nu
# )

### HMC-Whittle results
# ndraws <- 11000 # per chain
# hmcw.phi <- hmcw_results$draws[1:ndraws,,1]
# hmcw.theta <- hmcw_results$draws[1:ndraws,,2]
# hmcw.d <- hmcw_results$draws[1:ndraws,,3]
# hmcw.sigma_eta <- hmcw_results$draws[1:ndraws,,4]
# hmcw.nu <- hmcw_results$draws[1:ndraws,,5]

# burn_in <- 1000 #hmcw_results$metadata$iter_warmup

# hmcw.phi <- c(hmcw.phi[-(1:burn_in),,])
# hmcw.theta <- c(hmcw.theta[-(1:burn_in),,])
# hmcw.d <- c(hmcw.d[-(1:burn_in),,])
# hmcw.sigma_eta <- c(hmcw.sigma_eta[-(1:burn_in),,])
# hmcw.nu <- c(hmcw.nu[-(1:burn_in),,])

### HMC results
# hmc_filepath <- paste0(result_dir, "hmc_results_n", n, "_", noise_dist,
#                        "_", date, ".rds")
# hmc_results <- readRDS(hmc_filepath)

# hmc.phi <- c(hmc_results$draws[,,1])
# hmc.theta <- c(hmc_results$draws[,,2])
# hmc.d <- c(hmc_results$draws[,,3])
# hmc.sigma_eta <- c(hmc_results$draws[,,4])
# hmc.nu <- c(hmc_results$draws[,,5])

# hmc_df <- data.frame(
#     phi = hmc.phi,
#     theta = hmc.theta,
#     d = hmc.d,
#     sigma_eta = hmc.sigma_eta,
#     nu = hmc.nu
# )

## Trace plots
png(paste0("./plots/hmcw_arfima_ss_trace_n", n, ".png"), width = 1200, height = 600)
bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_trace(hmcw_results$draws)
dev.off()

###############################
##  Convergence diagnostics  ##
###############################

param_names <- c("phi", "theta", "d", "sigma_eta", "nu")
param_values <- c(phi, theta, d, sigma_eta, nu)
param_dim <- length(param_names)

## Discard burn-in samples
hmcw_samples <- hmcw_samples[-(1:burn_in), , ]

hmcw_samples_ls <- lapply(1:param_dim, function(i) {
    c(hmcw_samples[, , i])
})
hmcw_dns <- hmc_diagnostics(hmcw_samples_ls)

# hmcw_samples_ls <- list(
#     c(hmcw.phi),
#     c(hmcw.theta),
#     c(hmcw.d),
#     c(hmcw.sigma_eta),
#     c(hmcw.nu)
# )
# hmcw_dns <- hmc_diagnostics(hmcw_samples_ls)

# hmc_samples_ls <- list(
#     hmc.phi,
#     hmc.theta,
#     hmc.d,
#     hmc.sigma_eta,
#     hmc.nu
# )
# hmc_dns <- hmc_diagnostics(hmc_samples_ls)

## Turn the samples into data frames
rvgaw.df <- data.frame(do.call(cbind, rvgaw_samples_ls))
hmcw.df <- data.frame(do.call(cbind, hmcw_samples_ls))

names(rvgaw.df) <- param_names
names(hmcw.df) <- param_names

## Thinning if needed
thin_interval <- 10
inds <- seq(1, nrow(hmcw.df), by = thin_interval)
hmcw.df_thin <- hmcw.df[inds, ] # thin the HMC samples
# hmc_df_thin <- hmc_df[1:nrow(hmc_df)/2, ] # get rid of the second chain for now
# hmc_df_thin <- hmc_df[seq(1, nrow(hmc_df), by = 100), ] # thin the HMC samples

###########################
##    Posterior plots    ##
###########################

plots <- list()

xlims <- list(
    c(0.22, 0.4), # phi
    c(0.1, 1), # theta
    c(0.22, 0.29),  # d
    c(0.8, 1.5),  # sigma_eta
    c(3.8, 4.5)  # nu
)

for (p in 1:length(param_names)) {
    true_vals_df <- data.frame(name = param_names[p], val = param_values[p])

    plot <- ggplot(rvgaw.df, aes(x = .data[[param_names[p]]])) +
        geom_density(col = "red", lwd = 1) +
        geom_density(data = hmcw.df_thin, col = "goldenrod", lwd = 1) +
        # geom_density(data = hmc_df_thin, col = "deepskyblue", lwd = 1) +
        geom_vline(
            data = true_vals_df, aes(xintercept = val),
            color = "black", linetype = "dashed", linewidth = 1
        ) +
        xlim(xlims[[p]]) +
        # labs(x = vars) +
        theme_bw() +
        theme(axis.title = element_blank(), text = element_text(size = 22)) #+
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
        stat_ellipse(data = hmcw.df_thin, col = "goldenrod", type = "norm", lwd = 1) +
        # stat_ellipse(data = hmc_df_thin, col = "deepskyblue", type = "norm", lwd = 1) +
        geom_point(
            data = param_df, aes(x = x, y = y),
            shape = 4, color = "black", size = 5
        ) +
        theme_bw() +
        theme(axis.title = element_blank(), text = element_text(size = 22)) + # Assign pretty axis ticks
        scale_x_continuous(breaks = scales::pretty_breaks(n = 3))

    cov_plots[[ind]] <- cov_plot
}

m <- matrix(NA, param_dim, param_dim)
m[lower.tri(m, diag = F)] <- 1:n_lower_tri
gr <- grid.arrange(grobs = cov_plots, layout_matrix = m)
gr2 <- gtable_add_cols(gr, unit(1, "null"), -1)
gr3 <- gtable_add_grob(gr2, grobs = lapply(plots, ggplotGrob), t = 1:param_dim, l = 1:param_dim)

# grid.draw(gr3)

# A list of text grobs - the labels
vars <- list(textGrob(bquote(phi)), textGrob(bquote(theta)),
             textGrob(bquote(d)), textGrob(bquote(sigma[eta])), textGrob(bquote(nu)))
vars <- lapply(vars, editGrob, gp = gpar(col = "black", fontsize = 24))

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
        "arfima_ss_posterior_n",  n, temper_info, reorder_info, block_info,
        "_", noise_dist, "_", date, ".png"
    )
    filepath <- paste0("./plots/", plot_file)
    png(filepath, width = 1000, height = 750)
    grid.draw(gp)
    dev.off()
}

## Timing comparison
rvgaw.time <- rvgaw_results$time_elapsed[3]
hmcw.time <- hmcw_results$time$total / dim(hmcw_results$draws)[1] * (burn_in + hmcw_iters)
print(data.frame(method = c("R-VGA-Whittle", "HMC-Whittle"),
                 time = c(rvgaw.time, hmcw.time)))


#######################
##    Trace plots    ##
#######################
param_labels <- c("phi", "theta", "d", "sigma[eta]", "nu")
names(hmcw.df) <- param_labels
names(hmcw.df_thin) <- param_labels

hmcw.df_long <- hmcw.df %>% mutate(n = row_number()) %>% 
  pivot_longer(
      cols = !n,
      names_to = "param", values_to = "value"
  )

hmcw.df_thin_long <- hmcw.df_thin %>% mutate(n = row_number()) %>% 
  pivot_longer(
      cols = !n,
      names_to = "param", values_to = "value"
  )

true_df <- data.frame(
        param = param_labels,
        value = c(phi, theta, d, sigma_eta, nu)
    )  

hmcw_traceplot <- hmcw.df_long %>% ggplot() + 
        geom_line(aes(x = n, y = value), linewidth = 1) +
        geom_hline(data = true_df, aes(yintercept = value), col = "red", 
                    linetype = "dashed", linewidth = 1.5) +
        facet_wrap(~param, scales = "free", labeller = label_parsed) +
        theme_bw() +
        theme(text = element_text(size = 25)) +
        xlab("Iterations") +
        ylab("Value")

hmcw_thin_traceplot <- hmcw.df_thin_long %>% ggplot() + 
        geom_line(aes(x = n, y = value), linewidth = 1) +
        geom_hline(data = true_df, aes(yintercept = value), col = "red", 
                    linetype = "dashed", linewidth = 1.5) +
        facet_wrap(~param, scales = "free", labeller = label_parsed) +
        theme_bw() +
        theme(text = element_text(size = 25)) +
        xlab("Iterations") +
        ylab("Value")
  

png(paste0("./plots/hmcw_arfima_ss_trace_n", n, ".png"), width = 1200, height = 600)
print(hmcw_traceplot)
dev.off()

png(paste0("./plots/hmcw_arfima_ss_trace_n", n, "_thinned.png"), width = 1200, height = 600)
print(hmcw_thin_traceplot)
dev.off()
