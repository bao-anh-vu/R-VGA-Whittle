# Post-processing results for SV model with simulated data
setwd("~/R-VGA-Whittle/04_Multi_SV_Sigma/")

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

source("./source/compute_periodogram.R")
source("./source/compute_periodogram_uni.R")
source("./source/find_cutoff_freq.R")
source("./source/construct_Sigma.R")
source("./source/hmc_diagnostics.R")

## Flags
# plot_trajectories <- T
save_plots <- T

date <- "20240613" #"20240227" # "20230918" #the 20230918 version has sigma_eta = sqrt(0.1)
use_cholesky <- T # use lower Cholesky factor to parameterise Sigma_eta
transform <- "arctanh"
prior_type <- "prior1"
use_heaps_mapping <- F
plot_likelihood_surface <- F
plot_prior_samples <- F
plot_trace <- T

## R-VGAW flags
use_tempering <- T #T
temper_first <- T
reorder <- 0 #"decreasing"
reorder_seed <- 2024
# decreasing <- T
use_median <- F

## HMC/HMC-Whittle settings
burn_in <- 1000
hmcw_iters <- 2000 # per chain

## Read data
d <- 2 # bivariate
Tfin <- 5000
print("Reading saved data...")
multi_sv_data <- readRDS(file = paste0("./data/multi_sv_data_", d, "d_Tfin", Tfin, "_20240227.rds"))

X <- multi_sv_data$X
Y <- multi_sv_data$Y
Phi <- multi_sv_data$Phi
Sigma_eta <- multi_sv_data$Sigma_eta
Sigma_eps <- multi_sv_data$Sigma_eps

## Read results
print("Reading saved results...")
result_directory <- paste0("./results/", d, "d/")

S <- 1000L
# nblocks <- 100
blocksize <- 100
nsegs <- 25
power_prop <- 1/2

c1 <- find_cutoff_freq(Y[, 1], nsegs = nsegs, power_prop = power_prop)$cutoff_ind
c2 <- find_cutoff_freq(Y[, 2], nsegs = nsegs, power_prop = power_prop)$cutoff_ind
n_indiv <- max(c1, c2)

if (use_tempering) {
  n_temper <- 5
  K <- 100
  temper_schedule <- rep(1/K, K)
  temper_info <- paste0("_temper", n_temper)
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

if (prior_type == "minnesota") {
  prior_type <- ""
} else {
  prior_type <- paste0("_", prior_type)
}

rvgaw_filepath <- paste0(result_directory, "rvga_whittle_results_Tfin", Tfin, 
                         temper_info, reorder_info, block_info, "_", date, 
                         prior_type, ".rds")

hmc_filepath <- paste0(result_directory, "hmc_results_Tfin", Tfin, 
                       "_", date, prior_type, ".rds")

hmcw_filepath <- paste0(result_directory, "hmcw_results_Tfin", Tfin, 
                       "_", date, prior_type, ".rds")

rvgaw_results <- readRDS(rvgaw_filepath)
hmc_results <- readRDS(hmc_filepath)
hmcw_results <- readRDS(hmcw_filepath)

rvgaw_samples <- rvgaw_results$post_samples
hmc_samples <- hmc_results$draws
hmcw_samples <- hmcw_results$draws[1:(burn_in + hmcw_iters), , ] # take first 3000 samples

## Trace plots (before discarding burn-in samples)
png(paste0("./plots/hmc_trace_n", Tfin, ".png"), width = 1200, height = 600)
bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_trace(hmc_samples)
dev.off()

png(paste0("./plots/hmcw_trace_n", Tfin, ".png"), width = 1200, height = 600)
bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_trace(hmcw_samples)
dev.off()

## Discard burn-in samples
hmc_samples <- hmc_samples[-(1:burn_in), , ]
hmcw_samples <- hmcw_samples[-(1:burn_in), , ]

# hmc.Phi <- hmc.Phi[-(1:hmc.burn_in),,]
# hmc.Sigma_eta <- hmc.Sigma_eta[-(1:hmc.burn_in),,]
# hmcw.Phi <- hmcw.Phi[-(1:hmcw.burn_in),,]
# hmcw.Sigma_eta <- hmcw.Sigma_eta[-(1:hmcw.burn_in),,]

###################################
##        MCMC diagnostics       ##
###################################
param_names <- c("Phi[11]", "Phi[22]", "Sigma[eta[11]]", "Sigma[eta[21]]", "Sigma[eta[22]]")
param_dim <- length(param_names)
param_values <- c(diag(Phi), Sigma_eta[lower.tri(Sigma_eta, diag = T)])

ind_df <- data.frame(i = rep(1:d, each = d), j = rep(1:d, d)) # (i,j) indices of elements in a dxd matrix

indmat <- matrix(1:d^2, d, d, byrow = T) # number matrix elements by row
phi_indices <- diag(indmat) # indices of diagonal elements of Phi
sigma_indices <- indmat[lower.tri(indmat, diag = T)] # lower triangular elements of Sigma_eta

## R-VGA-Whittle
rvgaw_Phi_samples_ls <- list()
rvgaw_Sigma_samples_ls <- list()
for (k in phi_indices) {
  inds <- as.numeric(ind_df[k, ])
  # i <- as.numeric(ind_df[k, ][1])
  # j <- as.numeric(ind_df[r, ][2])
  rvgaw_Phi_samples_ls[[k]] <- sapply(rvgaw_samples$Phi, function(x) x[inds[1], inds[2]])

}

for (k in sigma_indices) {
  inds <- as.numeric(ind_df[k, ])
  rvgaw_Sigma_samples_ls[[k]] <- sapply(rvgaw_samples$Sigma_eta, function(x) x[inds[1], inds[2]])
}

rvgaw_samples_ls <- c(rvgaw_Phi_samples_ls, rvgaw_Sigma_samples_ls)

## HMC
param_indices <- c(phi_indices, d^2 + sigma_indices) # indices out of d^2 parameters

# hmc_Phi_samples_ls <- lapply(phi_indices, function(x) c(hmc.Phi[,,x]))
# hmc_Sigma_samples_ls <- lapply(sigma_indices, function(x) c(hmc.Sigma_eta[,,x]))
# hmc_samples_ls <- c(hmc_Phi_samples_ls, hmc_Sigma_samples_ls)

hmc_samples_ls <- lapply(param_indices, function(x) c(hmc_samples[, , x]))
hmc_dns <- hmc_diagnostics(hmc_samples_ls)

## HMCW
# hmcw_Phi_samples_ls <- lapply(phi_indices, function(x) c(hmcw.Phi[,,x]))
# hmcw_Sigma_samples_ls <- lapply(sigma_indices, function(x) c(hmcw.Sigma_eta[,,x]))
# hmcw_samples_ls <- c(hmcw_Phi_samples_ls, hmcw_Sigma_samples_ls)

hmcw_samples_ls <- lapply(param_indices, function(x) c(hmcw_samples[, , x]))
hmcw_dns <- hmc_diagnostics(hmcw_samples_ls)

hmc.Rhat <- hmc_dns$Rhat
hmcw.Rhat <- hmcw_dns$Rhat
hmc.ESS <- hmc_dns$ESS
hmcw.ESS <- hmcw_dns$ESS

methods <- c("HMC-Whittle", "HMC-exact")
metrics <- c("Rhat", "ESS")
dns <- data.frame(params = rep(param_names, times = 2),
                  metrics = rep(metrics, each = length(param_names) * 2),
                  method = rep(methods, each = length(param_names)),
                  value = formatC(c(hmcw.Rhat, hmc.Rhat, hmcw.ESS, hmc.ESS), 
                                  format = "f", width = 5))

write.csv(dns, file = paste0(result_directory, "/multi_sv_diagnostics", block_info, ".csv"), row.names = F)

## Turn the samples into data frames
rvgaw.df <- data.frame(do.call(cbind, rvgaw_samples_ls))
hmc.df <- data.frame(do.call(cbind, hmc_samples_ls))
hmcw.df <- data.frame(do.call(cbind, hmcw_samples_ls))

names(rvgaw.df) <- param_names
names(hmc.df) <- param_names
names(hmcw.df) <- param_names

## Thinning if needed
thin_interval <- 50
inds <- seq(1, nrow(hmc.df), by = thin_interval)
hmc_thin.df <- hmc.df[inds, ]

##############################
##      Posterior plots     ##
##############################

plots <- list()
xlims <- list(c(0.97, 1), c(0.95, 1), c(0, 0.04), c(0, 0.01), c(0, 0.02))

## Marginal posteriors
for (p in 1:param_dim) {
  
  true_vals.df <- data.frame(name = param_names[p], val = param_values[p])

  plot <- ggplot(data = rvgaw.df, aes(x=.data[[param_names[p]]])) +
    # plot <- ggplot(exact_rvgal.df, aes(x=colnames(exact_rvgal.df)[p])) + 
    geom_density(col = "red", lwd = 1) +
    geom_density(data = hmcw.df, col = "goldenrod", lwd = 1) +
    geom_density(data = hmc_thin.df, col = "deepskyblue", lwd = 1) +
    geom_vline(data = true_vals.df, aes(xintercept=val),
               color="black", linetype="dashed", linewidth=1) +
    labs(x = vars) +
    # xlim(x = xlims[[p]]) +
    theme_bw() +
    theme(axis.title = element_blank(), text = element_text(size = 24)) +
    scale_x_continuous(limits = xlims[[p]], breaks = scales::pretty_breaks(n = 3)) + 
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"))
  # theme(legend.position="bottom") + 
  # scale_color_manual(values = c('RVGA' = 'red', 'HMC' = 'blue'))
  
  plots[[p]] <- plot  
}

## Arrange bivariate plots in lower off-diagonals
n_lower_tri <- (param_dim^2 - param_dim)/2 # number of lower triangular elements

index_to_i_j_colwise_nodiag <- function(k, n) {
  kp <- n * (n - 1) / 2 - k
  p  <- floor((sqrt(1 + 8 * kp) - 1) / 2)
  i  <- n - (kp - p * (p + 1) / 2)
  j  <- n - 1 - p
  c(i, j)
}

cov_plots <- list()
for (ind in 1:n_lower_tri) {
  mat_ind <- index_to_i_j_colwise_nodiag(ind, param_dim)
  p <- mat_ind[1]
  q <- mat_ind[2]
  
  param_df <- data.frame(x = param_values[q], y = param_values[p])

  cov_plot <- ggplot(data = rvgaw.df, aes(x = .data[[param_names[q]]], y = .data[[param_names[p]]])) +
    stat_ellipse(col = "red", type = "norm", lwd = 1) +
    stat_ellipse(data = hmcw.df, col = "goldenrod", type = "norm", lwd = 1) +
    stat_ellipse(data = hmc.df, col = "deepskyblue", type = "norm", lwd = 1) +
    geom_point(data = param_df, aes(x = x, y = y),
               shape = 4, color = "black", size = 4) +
    theme_bw() +
    theme(axis.title = element_blank(), text = element_text(size = 24)) +                               # Assign pretty axis ticks
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) + 
    theme(plot.margin = margin(0.35, 0.35, 0.35, 0.35, "cm"))
  
  cov_plots[[ind]] <- cov_plot
}

m <- matrix(NA, param_dim, param_dim)
m[lower.tri(m, diag = F)] <- 1:n_lower_tri 
gr <- grid.arrange(grobs = cov_plots, layout_matrix = m)
gr2 <- gtable_add_cols(gr, unit(1, "null"), -1)
gr3 <- gtable_add_grob(gr2, grobs = lapply(plots, ggplotGrob), t = 1:param_dim, l = 1:param_dim)

# grid.draw(gr3)

# A list of text grobs - the labels
vars <- list(textGrob(bquote(Phi[11])), textGrob(bquote(Phi[22])),
             textGrob(bquote(Sigma[eta[11]])), textGrob(bquote(Sigma[eta[21]])),
             textGrob(bquote(Sigma[eta[21]])))
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
gp <- gtable_add_cols(gr3, unit(2, "lines"), 0)
gp <- gtable_add_rows(gp, unit(2, "lines"), -1) #0 adds on the top

# gtable_show_layout(gp)

# Add the label grobs.
# The labels on the left should be rotated; hence the edit.
# t and l refer to cells in the gtable layout.
# gtable_show_layout(gp) shows the layout.
gp <- gtable_add_grob(gp, lapply(vars[1:param_dim], editGrob, rot = 90), t = 1:param_dim, l = 1)
gp <- gtable_add_grob(gp, vars[1:param_dim], t = param_dim+1, l = 2:(param_dim+1))

grid.newpage()
grid.draw(gp)

if (save_plots) {
  plot_file <- paste0("multi_sv_sim_posterior", "_", Tfin, temper_info, reorder_info, block_info,
                      "_", transform, "_thinned_", date, ".png")
  filepath = paste0("./plots/", plot_file)
  png(filepath, width = 1200, height = 900)
  grid.draw(gp)
  dev.off()
}

## Thinning
# thin_interval <- 100
# hmc.phi_thin <- as.vector(window(hmc.phi_mcmc, thin = thin_interval))
# hmc.sigma_eta_thin <- as.vector(window(hmc.sigma_eta_mcmc, thin = thin_interval))

# hmcw.phi_thin <- as.vector(window(hmcw.phi_mcmc, thin = 1))
# hmcw.sigma_eta_thin <- as.vector(window(hmcw.sigma_eta_mcmc, thin = 1))

# rvgaw.df <- data.frame(
#     phi = rvgaw.phi,
#     sigma_eta = rvgaw.sigma_eta
# )

# hmc.df <- data.frame(
#     phi = hmc.phi,
#     sigma_eta = hmc.sigma_eta
# )

# hmc_thin.df <- data.frame(
#     phi = hmc.phi_thin,
#     sigma_eta = hmc.sigma_eta_thin
# )
# hmcw.df <- data.frame(
#     phi = hmcw.phi_thin,
#     sigma_eta = hmcw.sigma_eta_thin
# )

# names(rvgaw.df) <- param_names
# names(hmc.df) <- param_names
# names(hmc_thin.df) <- param_names
# names(hmcw.df) <- param_names

# true_vals.df <- data.frame(phi = phi, sigma_eta = sigma_eta)



## Timing comparison
rvgaw.time <- rvgaw_results$time_elapsed[3]
hmcw.time <- hmcw_results$time$total / dim(hmcw_results$draws)[1] * (burn_in + hmcw_iters)
hmc.time <- hmc_results$time$total
print(data.frame(method = c("R-VGA-Whittle", "HMC-Whittle", "HMC-exact"),
                 time = c(rvgaw.time, hmcw.time, hmc.time)))

## R-VGA-Whittle trajectories/trace plots
# if (plot_trajectories) {

  # param_names <- c("Phi[11]", "Phi[22]", "Sigma[eta[11]]", "Sigma[eta[22]]", "Sigma[eta[21]]")
  # param_vals <- c(diag(Phi), diag(Sigma_eta), Sigma_eta[lower.tri(Sigma_eta, diag = F)])
  true_vals.df <- data.frame(param = param_names, 
                            value = param_values)

  block_df <- data.frame(cutoff = n_indiv)

  mu_Phi <- lapply(rvgaw_results$mu, function(x) tanh(x[1:d]))
  mu_Sigma <- lapply(rvgaw_results$mu, construct_Sigma_eta, d = d)
  mu_Sigma_vec <- lapply(mu_Sigma, function(S) S[lower.tri(S, diag = T)])
  mu <- mapply(c, mu_Phi, mu_Sigma_vec, SIMPLIFY = F)

  trajectory_df <- as.data.frame(matrix(unlist(mu), nrow = length(mu), byrow = T))
  names(trajectory_df) <- param_names
  
    trajectory_df$iter <- 1:nrow(trajectory_df)

    trajectory_df_long <- trajectory_df %>% pivot_longer(
        cols = !iter,
        names_to = "param", values_to = "value"
    )
    trajectory_plot <- trajectory_df_long %>% ggplot() +
        geom_line(aes(x = iter, y = value), linewidth = 1) +
        facet_wrap(~param, scales = "free", labeller = label_parsed) +
        geom_hline(data = true_vals.df, aes(yintercept = value), linetype = "dashed", linewidth = 1.5) +
        geom_vline(data = block_df, aes(xintercept = cutoff), linetype = "dotted", linewidth = 1.5) +
        theme_bw() +
        theme(text = element_text(size = 28)) +
        xlab(TeX("Iterations ($\\tilde{k}$)")) +
        ylab("Value")

    if (save_plots) {
      png(paste0("plots/trajectories_multi_sv_sim", block_info, ".png"), width = 1000, height = 500)
      print(trajectory_plot)
      dev.off()
    }

# }

## HMC and HMC-Whittle trace plots

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

  hmcw.df_long <- hmcw.df %>% mutate(n = row_number()) %>% 
  pivot_longer(
      cols = !n,
      names_to = "param", values_to = "value"
  )

## Traceplots

# hmc.traceplots <- list()
# for (p in 1:param_dim) {
# p <- 1
  hmc.traceplot <- hmc.df_long %>% ggplot() + geom_line(aes(x = n, y = value), linewidth = 1) +
      geom_hline(data = true_vals.df, aes(yintercept = value), col = "red", 
                  linetype = "dashed", linewidth = 1.5) +
      facet_wrap(~param, scales = "free", labeller = label_parsed) +
      theme_bw() +
      theme(text = element_text(size = 28)) +
      xlab("Iterations") +
      ylab("Value")
  print(hmc.traceplot)

  hmc_thin.traceplot <- hmc_thin.df_long %>% ggplot() + geom_line(aes(x = n, y = value), linewidth = 1) +
  geom_hline(data = true_vals.df, aes(yintercept = value), col = "red", 
              linetype = "dashed", linewidth = 1.5) +
  facet_wrap(~param, scales = "free", labeller = label_parsed) +
  theme_bw() +
  theme(text = element_text(size = 28)) +
  xlab("Iterations") +
  ylab("Value")
  print(hmc_thin.traceplot)

  hmcw.traceplot <- hmcw.df_long %>% ggplot() + geom_line(aes(x = n, y = value), linewidth = 1) +
  geom_hline(data = true_vals.df, aes(yintercept = value), col = "red", 
              linetype = "dashed", linewidth = 1.5) +
  facet_wrap(~param, scales = "free", labeller = label_parsed) +
  theme_bw() +
  theme(text = element_text(size = 28)) +
  xlab("Iterations") +
  ylab("Value")
  print(hmcw.traceplot)
    # scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
  # theme(legend.position="bottom") + 
  # scale_color_manual(values = c('RVGA' = 'red', 'HMC' = 'blue'))

  # hmc.traceplots[[p]] <- plot  
# }
  if (save_plots)  {
      png("./plots/multi_sv_hmc_traceplot.png", width = 1500, height = 500)
      print(hmc.traceplot)
      dev.off()

      png("./plots/multi_sv_hmc_traceplot_thin.png", width = 1500, height = 500)
      print(hmc_thin.traceplot)
      dev.off()

      png("./plots/multi_sv_hmcw_traceplot.png", width = 1500, height = 500)
      print(hmcw.traceplot)
      dev.off()

  }
