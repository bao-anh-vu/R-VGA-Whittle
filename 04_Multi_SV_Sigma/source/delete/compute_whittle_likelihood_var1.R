compute_whittle_likelihood_multi_lgss <- function(Y, params) {
  
  Tfin <- ncol(X)
  
  A <- params$A
  Sigma_eta <- params$Sigma_eta
  
  ## Fourier frequencies
  k <- seq(-ceiling(Tfin/2)+1, floor(Tfin/2), 1)
  k_in_likelihood <- k [k >= 1 & k <= floor((Tfin-1)/2)]
  freq <- 2 * pi * k_in_likelihood / Tfin
  
  # Y_tilde <- log(Y^2) - rowMeans(log(Y^2))
  
  # J <- fft(Y)
  # J <- t(J)
  # I_omega <- 1/(2*pi*Tfin) * J %*% Conj(t(J)) # Should this be TxT?
  
  # J_test <- 0
  # for (t in 1:Tfin) {
  #   J_test <- J_test + Y[, t] * exp(-1i * freq[1] * t) 
  # }
  # test <- exp(-1i * freq[1] * (0:(Tfin-1)))
  # test2 <- t(cbind(test, test))
  # test2 <- t(cbind(test, test))
  # J_all <- Y * test2 # but there is no Y_0???
  # J_test2 <- rowSums(J_all)
  
  ##########################################
  
  # J_list <- list()
  # for (k in 1:length(freq)) {
  #   test <- exp(-1i * freq[k] * (0:(Tfin-1))) #(1:Tfin))
  #   # test2 <- t(cbind(test, test))
  #   # J_manual <- X[, 1] * exp(-1i * freq[1] * 1) # need to check the fft calculation manually
  #   J_all <- Y_tilde * t(cbind(test, test)) # but there is no Y_0???
  #   J_list[[k]] <- rowSums(J_all)
  # }
  # 
  # # # J_vec <- lapply(J, function(j) j[1])
  # # # re <- Re(unlist(J_vec))
  # # fft_out <- fft(Y_tilde)
  # # J_fft <- fft_out[, 2:501]
  # # # head(Re(fft_out[1, 2:501]))
  # # J_list <- lapply(seq_len(ncol(J_fft)), function(i) J_fft[, i])
  # 
  # I_all <- lapply(J_list, function(M, Tfin) 1/(2*pi*Tfin) * M %*% Conj(t(M)), Tfin = Tfin)
  # 
  
  # ## astsa package
  fft_out <- mvspec(t(X))
  I_all <- fft_out$fxx
  # 
  
  # Spectral density matrix
  Phi_0 <- diag(2)
  Phi_1 <- A
  Theta <- diag(2)
  
  log_likelihood <- 0
  for (k in 1:length(freq)) {
    Phi_inv <- solve(Phi_0 - Phi_1 * exp(- 1i * freq[k]))
    Phi_inv_H <- Conj(t(Phi_inv))
    
    # M <- Phi_0 - Phi_1 * exp(- 1i * freq[k])
    # M_H <- Conj(t(M))
    # M_H_inv <- solve(M_H)
    
    # spec_dens_X <- 1/(2*pi) * Phi_inv %*% Theta %*% Sigma_eta %*% Theta %*% Phi_inv_H
    spec_dens_X <- Phi_inv %*% Theta %*% Sigma_eta %*% Theta %*% Phi_inv_H
    
    # spec_dens_Xi <- 1/(2*pi) * diag(pi^2/2, 2)
    # spec_dens_Xi <- diag(pi^2/2, 2)
    
    spec_dens <- spec_dens_X #+ spec_dens_Xi  
    
    part2 <- sum(diag(solve(spec_dens) %*% I_all[, , k]))
    
    # log(det(spec_dens))
    
    det_spec_dens <- prod(eigen(spec_dens)$values)
    part1 <- log(det_spec_dens)
    
    log_likelihood <- log_likelihood - (part1 + part2)
    # test <- spec_dens[1, 2] * spec_dens[2, 1] - spec_dens[1, 1] * spec_dens[2, 2]
  }
  
  return(log_likelihood)
  
}