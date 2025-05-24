compute_grad <- tf_function(
    compute_grad <- function(samples_tf, I_i, freq_i, blocksize, noise_dist = "t") {
        log_likelihood_tf <- 0
        with(tf$GradientTape() %as% tape2, {
            with(tf$GradientTape(persistent = TRUE) %as% tape1, {
                S <- as.integer(nrow(samples_tf))

                freq_i <- tf$reshape(freq_i, c(1L, blocksize, 1L)) # 1 x blocksize x 1

                phi_s <- tf$math$tanh(samples_tf[, 1])
                phi_s <- tf$reshape(phi_s, c(length(phi_s), 1L, 1L)) # S x 1 x 1

                theta_s <- tf$math$tanh(samples_tf[, 2])
                theta_s <- tf$reshape(theta_s, c(length(theta_s), 1L, 1L)) # S x 1 x 1

                d_s <- 0.5 * tf$tanh(samples_tf[, 3]) # fixed for now

                sigma_eta2_s <- tf$math$exp(samples_tf[, 4])
                sigma_eta2_s <- tf$reshape(sigma_eta2_s, c(dim(sigma_eta2_s), 1L, 1L))

                ## Calculate the spectral density of x
                # term1 <- tf$multiply(tf$constant(1 / (2*pi), "float64"), sigma_eta2_tiled)
                sigma_eta2_tiled <- tf$tile(sigma_eta2_s, c(1L, blocksize, 1L))
                term1 <- sigma_eta2_tiled

                arg <- tf$math$exp(tf$multiply(-1i, tf$cast(freq_i, "complex128")))
                base <- tf$cast(tf$math$abs(1 - arg), "float64")
                term2 <- tf$transpose(base^(-2 * d_s)) 
                # no need to tile this? because freq_i is already in blocks

                term3_num <- 1 + tf$multiply(tf$cast(theta_s, "complex128"), arg)
                term3_den <- 1 - tf$multiply(tf$cast(phi_s, "complex128"), arg)
                term3 <- tf$math$square(tf$math$abs(tf$divide(term3_num, term3_den)))

                ## spectral density of the latent process
                spec_dens_x_tf <- tf$multiply(tf$multiply(term1, term2), term3)
                
                spec_dens_y_tf <- spec_dens_x_tf #+ spec_dens_eps_tf
 
                I_i <- tf$reshape(tf$cast(I_i, dtype = "float64"), c(1L, blocksize, 1L))
                I_tile <- tf$tile(I_i, c(S, 1L, 1L))
                log_likelihood_tf <- - tf$math$log(spec_dens_y_tf) - tf$multiply(I_i, tf$math$reciprocal(spec_dens_y_tf))

                log_likelihood_tf <- tf$math$reduce_sum(log_likelihood_tf, 1L) # sum all log likelihoods over the block

            })
            grad_tf %<-% tape1$gradient(log_likelihood_tf, samples_tf)
           
        })

        grad2_tf %<-% tape2$batch_jacobian(grad_tf, samples_tf)

        E_grad_tf <- tf$reduce_mean(grad_tf, 0L)
        E_hessian_tf <- tf$reduce_mean(grad2_tf, 0L)

        return(list(
            spec_dens_x_tf = spec_dens_x_tf,
            log_likelihood = log_likelihood_tf,
            grad = grad_tf,
            hessian = grad2_tf,
            E_grad = E_grad_tf,
            E_hessian = E_hessian_tf
        ))
    }
)
