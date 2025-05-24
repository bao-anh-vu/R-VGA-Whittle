functions {
  // Compute spectral density here
  real compute_spec_dens(real phi, real theta, real d, real sigma_eta, real freq) {

    real term1;
    real term2;
    real term3;
    real spec_dens_x;
    real spec_dens_eps;
    real spec_dens;

    term1 = sigma_eta^2; // / (2*pi());
    term2 = abs(1 - exp(-1i * freq))^(-2 * d);
    term3 = square(abs((1 + theta * exp(-1i * freq)) / (1 - phi * exp(-1i * freq))));
    spec_dens_x = term1 * term2 * term3;

    spec_dens_eps = pi()^2/2;

    spec_dens = spec_dens_x + spec_dens_eps;

    return spec_dens;
  }
  
}

data {
  int<lower=0> nfreq;   // # time points (equally spaced)
  vector[nfreq] freqs;
  vector[nfreq] periodogram;
  vector[4] prior_mean;
  vector[4] diag_prior_var;
}

parameters {
  real tilde_phi;
  real tilde_theta;
  real tilde_d;
  real tilde_sigma_eta;
}

transformed parameters {
  real<lower = -1, upper = 1> phi;
  real<lower = -1, upper = 1> theta;  
  real<lower = 0> sigma_eta;
  real<lower = -0.5, upper = 0.5> d;

  phi = tanh(tilde_phi);
  theta = tanh(tilde_theta); 
  d = 0.5 * tanh(tilde_d);
  sigma_eta = sqrt(exp(tilde_sigma_eta));

  //print("phi = ", phi);
  //print("sigma_eta = ", sigma_eta);
}

model {
  vector[nfreq] spec_dens_inv;
  
  tilde_phi ~ normal(prior_mean[1], sqrt(diag_prior_var[1]));
  tilde_theta ~ normal(prior_mean[2], sqrt(diag_prior_var[2]));
  tilde_d ~ normal(prior_mean[3], sqrt(diag_prior_var[3]));
  tilde_sigma_eta ~ normal(prior_mean[4], sqrt(diag_prior_var[4]));

  for (k in 1:nfreq) {
    spec_dens_inv[k] = 1/compute_spec_dens(phi, theta, d, sigma_eta, freqs[k]);
  }
  
  periodogram ~ exponential(spec_dens_inv); 
}