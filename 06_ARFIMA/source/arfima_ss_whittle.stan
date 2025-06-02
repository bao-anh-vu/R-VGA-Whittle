functions {
  // Compute spectral density here
  real compute_spec_dens(real phi, real theta, real d, real sigma_eta, real nu, real freq) {
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

//    spec_dens_eps = 1/(2*pi()) * (nu/(nu - 2));
//    spec_dens_eps = nu/(nu - 2); // t noise
      spec_dens_eps = nu^2; // Gaussian noise

    spec_dens = spec_dens_x + spec_dens_eps;

    return spec_dens;
  }
}

data {
  int<lower=0> nfreq;   // # time points (equally spaced)
  vector[nfreq] freqs;
  vector[nfreq] periodogram;
  vector[5] prior_mean;
  vector[5] diag_prior_var;
  int fix_sigma;
}

parameters {
  real tilde_phi;
  real tilde_theta;
  real tilde_d;
  real tilde_sigma_eta;
  real tilde_nu;
  
  //vector[Tfin] x; 
  // log volatility at time t
}

transformed parameters {
  real<lower = -1, upper = 1> phi;
  real<lower = -1, upper = 1> theta;
  real<lower = -0.5, upper = 0.5> d;
  real<lower = 0> sigma_eta;
  real<lower = 0> nu;
  
    phi = tanh(tilde_phi);
    theta = tanh(tilde_theta); 
    d = 0.5 * tanh(tilde_d);

if (fix_sigma == 0) {
  sigma_eta = sqrt(exp(tilde_sigma_eta));
} else {
   sigma_eta = 1;
}

 
//    nu = 2 + exp(tilde_nu);
    nu = sqrt(exp(tilde_nu));
  
}

model {
  vector[nfreq] spec_dens_inv;
  //vector[nfreq] spec_dens;
  
  tilde_phi ~ normal(prior_mean[1], sqrt(diag_prior_var[1]));
  tilde_theta ~ normal(prior_mean[2], sqrt(diag_prior_var[2]));
  tilde_d ~ normal(prior_mean[3], sqrt(diag_prior_var[3]));

  if (fix_sigma == 0) {
    tilde_sigma_eta ~ normal(prior_mean[4], sqrt(diag_prior_var[4]));
  }
  tilde_nu ~ normal(prior_mean[5], sqrt(diag_prior_var[5]));
  
  for (k in 1:nfreq) { 
    //spec_dens[k] = compute_spec_dens(0.22, 0.5, 0.25, 1, 50, freqs[k]);
    spec_dens_inv[k] = 1/compute_spec_dens(phi, theta, d, sigma_eta, nu, freqs[k]);
  }
  
  //print(spec_dens[1:5]);

  periodogram ~ exponential(spec_dens_inv); 
}