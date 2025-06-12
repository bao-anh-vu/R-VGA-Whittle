functions {
  vector frac_diff_weights(real d, int K) {
    vector[K + 1] w;
    w[1] = 1.0;
    for (k in 1:K) {
      w[k + 1] = w[k] * (-(d - k + 1)) / k;
    }
    return w;
  }

  vector frac_diff(vector x, real d, int K) {
    int N = num_elements(x);
    vector[N - K] x_diff;
    vector[K + 1] w = frac_diff_weights(d, K);
    
    for (t in (K + 1):N) {
      real sum = 0;
      for (k in 0:K) {
        sum += w[k + 1] * x[t - k];
      }
      x_diff[t - K] = sum;
    }
    return x_diff;
  }
}

data {
  int<lower=1> N;
  vector[N] y;
  int<lower=1> K;
  vector[5] prior_mean;
  vector[5] diag_prior_var;
  int use_t_noise;
}

parameters {
  vector[N] x;  // latent state
  real mu;
  real tilde_phi;
  real tilde_theta;
  real tilde_d;
  real tilde_sigma_eta;
  real tilde_nu;
}

transformed parameters {
  real<lower = -1, upper = 1> phi = tanh(tilde_phi);
  real<lower = -1, upper = 1> theta = tanh(tilde_theta);
  real<lower = -0.5, upper = 0.5> d = 0.5 * tanh(tilde_d);
  real<lower = 0> sigma_eta = sqrt(exp(tilde_sigma_eta));
  real<lower = 0> nu = sqrt(exp(tilde_nu));
}

model {
  vector[N - K] x_diff = frac_diff(x, d, K);
  vector[N - K] eta;
  vector[N] v;

  // Priors on transformed parameters
  tilde_phi ~ normal(prior_mean[1], sqrt(diag_prior_var[1]));
  tilde_theta ~ normal(prior_mean[2], sqrt(diag_prior_var[2]));
  tilde_d ~ normal(prior_mean[3], sqrt(diag_prior_var[3]));
  tilde_sigma_eta ~ normal(prior_mean[4], sqrt(diag_prior_var[4]));
  tilde_nu ~ normal(prior_mean[5], sqrt(diag_prior_var[5]));

  eta[1] = x_diff[1] - mu;

  for (t in 2:(N - K)) {
    real ar_term = phi * (x_diff[t - 1] - mu);
    real ma_term = theta * eta[t - 1];
    real pred = mu + ar_term + ma_term;
    eta[t] = x_diff[t] - pred;
  }

  // ARFIMA(1,d,1) latent process innovations
  eta ~ normal(0, sigma_eta);        
  
  // Observations
  if (use_t_noise) {
    v = y - x; // Measurement noise 
    v ~ student_t(nu, 0, 1);
  } else {
    y ~ normal(x, nu); 
  }
            
}
