functions {
  
  matrix to_matrix_colwise(vector v, int m, int n) {
    matrix[m, n] res;
    for (j in 1:n) {
      for (i in 1:m) {
        res[i, j] = v[(j - 1) * m + m];
      }
    }
    return res;
  }
  
  matrix kronecker_prod(matrix A, matrix B) {
    matrix[rows(A) * rows(B), cols(A) * cols(B)] C;
    int m;
    int n;
    int p;
    int q;
    m = rows(A);
    n = cols(A);
    p = rows(B);
    q = cols(B);
    for (i in 1:m) {
      for (j in 1:n) {
        int row_start;
        int row_end;
        int col_start;
        int col_end;
        row_start = (i - 1) * p + 1;
        row_end = (i - 1) * p + p;
        col_start = (j - 1) * q + 1;
        col_end = (j - 1) * q + q; // the original Stan function had a mistake here; should be +q not +1
        C[row_start:row_end, col_start:col_end] = A[i, j] * B;
      }
    }
    return C;
  }
  
  matrix to_lowertri(vector x, int d) {
    int pos = 1;
    matrix[d, d] L;
    vector[num_elements(x) - d] lower_entries;
    // for (i in 2:d) {
    //   for (j in 1:(i-1)) {
    //     L[i,j] = x[pos]
    //     pos = pos + 1
    //   }
    // }
    L = diag_matrix(exp(x[1:d])); // the first d elements form the diagonal entries
    lower_entries = x[(d+1):num_elements(x)];
    
    for (i in 2:d) {
      for (j in 1:(i-1)) {
        L[i, j] = lower_entries[pos];
        pos += 1;
      }
    }
    return L;
  }
  
  matrix arima_stationary_cov2(matrix T, matrix R) {
    matrix[rows(T), cols(T)] Q0;
    matrix[rows(T) * rows(T), rows(T) * rows(T)] TT;
    vector[rows(T) * rows(T)] RR;
    int m;
    int m2;
    m = rows(T);
    m2 = m * m;
    RR = to_vector(tcrossprod(R));
    TT = kronecker_prod(T, T);
    // Q0 = to_matrix_colwise((diag_matrix(rep_vector(1.0, m2)) - TT) \ RR, m, m);
    //Q0 = to_matrix_colwise(rep_vector(1.0, m2), m, m);
    Q0 = diag_matrix(rep_vector(1.0, m));
    return Q0;
  }
  
  matrix arima_stationary_cov(matrix Phi, matrix Sigma_eta) {
    matrix[rows(Phi), cols(Phi)] Sigma1;
    matrix[rows(Phi) * rows(Phi), rows(Phi) * rows(Phi)] Phi_kron;
    vector[rows(Phi) * rows(Phi)] vec_Sigma1;
    int m;
    int m2;
    m = rows(Phi);
    m2 = m * m;
    Phi_kron = kronecker_prod(Phi, Phi);
    Sigma1 = to_matrix((diag_matrix(rep_vector(1.0, m2)) - Phi_kron) \ to_vector(Sigma_eta), m, m);

    return Sigma1;
  }
  
  matrix to_VAR1_trans_mat(matrix A, matrix Sigma_eta) {
    //int d;
    //d = rows(A);
    matrix[rows(A), cols(A)] Phi_mat;
    //matrix[d, d] Sigma_eta_mat;
    matrix[rows(A), cols(A)] B;
    matrix[rows(A), cols(A)] P1;
    matrix[rows(A), cols(A)] Sigma_tilde;
    matrix[rows(A), cols(A)] P;
    matrix[rows(A), cols(A)] Q;
    matrix[rows(A), cols(A)] T;
    int d;
    d = rows(A);
    B = cholesky_decompose(diag_matrix(rep_vector(1.0, d)) + A * A');
    P1 = inverse(B) * A; // B \ A; // same as inv(B) * A
    Sigma_tilde = diag_matrix(rep_vector(1.0, d)) - P1 * P1';
    P = cholesky_decompose(Sigma_tilde);
    Q = cholesky_decompose(Sigma_eta);
    T = Q / P; // same as Q * inv(P)
    Phi_mat = T * P1 * inverse(T);
    return Phi_mat;
  }
}

data {
  int d;    // dimension of the data at time t
  int<lower=0> Tfin;   // time points (equally spaced)
  matrix[Tfin, d] Y;
  int transform;
  // int<lower = 0, upper = 1> use_chol;
  //matrix[d, Tfin] X;
  // int<lower = 0> num_L; // number of lower triangular elements
  // 
  // if (use_chol == 1) {
  //   num_L = d + d*(d-1)/2;
  // } else {
  //   num_L = d;
  // }

  //matrix[d, d] Sigma_1;
  vector[d] prior_mean_Phi;
  //matrix[d*d, d*d] prior_var_A;
  vector[d] diag_prior_var_Phi;
  vector[d + d*(d-1)/2] prior_mean_gamma;
  //matrix[d, d] prior_var_gamma;
  vector[d + d*(d-1)/2] diag_prior_var_gamma;
}

parameters {
  // matrix[d, d] A;
  vector[d] theta_phi;
  //real gamma_11;
  //real gamma_22;
  vector[d + d*(d-1)/2] gamma;
  matrix[Tfin, d] X;
}

transformed parameters { // define the mapping from A to Phi here
  matrix[d, d] Phi_mat;
  cov_matrix[d] Sigma_eta_mat;
  //cholesky_factor_cov[d] L;
  matrix[d,d] L;
  
    // L = diag_matrix(exp(gamma[1:d]));
    // L[2,1] = gamma[d+1];

  L = to_lowertri(gamma, d);
  // L = diag_matrix(rep_vector(1, d));
  Sigma_eta_mat = L*L';
  // Phi_mat = to_VAR1_trans_mat(A, Sigma_eta_mat);
  // Phi_mat = diag_matrix(tanh(theta_phi[1:d]));
  Phi_mat = transform ? diag_matrix(tanh(theta_phi[1:d])) : diag_matrix(1 / (1+exp(-theta_phi[1:d])));
}
model {
  //to_vector(A) ~ multi_normal(prior_mean_A, prior_var_A);
  //gamma ~ multi_normal(prior_mean_gamma, prior_var_gamma);
  // to_vector(A) ~ normal(prior_mean_A, sqrt(diag_prior_var_A));
  theta_phi ~ normal(prior_mean_Phi, sqrt(diag_prior_var_Phi));
  gamma ~ normal(prior_mean_gamma, sqrt(diag_prior_var_gamma));
  
  // gamma ~ normal(rep_vector(0, 3), rep_vector(1, 3));
  // X[, 1] ~ multi_normal(rep_vector(0, d),
  //                    arima_stationary_cov2(Phi_mat, cholesky_decompose(Sigma_eta_mat)));
  X[1, ] ~ multi_normal(rep_vector(0, d), arima_stationary_cov(Phi_mat, Sigma_eta_mat));
  // X[, 1] ~ multi_normal(rep_vector(0, d), diag_matrix(rep_vector(1.0, d)));
  for (t in 2:Tfin)
    // X[, t] ~ normal(Phi_mat * X[, t-1], sqrt(exp(gamma)));
    X[t, ] ~ multi_normal(Phi_mat * X[t-1, ]', Sigma_eta_mat);
  for (t in 1:Tfin)
    Y[t, ] ~ normal(rep_vector(0, d), exp(X[t, ]/2));
}