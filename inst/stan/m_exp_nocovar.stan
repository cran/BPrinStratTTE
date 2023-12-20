data {
  
  // --- Scalars
  int<lower=1>             n;          // number of patients
  int<lower=1>             tg;         // length of time grid

  // --- Individual level data
  int<lower=0, upper=1>    Z[n];       // treatment assignment
  int<lower=0, upper=1>    S[n];       // observed ICE development
  real<lower=0>            TIME[n];    // time to event
  int<lower=0, upper=1>    EVENT[n];   // event

  // --- Prior parameters
  real<lower=0>            prior_piT[2];
  real<lower=0>            prior_0N[2];
  real<lower=0>            prior_1N[2];
  real<lower=0>            prior_0T[2];
  real<lower=0>            prior_1T[2];
  
  // --- Time grid for RMST
  vector[tg]  t_grid;
  
}

parameters {
  
  real<lower=0, upper=1>  pi_T;
  real<lower=0>  lambda_0N;
  real<lower=0>  lambda_1N;
  real<lower=0>  lambda_0T;
  real<lower=0>  lambda_1T;
  
}

model {
  
  // --- Priors
  pi_T ~ beta(prior_piT[1],prior_piT[2]);
  lambda_0N ~ gamma(prior_0N[1],prior_0N[2]);
  lambda_1N ~ gamma(prior_1N[1],prior_1N[2]);
  lambda_0T ~ gamma(prior_0T[1],prior_0T[2]);
  lambda_1T ~ gamma(prior_1T[1],prior_1T[2]);

  // --- Likelihood
  {
    vector[n]   prob;
    for (i in 1:n) {
      if (Z[i]==0) {
        // ND or TD, placebo
        prob[i] = log_mix(pi_T,
                          EVENT[i]*log(lambda_0T)-lambda_0T*TIME[i],
                          EVENT[i]*log(lambda_0N)-lambda_0N*TIME[i]);
      } else if (Z[i]==1 && S[i]==0) {
        // ND, treatment
        prob[i] = log1m(pi_T) + (EVENT[i]*log(lambda_1N)-lambda_1N*TIME[i]);
      } else if (Z[i]==1 && S[i]==1) {
        // TD, treatment
        prob[i] = log(pi_T) + (EVENT[i]*log(lambda_1T)-lambda_1T*TIME[i]);
      }
    }
    target += prob;
  }
  
}

generated quantities {
  
  real  hr_N;     // hazard ratio for ND
  real  hr_T;     // hazard ratio for TD
  real  rmst_N;   // difference in rmst for ND
  real  rmst_T;   // difference in rmst for TD
  
  vector[tg]  S_0N;
  vector[tg]  S_1N;
  vector[tg]  S_0T;
  vector[tg]  S_1T;
  real  rmst_0N;
  real  rmst_1N;
  real  rmst_0T;
  real  rmst_1T;
  
  // Hazard ratio 
  hr_N = (lambda_1N)/(lambda_0N);
  hr_T = (lambda_1T)/(lambda_0T);
  
  // Restricted mean survival time
  for (t in 1:tg) {
    S_0N[t] = 1-exponential_cdf(t_grid[t], lambda_0N);
    S_1N[t] = 1-exponential_cdf(t_grid[t], lambda_1N);
    S_0T[t] = 1-exponential_cdf(t_grid[t], lambda_0T);
    S_1T[t] = 1-exponential_cdf(t_grid[t], lambda_1T);
  }
  rmst_0N = 0; 
  rmst_1N = 0; 
  rmst_0T = 0; 
  rmst_1T = 0; 
  for (j in 2:tg) {
    rmst_0N = rmst_0N + ((S_0N[j-1]+S_0N[j])/2*(t_grid[j]-t_grid[j-1]));
    rmst_1N = rmst_1N + ((S_1N[j-1]+S_1N[j])/2*(t_grid[j]-t_grid[j-1]));
    rmst_0T = rmst_0T + ((S_0T[j-1]+S_0T[j])/2*(t_grid[j]-t_grid[j-1]));
    rmst_1T = rmst_1T + ((S_1T[j-1]+S_1T[j])/2*(t_grid[j]-t_grid[j-1]));
  }
  rmst_N = rmst_1N - rmst_0N;
  rmst_T = rmst_1T - rmst_0T;
  
}
