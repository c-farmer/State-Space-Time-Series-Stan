//level is now stochastic 
data{
  int<lower=0> N;
  real y[N];
}

parameters{
  real mu[N];
  real<lower=0> sigma_obs;
  real<lower=0> sigma_state;
}

model{
  mu[1] ~ normal(y[1], 5);      
  mu[2:N] ~ normal(mu[1:(N-1)], sigma_state);
  
  y ~ normal(mu, sigma_obs);
}

generated quantities{
  vector[N] log_lik;  //capture for LOO/WAIC
  vector[N] y_hat;    //model generated observations
  
    for (i in 1:N){
      log_lik[i] = normal_lpdf(y[i] | mu[i], sigma_obs);
      y_hat[i] = normal_rng(mu[i], sigma_obs);
    }  
}
