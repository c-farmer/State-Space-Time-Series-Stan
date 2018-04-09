//local level with intervention, both stochastic

data{
  int<lower=0> N;
  vector<lower=0, upper=1>[N] w;
  real y[N];
}

parameters{
  vector[N] mu;
  vector[N] lambda;
  real<lower=0> sigma_obs;
  real<lower=0> sigma_lambda;
  real<lower=0> sigma_state;
}

model{
  
  mu[1] ~ normal(y[1], 5);
  mu[2:N] ~ normal(mu[1:N-1], sigma_state);
  
  lambda[1] ~ normal(0,1);
  lambda[2:N] ~ normal(lambda[1:N-1], sigma_lambda);
  
  for (i in 1:N)
    y[i] ~ normal(mu[i] + lambda[i] * w[i], sigma_obs);
}

generated quantities{
  vector[N] y_hat;
  vector[N] log_lik;
  
  for (i in 1:N){
    y_hat[i] = normal_rng(mu[i] + lambda[i]*w[i], sigma_obs);
    log_lik[i] = normal_lpdf(y[i] | mu[i] + lambda[i]*w[i], sigma_obs);
  }
}
