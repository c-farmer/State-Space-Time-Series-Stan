data {
  int<lower=0> N;
  real y[N];
  vector[N] x;
}

parameters{
  vector[N] mu_lvl;
  real beta;
  real<lower=0> sigma_obs;
  real<lower=0> sigma_level;
}

transformed parameters{
  vector[N] mu;
  mu = mu_lvl + beta * x;
}

model{
  mu_lvl[1] ~ normal(y[1], 5);
  mu_lvl[2:N] ~ normal(mu_lvl[1:N-1], sigma_level);
  
  y ~ normal(mu, sigma_obs);
}

generated quantities{
  vector[N] log_lik;
  vector[N] y_hat;
  
  
  for (i in 1:N){
    y_hat[i] = normal_rng(mu[i], sigma_obs);
    log_lik[i] = normal_lpdf(y[i] | mu[i], sigma_obs);
  }
}
