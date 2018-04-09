data {
  int<lower=0> N;
  real y[N];
  vector[N] x;
}

parameters{
  real mu_lvl;
  real beta;
  real<lower=0> sigma_obs;
}

transformed parameters{
  vector[N] mu;
  mu = mu_lvl + beta * x;
}

model{
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
