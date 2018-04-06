data{
  int<lower=0> N;
  real y[N];
}

parameters{
  real mu[N];
  real nu[N];
  real<lower=0> sigma_obs;
  real<lower=0> sigma_state;
  real<lower=0> sigma_slope;
}

model{

  nu[1] ~ cauchy(0, 1);
  nu[2:N] ~ normal(nu[1:(N-1)], sigma_slope);
  
  mu[1] ~ normal(y[1], 5);
  for (i in 2:N)  
    mu[i] ~ normal(mu[i-1] + nu[i-1], sigma_state);
  
  y ~ normal(mu, sigma_obs);
}

generated quantities{
  vector[N] log_lik;
  vector[N] y_hat;
  vector[N] nu_hat;
  
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | mu[i], sigma_obs);
    y_hat[i] = normal_rng(mu[i], sigma_obs);
    nu_hat[i] = normal_rng(nu[i], sigma_slope);
  }
}
