// this model is stochastic level and seasonal
data{
  int<lower=0> N;
  int<lower=1> k; //number of seasons
  real y[N];
}

parameters{
  real<lower=0> sigma_obs;
  real<lower=0> sigma_state;
  real<lower=0> sigma_seasons;
  vector[N] all_seasons;
  vector[N] mu_lvl;
}

transformed parameters{
  vector[N] mu;
  mu = mu_lvl + all_seasons;
}

model{
  
  for (t in 1:k-1)
    all_seasons[t] ~ cauchy(0,2);
  for (t in k:N)
    all_seasons[t] ~ normal(-sum(all_seasons[t-(k-1):t-1]), sigma_seasons);
  
  mu_lvl[1] ~ normal(y[1], 5);
  for (i in 2:N)  
    mu_lvl[i] ~ normal(mu_lvl[i-1], sigma_state);
  
  y ~ normal(mu, sigma_obs);
}

generated quantities{
  vector[N] log_lik;
  vector[N] y_hat;
  vector[N] diff_hat;
  
  
  for (i in 1:N){
    y_hat[i] = normal_rng(mu[i], sigma_seasons);
    diff_hat[i] = y_hat[i] - y[i];
    log_lik[i] = normal_lpdf(y[i] | mu[i], sigma_obs);
  }
  
}
