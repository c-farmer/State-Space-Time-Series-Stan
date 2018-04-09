//This model is seasonal fluctuations only
data{
  int<lower=0> N;
  int<lower=1> k; //number of seasons
  real y[N];
}

parameters{
  real mu_lvl;
  vector[k-1] season;
  real<lower=0> sigma_obs;
}

transformed parameters{
  vector[N] mu;
  vector[N] all_seasons;
  
  for (t in 1:k-1)
    all_seasons[t] = season[t];
  
  for (t in k:N)
    all_seasons[t] = - sum(all_seasons[t-(k-1):t-1]);

  mu = mu_lvl + all_seasons;
}

model{
    y ~ normal(mu, sigma_obs);
}

generated quantities{
  vector[N] log_lik;
  vector[N] y_hat;
  
    for (i in 1:N) {
      log_lik[i] = normal_lpdf(y[i] | mu[i], sigma_obs);
      y_hat[i] = normal_rng(mu[i], sigma_obs);
    }
}
