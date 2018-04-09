//local level with intervention, both static

data{
  int<lower=0> N;
  vector<lower=0, upper=1>[N] w;
  real y[N];
}

parameters{
  real mu;
  real lambda;
  real<lower=0> sigma_obs;
}

model{
  y ~ normal(mu + lambda*w, sigma_obs);
}

generated quantities{
  vector[N] y_hat;
  vector[N] log_lik;
  
  for (i in 1:N){
    log_lik[i] = normal_lpdf(y[i] | mu + lambda * w[i], sigma_obs);
    y_hat[i] = normal_rng(mu + lambda*w[i], sigma_obs);
  }
}
