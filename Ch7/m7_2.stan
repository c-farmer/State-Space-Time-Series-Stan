data{
  int<lower=0> N;
  real y[N];      //dependent
  real x[N];      //independent
  int k;          //seasonality
  real w[N];      //dummy
}

parameters{
  vector[N] mu;
  real beta;
  vector[k-1] season;
  vector[N] all_seasons;
  real lambda;
  real<lower=0> sigma_obs;
  real<lower=0> sigma_state;
  real<lower=0> sigma_season;

}

model{

  mu[1] ~ normal(y[1], 5);
  mu[2:N] ~ normal(mu[1:N-1], sigma_state);
  
  all_seasons[1:k-1] ~ normal(season[1:k-1], sigma_season);
  for (t in k:N)
    all_seasons[t] ~ normal(-sum(all_seasons[t-(k-1):t-1]), sigma_season);

  for (t in 1:N)
    y[t] ~ normal(mu[t] + all_seasons[t] + lambda * w[t] + beta * x[t], sigma_obs);
}

generated quantities{
  vector[N] y_hat;
  vector[N] log_lik;
  vector[N] y_plot;
  
  for(i in 1:N){
    y_plot[i] = normal_rng(mu[i] + lambda * w[i] + beta * x[i], sigma_obs);
    y_hat[i] = normal_rng(mu[i] + all_seasons[i] + lambda * w[i] + beta * x[i], sigma_obs);
    log_lik[i] = normal_lpdf(y[i] | mu[i] + all_seasons[i] + lambda * w[i] + beta * x[i], sigma_obs);
    }
    
}
