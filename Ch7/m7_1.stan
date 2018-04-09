data{
  int<lower=0> N;
  real y[N];      //dependent
  real x[N];      //independent
  int k;          //seasonality
  real w[N];      //dummy
}

parameters{
  real mu;
  real beta;
  vector[k-1] season;
  real lambda;
  real<lower=0> sigma_obs;

}

transformed parameters{
  vector[N] all_seasons;
    
  for (t in 1:k-1)
    all_seasons[t] = season[t];
  
  for (t in k:N)
    all_seasons[t] = - sum(all_seasons[t-(k-1):t-1]);
}

model{

  for (t in 1:N)
    y[t] ~ normal(mu + all_seasons[t] + lambda * w[t] + beta * x[t], sigma_obs);
  
}

generated quantities{
  vector[N] y_hat;
  vector[N] log_lik;
  vector[N] y_plot;
  
  for(i in 1:N){
    y_plot[i] = normal_rng(mu + lambda * w[i] + beta * x[i], sigma_obs);
    y_hat[i] = normal_rng(mu + all_seasons[i] + lambda * w[i] + beta * x[i], sigma_obs);
    log_lik[i] = normal_lpdf(y[i] | mu + all_seasons[i] + lambda * w[i] + beta * x[i], sigma_obs);
    }
    
}
