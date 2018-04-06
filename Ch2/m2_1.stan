//level is deterministic

data{
  int<lower=0> N; //number of data points
  real y[N];      //observations
}

parameters{
  real mu;                  //static level
  real<lower=0> sigma_obs;  //static width
}

model{
  y ~ normal(mu, sigma_obs); 
}

generated quantities{
  vector[N] log_lik;  //capture for LOO/WAIC
  vector[N] y_hat;    //model generated observations
  
    for (i in 1:N){
      log_lik[i] = normal_lpdf(y[i] | mu, sigma_obs);
      y_hat[i] = normal_rng(mu, sigma_obs);
    }  
}
