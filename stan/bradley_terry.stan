data {
  int<lower=1> N;              // number of comparisons
  int<lower=1> K;              // number of items/players
  array[N] int<lower=1, upper=K> player1;  // first player in each comparison
  array[N] int<lower=1, upper=K> player2;  // second player in each comparison
  array[N] int<lower=0, upper=1> y;        // outcome: 1 if player1 wins, 0 if player2 wins
}

parameters {
  sum_to_zero_vector[K] alpha;
}


model {
  // Priors
  alpha ~ normal(0, 1);
  
  // Likelihood
  for (n in 1:N) {
    y[n] ~ bernoulli_logit(alpha[player1[n]] - alpha[player2[n]]);
  }
}

generated quantities {
  vector[N] log_lik;           // log-likelihood for each observation
  vector[N] y_rep;             // posterior predictive samples
  
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha[player1[n]] - alpha[player2[n]]);
    y_rep[n] = bernoulli_logit_rng(alpha[player1[n]] - alpha[player2[n]]);
  }
}
