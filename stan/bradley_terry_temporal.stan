/**
 * Time-Varying Bradley-Terry Model with Cube Effects
 *
 * Combines:
 * 1. Time-varying baseline strength via OU process with attendance-based reversion
 * 2. Static cube-specific effects (player affinity for different cube types)
 *
 * Model:
 *   P(player1 wins) = logit⁻¹(α₁(t) + γ₁(c) - α₂(t) - γ₂(c))
 *
 * where:
 *   - α_k(t) evolves via OU with φ_present or φ_absent depending on attendance
 *   - γ_k(c) captures player k's relative strength in cube type c (static)
 *
 * This separates "overall skill trajectory" from "cube-specific strengths",
 * so a bad run at Vintage Cube doesn't drag down your Pauper rating.
 */
data {
  int<lower=1> N;                              // Total number of games
  int<lower=1> K;                              // Number of players
  int<lower=1> T;                              // Number of distinct game dates
  int<lower=1> C;                              // Number of cube types
  array[N] int<lower=1, upper=K> player1;      // Player 1 in each game
  array[N] int<lower=1, upper=K> player2;      // Player 2 in each game
  array[N] int<lower=0, upper=1> y;            // Outcome: 1 if player1 wins
  array[N] int<lower=1, upper=T> game_time;    // Which time point each game belongs to
  array[N] int<lower=1, upper=C> cube;         // Which cube type for each game
  vector<lower=0>[T] delta_t;                  // Time gap before each date (in weeks)
  matrix<lower=0, upper=1>[K, T] played;       // 1 if player k played at time t
}

parameters {
  // Initial player strengths (sum-to-zero)
  sum_to_zero_vector[K] alpha0;

  // Standardized innovations for random walk (unconstrained, will re-center)
  array[T - 1] vector[K] z_alpha;

  // Shared volatility (log-scale for numerical stability)
  real<lower = 0> sigma;

  // Mean-reversion parameters
  real<lower=0, upper=1> phi_present;          // Weak reversion for attendees
  real<lower=0, upper=1> phi_absent;           // Stronger reversion for absentees

  // Cube effects: player-specific affinity for each cube type
  // Sum-to-zero across cubes for each player
  array[K] sum_to_zero_vector[C] gamma;
}

transformed parameters {
  // Player strengths at each time point
  array[T] vector[K] alpha;


  // Initialize
  alpha[1] = alpha0;

  // OU evolution with attendance-dependent mean reversion
  for (t in 2:T) {
    vector[K] alpha_raw;

    for (k in 1:K) {
      // Choose phi based on whether player k played at time t
      real phi_k = played[k, t] > 0.5 ? phi_present : phi_absent;

      // OU update: mean reversion + innovation
      alpha_raw[k] = pow(phi_k, delta_t[t]) * alpha[t - 1, k]
                     + sqrt(delta_t[t]) * sigma * z_alpha[t - 1, k];
    }

    // Re-center to maintain identifiability
    alpha[t] = alpha_raw - mean(alpha_raw);
  }
}

model {
  // Prior on initial strengths (tight to avoid spurious early dispersion)
  alpha0 ~ normal(0, 0.5);

  // Standard normal prior on innovations
  for (t in 1:(T - 1)) {
    z_alpha[t] ~ std_normal();
  }

  // Tight prior on volatility (mean 0.2, favoring smooth trajectories)
  sigma ~ exponential(5);

  // Priors on mean-reversion parameters
  phi_present ~ beta(90, 1);
  phi_absent ~ beta(30, 1);

  // Prior on cube effects: small effects centered at 0
  for (k in 1:K) {
    gamma[k] ~ normal(0, 0.3);
  }

  // Likelihood with both time-varying strength and cube effects
  for (n in 1:N) {
    int t = game_time[n];
    int c = cube[n];
    real eta = alpha[t, player1[n]] + gamma[player1[n], c]
             - alpha[t, player2[n]] - gamma[player2[n], c];
    y[n] ~ bernoulli_logit(eta);
  }
}

generated quantities {
  // Log-likelihood for model comparison (LOO-CV)
  vector[N] log_lik;

  // Posterior predictive
  array[N] int y_rep;

  // Current strengths (most recent time point)
  vector[K] alpha_current = alpha[T];

  // Half-life in weeks
  real halflife_present = -log(2) / log(phi_present + 1e-10);
  real halflife_absent = -log(2) / log(phi_absent + 1e-10);

  for (n in 1:N) {
    int t = game_time[n];
    int c = cube[n];
    real eta = alpha[t, player1[n]] + gamma[player1[n], c]
             - alpha[t, player2[n]] - gamma[player2[n], c];
    log_lik[n] = bernoulli_logit_lpmf(y[n] | eta);
    y_rep[n] = bernoulli_logit_rng(eta);
  }
}
