library(cmdstanr)
library(posterior)

# Load the prepared data
stan_data <- readRDS("data/stan_data.rds")

# Compile the Stan model
model <- cmdstan_model("stan/bradley_terry.stan")

# Fit the model
fit <- model$sample(
  data = stan_data,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 3000,
  parallel_chains = 4
)

# Save the fitted model
fit$save_object(file = "data/fitted_model.rds")

cat("Model fitting complete. Saved fitted_model.rds\n")
cat("Model summary:\n")
print(fit$summary())
