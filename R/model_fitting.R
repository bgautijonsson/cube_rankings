library(cmdstanr)
library(posterior)

fit_cube_model <- function(
  stan_data,
  model_path = "stan/bradley_terry_cubeeffects.stan",
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 3000,
  parallel_chains = min(chains, 4),
  refresh = NULL,
  save_path = NULL
) {
  model <- cmdstan_model(model_path)
  fit <- model$sample(
    data = stan_data,
    chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    parallel_chains = parallel_chains,
    refresh = refresh
  )

  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    fit$save_object(file = save_path)
  }

  fit
}

run_model_fitting <- function(
  data_path = "data/stan_data.rds",
  save_path = "data/fitted_model.rds",
  model_path = "stan/bradley_terry_cubeeffects.stan",
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 3000,
  parallel_chains = min(chains, 4),
  refresh = NULL
) {
  stan_data <- readRDS(data_path)
  fit <- fit_cube_model(
    stan_data = stan_data,
    model_path = model_path,
    chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    parallel_chains = parallel_chains,
    refresh = refresh,
    save_path = save_path
  )

  cat("Model fitting complete. Saved", save_path, "\n")
  fit
}

#' Fit temporal Bradley-Terry model
#'
#' Convenience wrapper for the time-varying model with attendance-based
#' mean reversion. Players who show up maintain their rating; absent
#' players regress toward the mean.
#'
#' @param data_path Path to stan_data_temporal.rds (must include `played` matrix)
#' @param save_path Path to save fitted model
#' @param model_path Path to Stan model file
#' @param chains Number of MCMC chains
#' @param iter_warmup Number of warmup iterations
#' @param iter_sampling Number of sampling iterations
#' @param parallel_chains Number of chains to run in parallel
#' @param adapt_delta Target acceptance rate (increase if divergences)
#' @param refresh How often to print progress (NULL for default)
#' @return Fitted CmdStanMCMC object
fit_temporal_model <- function(
  data_path = "data/stan_data_temporal.rds",
  save_path = "data/fitted_model_temporal.rds",
  model_path = "stan/bradley_terry_temporal.stan",
  chains = 4,
  iter_warmup = 1500,
  iter_sampling = 3000,
  parallel_chains = min(chains, 4),
  adapt_delta = 0.9,
  refresh = NULL
) {
  stan_data <- readRDS(data_path)

  # Verify played matrix is present

  if (is.null(stan_data$played)) {
    stop("stan_data must include 'played' matrix. Re-run prepare_cube_data_temporal().")
  }

  model <- cmdstan_model(model_path)

  fit <- model$sample(
    data = stan_data,
    chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    parallel_chains = parallel_chains,
    adapt_delta = adapt_delta,
    refresh = refresh
  )

  if (!is.null(save_path)) {
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    fit$save_object(file = save_path)
  }

  cat("Temporal model fitting complete. Saved", save_path, "\n")
  fit
}
