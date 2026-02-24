source("R/data_preparation.R")
source("R/model_fitting.R")
source("R/visualization.R")

# Prepare data
d_raw <- download_cube_results()
datasets <- prepare_cube_data_temporal(d_raw)
save_prepared_datasets(datasets)

# Compile and fit (this will take a while first time)
fit <- fit_temporal_model()

fit$summary(c("phi_present", "phi_absent", "sigma"))

n_times <- nrow(game_dates)
n_players <- nrow(players)

# Extract alpha draws: alpha[t, k]
alpha_draws <- fit$draws("alpha", format = "draws_df") |>
  as_tibble() |>
  # select(-starts_with(".")) |>
  pivot_longer(
    -starts_with("."),
    names_to = "param",
    values_to = "value"
  ) |>
  mutate(
    # Parse alpha[t,k] format
    time_idx = str_extract(param, "(?<=\\[)[0-9]+") |> as.integer(),
    player_nr = str_extract(param, "(?<=,)[0-9]+(?=\\])") |> as.integer(),
    # Convert to ELO scale
    value = bt_to_elo(value)
  )

# Summarize by player and time
alpha_summary <- alpha_draws |>
  inner_join(players |> select(player_nr, player), by = "player_nr") |>
  inner_join(game_dates |> select(time_idx, date), by = "time_idx") |>
  summarise(
    median = median(value),
    lower90 = quantile(value, 0.05),
    upper90 = quantile(value, 0.95),
    lower50 = quantile(value, 0.25),
    upper50 = quantile(value, 0.75),
    .by = c(player, date)
  )


alpha_summary |>
  filter(
    player %in% c(
      "Örvar", 
      "Arnar",
      "Binni", 
      "Tommi", 
      "Diddi", 
      "Hjalti", 
      "Aron Gauti",
      "Oddur",
      "Aron Freyr"
    )) |>
  filter(
    # date >= clock::date_build(2025, 11)
  ) |> 
  ggplot(aes(date, median, col = player)) +
  geom_hline(yintercept = 1500, lty = 2, alpha = 0.3, linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  scale_x_date(
    date_breaks = "1 months",
    date_labels = "%b",
    guide = guide_axis(cap = "both")
  ) +
  scale_y_continuous(guide = guide_axis(cap = "both")) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  labs(
    title = "Þróun í mati á styrk leikmanna",
    x = NULL,
    y = "ELO stig"
  )
