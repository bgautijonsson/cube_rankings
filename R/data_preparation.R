library(tidyverse)
library(googlesheets4)
library(metill)
theme_set(theme_metill())

# Set up Google Sheets authentication using email from .Renviron
gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
# Download raw data from Google Sheets
d_raw <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1bq5DXQs1nobk0nu9cN-4UOHPkcPK3fvkTLa2t2lVNKk/edit?usp=sharing"
)

# Process the data
d <- d_raw |>
  mutate(
    date = as_date(date)
  ) |>
  pivot_longer(c(game1:game3), names_to = "game", values_to = "winner") |>
  drop_na() |>
  mutate(
    game = parse_number(game),
    result = 1 * (winner == player1)
  ) |>
  mutate_at(
    vars(winner, player1, player2),
    str_to_title
  )

# Create players lookup table
players <- d |>
  pivot_longer(c(player1, player2), values_to = "player") |>
  distinct(player) |>
  mutate(
    player_nr = row_number()
  )

# Add player numbers to the main dataset
d <- d |>
  inner_join(
    players |>
      rename(player_nr1 = player_nr),
    by = join_by(player1 == player)
  ) |>
  inner_join(
    players |>
      rename(player_nr2 = player_nr),
    by = join_by(player2 == player)
  )

# Create Stan data list
stan_data <- list(
  N = nrow(d),
  K = nrow(players),
  player1 = d$player_nr1,
  player2 = d$player_nr2,
  y = d$result
)

# Save data for use in other scripts
saveRDS(stan_data, "data/stan_data.rds")
saveRDS(players, "data/players.rds")
saveRDS(d, "data/processed_data.rds")

cat("Data preparation complete. Saved:\n")
cat("- stan_data.rds\n")
cat("- players.rds\n")
cat("- processed_data.rds\n")
