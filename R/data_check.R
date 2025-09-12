library(tidyverse)
library(googlesheets4)
library(metill)
theme_set(theme_metill())

# Download raw data from Google Sheets
d_raw <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1bq5DXQs1nobk0nu9cN-4UOHPkcPK3fvkTLa2t2lVNKk/edit?usp=sharing"
)
gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
# Check data structure (optional - can be removed in production)
d_raw |>
  mutate_at(
    vars(player1, player2),
    str_to_title
  ) |>
  mutate(
    match = row_number(),
    date = as_date(date),
    missing = pmap_lgl(
      list(game1, game2, game3),
      \(x, y, z) all(is.na(c(x, y, z)))
    )
  ) |>
  filter(!missing) |>
  pivot_longer(c(player1, player2), values_to = "player") |>
  distinct(
    player,
    date,
    cube,
    match
  ) |>
  count(player, date, cube, sort = TRUE) |> 
  filter(
    date == max(date)
  )
