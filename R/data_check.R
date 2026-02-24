library(tidyverse)
library(googlesheets4)
source("R/theme_colors.R")
theme_set(theme_mtgkubbur())

# Download raw data from Google Sheets
gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
d_raw <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1bq5DXQs1nobk0nu9cN-4UOHPkcPK3fvkTLa2t2lVNKk/edit?usp=sharing"
)
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
  distinct(player, date) |> 
  count(player, sort = TRUE) |> 
  View()



cube_calendar <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1bq5DXQs1nobk0nu9cN-4UOHPkcPK3fvkTLa2t2lVNKk/edit?usp=sharing",
  sheet = "Cube calander"
) |> 
  janitor::clean_names()

cube_calendar |> 
  mutate(
    cube = str_to_lower(cube),
    cube = if_else(
      str_detect(cube, "vintage"), 
      "vintage", 
      cube
    )
  ) |> 
  distinct(cube, date) |> 
  count(cube) |> 
  arrange(cube)

cube_calendar |> 
  mutate(cube = str_to_lower(cube)) |> 
  distinct(cube, date) |> 
  mutate(
    y = as.numeric(as.factor(cube))
  ) |> 
  ggplot(aes(date, y)) +
  geom_label(aes(label = cube))
