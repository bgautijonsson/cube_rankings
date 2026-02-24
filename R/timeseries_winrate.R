processed_data |>
  pivot_longer(c(player1, player2), values_to = "player") |>
  mutate(win = as.integer(winner == player)) |>
  summarise(
    wins = sum(win),
    losses = sum(1 - win),
    total = wins + losses,
    .by = c(player, date)
  ) |> 
  mutate(
    wins = slider::slide_dbl(wins, sum, .before = 3),
    losses = slider::slide_dbl(losses, sum, .before = 3),
    .by = player
  ) |> 
  mutate(
    total = wins + losses,
    ratio = wins / total
  ) |> 
  filter(
    player %in% c(
      "Arnar",
      "Binni",
      "Aron Elvar",
      "Diddi",
      "Hjalti",
      "Tommi",
      "Aron Freyr",
      "Örvar",
      "Einar",
      "Ingvi"
    )
  ) |> 
  ggplot(aes(date, ratio)) +
  geom_line(
    data = ~rename(.x, pl = player),
    aes(group = pl),
    alpha = 0.2,
    linewidth = 0.4
  ) +
  geom_line(
    aes(group = player),
    linewidth = 1
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both")
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = label_percent(),
    guide = guide_axis(cap = "both")
  ) +
  facet_wrap("player") +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun í winrate",
    subtitle = "Winrate reiknað yfir síðustu fjögur cube kvöld leikmanns"
  )
