d <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1bq5DXQs1nobk0nu9cN-4UOHPkcPK3fvkTLa2t2lVNKk/edit?gid=636250879#gid=636250879",
  sheet = 1
)

source("R/theme_colors.R")
theme_set(theme_mtgkubbur())

d |> 
  select(date, cube, player1, player2) |> 
  pivot_longer(c(-date, -cube)) |> 
  distinct(date, cube, value) |> 
  summarise(
    players = n(),
    cube = str_c(unique(cube), collapse = "\n"),
    .by = c(date)
  ) |> 
  mutate(
    date = as_date(date)
  ) |> 
  filter(players < 20) |> 
  ggplot() +
  geom_hline(
    data = ~summarise(.x, mean = mean(players)),
    aes(yintercept = mean),
    lty = 2,
    alpha = 0.4,
    linewidth = 0.3
  ) +
  geom_hline(
    data = ~filter(.x, players < 20) |> 
      summarise(mean = mean(players)),
    aes(yintercept = mean),
    lty = 2,
    alpha = 0.4,
    linewidth = 0.3
  ) +
  geom_hline(
    yintercept = c(7,  13),
    alpha = 0.3, 
    linewidth = 0.1
  ) +
  geom_line(
    aes(date, players),
    linewidth = 1
  ) +
  geom_label(
    aes(date, players, label = cube),
    size = 3
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both")
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    limits = c(0, NA),
    breaks = c(0, 7, 10, 11, 14, 20)
  ) +
  labs(
    x = "Dagsetning",
    y = "Fjöldi",
    title = "Fjöldi leikmanna á cube kvöldum",
    subtitle = "Mesti fjöldi (utan IceCube) er 13, lægsti er 7"
  )


d |> 
  select(date, player1, player2) |> 
  pivot_longer(c(-date)) |> 
  # distinct(date, value) |> 
  summarise(
    players = n(),
    .by = date
  ) |> 
  mutate(
    date = as_date(date)
  ) |> 
  filter(players > 100) 
  ggplot() +
  geom_hline(
    data = ~summarise(.x, mean = mean(players)),
    aes(yintercept = mean),
    lty = 2,
    alpha = 0.4,
    linewidth = 0.3
  ) +
  geom_hline(
    data = ~filter(.x, players < 20) |> 
      summarise(mean = mean(players)),
    aes(yintercept = mean),
    lty = 2,
    alpha = 0.4,
    linewidth = 0.3
  ) +
  geom_hline(
    yintercept = c(20, 38),
    alpha = 0.3, 
    linewidth = 0.1
  ) +
  geom_line(
    aes(date, players),
    linewidth = 1
  ) +
  geom_point(
    aes(date, players),
    size = 3
  ) +
  scale_x_date(
    guide = guide_axis(cap = "both")
  ) +
  scale_y_continuous(
    guide = guide_axis(cap = "both"),
    limits = c(0, NA),
    breaks = c(0, 20, 38, 28.5)
  ) +
  labs(
    x = "Dagsetning",
    y = "Fjöldi",
    title = "Fjöldi leikja á cube kvöldum",
    subtitle = "Mesti fjöldi (utan IceCube) er 38, minnsti er 20"
  )
