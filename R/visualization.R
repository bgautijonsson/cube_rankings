library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(metill)
library(ggtext)
library(scales)
theme_set(theme_metill())

# Load the fitted model and data
fit <- readRDS("data/fitted_model.rds")
players <- readRDS("data/players.rds")

# Plot 1: Player strength estimates with uncertainty intervals
plot1 <- fit$draws("alpha") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-starts_with("."))) |>
  mutate(
    name = parse_number(name)
  ) |>
  inner_join(
    players,
    by = join_by(name == player_nr)
  ) |>
  reframe(
    median = median(value),
    coverage = c(
      0.025,
      0.05,
      0.1,
      0.2,
      0.3,
      0.4,
      0.5,
      0.6,
      0.7,
      0.8,
      0.9,
      0.95,
      0.975
    ),
    lower = quantile(value, 0.5 - coverage / 2),
    upper = quantile(value, 0.5 + coverage / 2),
    .by = c(player)
  ) |>
  mutate(
    player = str_to_title(player) |> fct_reorder(median)
  ) |>
  ggplot(aes(median, player)) +
  geom_vline(
    xintercept = 0,
    lty = 2,
    linewidth = 0.3,
    alpha = 0.3
  ) +
  geom_hline(
    yintercept = seq(1, nrow(players), 2),
    linewidth = 7,
    alpha = 0.03
  ) +
  geom_point(
    shape = "|",
    size = 5
  ) +
  geom_segment(
    aes(
      x = lower,
      xend = upper,
      yend = player,
      alpha = -coverage
    ),
    linewidth = 3
  ) +
  geom_richtext(
    data = data.frame(x = 1),
    inherit.aes = FALSE,
    x = -2,
    y = -Inf,
    label.colour = NA,
    fill = NA,
    label = "&larr; Lægri mat á styrk",
    hjust = 0,
    vjust = 0,
    size = 4.5,
    colour = "grey40"
  ) +
  geom_richtext(
    data = data.frame(x = 1),
    inherit.aes = FALSE,
    x = 2,
    y = -Inf,
    label.colour = NA,
    fill = NA,
    label = "Hærri mat á styrk &rarr;",
    hjust = 1,
    vjust = 0,
    size = 4.5,
    colour = "grey40"
  ) +
  scale_alpha_continuous(
    range = c(0, 0.3),
    guide = guide_none()
  ) +
  scale_x_continuous(
    guide = guide_axis(cap = "both"),
    breaks = 0,
    labels = "Meðaltal allra leikmanna"
  ) +
  scale_y_discrete(
    guide = guide_axis(cap = "both")
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 5)
  ) +
  labs(
    title = "Mat á styrk leikmanna",
    subtitle = "Lóðrétt strik eru miðgildi matsins og kassar óvissubil",
    x = NULL,
    y = NULL
  )

# Plot 2: Ranking probability heatmap
plot2 <- fit$draws("alpha") |>
  as_draws_df() |>
  as_tibble() |>
  pivot_longer(c(-starts_with("."))) |>
  mutate(
    name = parse_number(name)
  ) |>
  inner_join(
    players,
    by = join_by(name == player_nr)
  ) |>
  arrange(desc(value)) |>
  mutate(
    position = row_number(),
    .by = .draw
  ) |>
  count(position, player) |>
  mutate(
    p = n / sum(n),
    .by = player
  ) |>
  mutate(
    player = fct_reorder(player, p * (position == 1), .fun = max)
  ) |>
  ggplot(aes(position, player)) +
  geom_tile(aes(fill = p)) +
  geom_text(
    aes(label = percent(p, accuracy = 1), col = p)
  ) +
  scale_colour_gradient2(
    low = "black",
    mid = "grey10",
    high = "#fdfcfc",
    midpoint = 0.2
  ) +
  scale_fill_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  scale_x_continuous(
    breaks = 1:12,
    guide = guide_axis(cap = "both"),
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    guide = guide_axis(cap = "both"),
    expand = c(0, 0)
  ) +
  theme(legend.position = "none") +
  labs(
    x = "Sæti í röð",
    y = NULL,
    title = "Hversu líklegt er að leikmaður eigi heima í ákveðnu sæti raðað eftir styrk?",
    subtitle = "Því fylgir óvissa að raða fólki eftir styrk og því gæti einstaklingur átt heima í mörgum mismunandi sætum"
  )

# Save the plots
ggsave(
  "plots/player_strength.png",
  plot1,
  width = 10,
  height = 8,
  dpi = 300
)
ggsave(
  "plots/ranking_probability.png",
  plot2,
  width = 10,
  height = 8,
  dpi = 300
)

cat("Visualization complete. Plots saved to plots/ directory.\n")
