library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggtext)
library(scales)
source("R/theme_colors.R")
theme_set(theme_mtgkubbur())

render_visualizations <- function(
  fit,
  players,
  processed_data,
  output_dir = "plots"
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  plot1 <- fit$draws("alpha") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-starts_with("."))) |>
    mutate(name = parse_number(name)) |>
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
      .by = c(player, text)
    ) |>
    mutate(
      player = glue::glue("{str_to_title(player)} | {text}") |>
        fct_reorder(median)
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
    geom_text(
      data = tibble(x = 1),
      inherit.aes = FALSE,
      x = -2,
      y = -Inf,
      label = "\u2190 L\u00e6gri mat \u00e1 styrk",
      hjust = 0,
      vjust = 0.8,
      size = 4.5,
      colour = "#5c4f42"
    ) +
    geom_text(
      data = tibble(x = 1),
      inherit.aes = FALSE,
      x = 2,
      y = -Inf,
      label = "H\u00e6rri mat \u00e1 styrk \u2192",
      hjust = 1,
      vjust = 0.8,
      size = 4.5,
      colour = "#5c4f42"
    ) +
    scale_alpha_continuous(
      range = c(0, 0.3),
      guide = guide_none()
    ) +
    scale_x_continuous(
      guide = guide_axis(cap = "both"),
      breaks = 0,
      labels = "Me<U+00F0>altal allra leikmanna"
    ) +
    scale_y_discrete(
      guide = guide_axis(cap = "both")
    ) +
    coord_cartesian(clip = "off", xlim = 2.5 * c(-1, 1)) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(
      title = "Mat <U+00E1> styrk leikmanna",
      subtitle = "L<U+00F3><U+00F0>r<U+00E9>tt strik eru mi<U+00F0>gildi matsins og kassar <U+00F3>vissubil",
      x = NULL,
      y = NULL
    )

  plot2 <- fit$draws("alpha") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-starts_with("."))) |>
    mutate(name = parse_number(name)) |>
    inner_join(
      players,
      by = join_by(name == player_nr)
    ) |>
    arrange(desc(value)) |>
    mutate(
      position = row_number(),
      .by = .draw
    ) |>
    count(position, player, text) |>
    mutate(
      p = n / sum(n),
      mean = sum(p * position),
      .by = player
    ) |>
    mutate(
      player = glue::glue("{str_to_title(player)} | {text}") |>
        fct_reorder(-mean, .fun = max)
    ) |>
    complete(player, position, fill = list(p = 0)) |>
    ggplot(aes(position, player)) +
    geom_tile(aes(fill = p)) +
    geom_text(
      aes(label = percent(p, accuracy = 1), col = p)
    ) +
    scale_colour_gradient2(
      low = "#0e68ab",
      mid = "#0a4f82",
      high = "#f9faf4",
      midpoint = 0.17
    ) +
    scale_fill_gradient(
      low = "#f9faf4",
      high = "#0e68ab"
    ) +
    scale_x_continuous(
      breaks = breaks_width(1),
      guide = guide_axis(cap = "both"),
      expand = c(0, 0)
    ) +
    scale_y_discrete(
      guide = guide_axis(cap = "both"),
      expand = c(0, 0)
    ) +
    theme(legend.position = "none") +
    labs(
      x = "S<U+00E6>ti <U+00ED> r<U+00F6><U+00F0>",
      y = NULL,
      title = "Hversu l<U+00ED>klegt er a<U+00F0> leikma<U+00F0>ur eigi heima <U+00ED> <U+00E1>kve<U+00F0>nu s<U+00E6>ti ra<U+00F0>a<U+00F0> eftir styrk?",
      subtitle = "<U+00DE>v<U+00ED> fylgir <U+00F3>vissa a<U+00F0> ra<U+00F0>a f<U+00F3>lki eftir styrk og <U+00FE>v<U+00ED> g<U+00E6>ti einstaklingur <U+00E1>tt heima <U+00ED> m<U+00F6>rgum mismunandi s<U+00E6>tum"
    )

  plot_dat <- fit$draws("gamma") |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(c(-starts_with("."))) |>
    mutate(
      player_nr = str_match(name, "([0-9]+),")[, 2] |> parse_number(),
      cube_nr = str_match(name, ",([0-9]+)")[, 2] |> parse_number()
    ) |>
    inner_join(players) |>
    inner_join(
      processed_data |>
        distinct(cube, cube_nr)
    )

  plot3 <- plot_dat |>
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
      .by = c(player, cube)
    ) |>
    mutate(
      cube = str_to_title(cube) |>
        fct_relevel("High", "Medium", "Low")
    ) |>
    filter(
      player %in% c(
        "Diddi",
        "Tommi",
        "Hjalti",
        "Binni"
        # "Aron Freyr",
        # "<U+00D6>rvar",
        # "Aron Gauti",
        # "Aron Elvar",
        # "Aron <U+00CD>vars",
        # "Einar",
        # "Dan<U+00ED>el",
        # "Oddur"
      )
    ) |>
    ggplot(aes(median, cube)) +
    geom_vline(
      xintercept = 0,
      lty = 2,
      linewidth = 0.3,
      alpha = 0.3
    ) +
    geom_point(
      shape = "|",
      size = 5
    ) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        yend = cube,
        alpha = -coverage
      ),
      linewidth = 3
    ) +
    scale_alpha_continuous(
      range = c(0, 0.3),
      guide = guide_none()
    ) +
    scale_x_continuous(
      guide = guide_axis(cap = "both"),
      breaks = 0,
      labels = "Me<U+00F0>algeta <U+00ED> <U+00F6>llum kubbum"
    ) +
    scale_y_discrete(
      guide = guide_axis(cap = "both")
    ) +
    facet_wrap("player", ncol = 2) +
    coord_cartesian(clip = "off") +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(
      title = "Mat <U+00E1> styrk leikmanna eftir kubb",
      subtitle = "L<U+00F3><U+00F0>r<U+00E9>tt strik eru mi<U+00F0>gildi matsins og kassar <U+00F3>vissubil",
      x = NULL,
      y = NULL,
      caption = "Other = Kubbar sem breyta reglum e<U+00F0>a eru <U+00F3>fyrirsj<U+00E1>anlegir. T.d. Genesis Cube, Turbo Cube, Stone Soup Cube"
    )

  ggsave(
    file.path(output_dir, "player_strength.png"),
    plot1,
    width = 10,
    height = 8,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, "ranking_probability.png"),
    plot2,
    width = 10,
    height = 8,
    scale = 1.2,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, "cube_effects.png"),
    plot3,
    width = 10,
    height = 8,
    dpi = 300
  )

  cat("Visualization complete. Plots saved to", output_dir, "\n")

  list(
    player_strength = plot1,
    ranking_probability = plot2,
    cube_effects = plot3
  )
}

run_visualization <- function(
  fit_path = "data/fitted_model.rds",
  players_path = "data/players.rds",
  processed_path = "data/processed_data.rds",
  output_dir = "plots"
) {
  fit <- readRDS(fit_path)
  players <- readRDS(players_path)
  processed_data <- readRDS(processed_path)

  render_visualizations(
    fit = fit,
    players = players,
    processed_data = processed_data,
    output_dir = output_dir
  )
}

# Convert Bradley-Terry log-odds to ELO scale (centered at 1500)
bt_to_elo <- function(x) {
  (400 / log(10)) * x + 1500
}

#' Visualize temporal evolution of player strengths
#'
#' Creates a faceted plot showing how each player's estimated strength
#' has evolved over time, with credible intervals. Strengths are shown
#' on a 0-100 scale where the weakest player at each time point gets 0
#' and the strongest gets 100 (normalized within each posterior draw).
#'
#' @param fit Fitted temporal Stan model
#' @param players Player metadata tibble
#' @param game_dates Game dates tibble with date and time_idx
#' @param top_n Number of top players to show (by current strength)
#' @param output_dir Directory to save plots
render_temporal_evolution <- function(
  fit,
  players,
  game_dates,
  top_n = 12,
  output_dir = "plots"
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  n_times <- nrow(game_dates)
  n_players <- nrow(players)

  # Extract alpha draws: alpha[t, k]
  alpha_draws <- fit$draws("alpha", format = "draws_df") |>
    as_tibble() |>
    pivot_longer(
      -starts_with("."),
      names_to = "param",
      values_to = "alpha"
    ) |>
    mutate(
      # Parse alpha[t,k] format
      time_idx = str_extract(param, "(?<=\\[)[0-9]+") |> as.integer(),
      player_nr = str_extract(param, "(?<=,)[0-9]+(?=\\])") |> as.integer()
    ) |>
    # Normalize to 0-100 within each draw and time point
    mutate(
      alpha_min = min(alpha),
      alpha_max = max(alpha),
      score = (alpha - alpha_min) / (alpha_max - alpha_min) * 100,
      .by = c(.draw, time_idx)
    )

  # Summarize by player and time
  alpha_summary <- alpha_draws |>
    inner_join(players |> select(player_nr, player), by = "player_nr") |>
    inner_join(game_dates |> select(time_idx, date), by = "time_idx") |>
    summarise(
      median = median(score),
      lower90 = quantile(score, 0.05),
      upper90 = quantile(score, 0.95),
      lower50 = quantile(score, 0.25),
      upper50 = quantile(score, 0.75),
      .by = c(player, date)
    )

  # Get current rankings to select top players
  current_rankings <- alpha_summary |>
    filter(date == max(date)) |>
    arrange(desc(median)) |>
    slice_head(n = top_n) |>
    pull(player)

  plot_data <- alpha_summary |>
    filter(player %in% current_rankings) |>
    mutate(player = factor(player, levels = current_rankings))

  # Combined line plot
  plot_lines <- plot_data |>
    ggplot(aes(date, median, col = player)) +
    geom_hline(yintercept = 50, lty = 2, alpha = 0.3, linewidth = 0.3) +
    geom_line(linewidth = 0.8) +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b",
      guide = guide_axis(cap = "both")
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      guide = guide_axis(cap = "both")
    ) +
    scale_colour_mtg() +
    labs(
      title = "<U+00DE>r<U+00F3>un <U+00ED> mati <U+00E1> styrk leikmanna",
      subtitle = "Skor fr<U+00E1> 0 (veikastur) til 100 (sterkastur) <U+00E1> hverjum t<U+00ED>mapunkti.",
      x = NULL,
      y = "Skor",
      col = NULL
    )

  # Main evolution plot (faceted)
  plot_evolution <- plot_data |>
    ggplot(aes(date, median)) +
    geom_hline(yintercept = 50, lty = 2, alpha = 0.3, linewidth = 0.3) +
    geom_ribbon(aes(ymin = lower90, ymax = upper90), fill = "#0e68ab", alpha = 0.2) +
    geom_ribbon(aes(ymin = lower50, ymax = upper50), fill = "#0e68ab", alpha = 0.3) +
    geom_line(linewidth = 0.8) +
    geom_point(
      data = ~ filter(.x, date == max(date)),
      size = 2
    ) +
    facet_wrap(~player, ncol = 3) +
    scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b",
      guide = guide_axis(cap = "both")
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      guide = guide_axis(cap = "both")
    ) +
    labs(
      title = "<U+00DE>r<U+00F3>un <U+00ED> mati <U+00E1> styrk leikmanna",
      subtitle = "Skygg<U+00F0> sv<U+00E6><U+00F0>i s<U+00FD>na 50% og 90% <U+00F3>vissubil. Skor: 0 = veikastur, 100 = sterkastur.",
      x = NULL,
      y = "Skor"
    )

  # Volatility diagnostic
  sigma_draws <- fit$draws("sigma", format = "draws_df") |>
    as_tibble() |>
    pull(sigma)

  plot_sigma <- tibble(sigma = sigma_draws) |>
    ggplot(aes(sigma)) +
    geom_histogram(bins = 50, fill = "#0e68ab", alpha = 0.7) +
    geom_vline(
      xintercept = median(sigma_draws),
      lty = 2,
      color = "#d3202a"
    ) +
    scale_x_continuous(guide = guide_axis(cap = "both")) +
    scale_y_continuous(guide = guide_axis(cap = "both")) +
    labs(
      title = "Dreifing <U+00E1> <U+03C3> (sveiflur <U+00ED> styrk)",
      subtitle = glue::glue("Mi<U+00F0>gildi: {round(median(sigma_draws), 3)}"),
      x = "<U+03C3>",
      y = "Fj<U+00F6>ldi"
    )

  ggsave(
    file.path(output_dir, "temporal_evolution.png"),
    plot_evolution,
    width = 12,
    height = 10,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, "temporal_lines.png"),
    plot_lines,
    width = 10,
    height = 6,
    dpi = 300
  )

  ggsave(
    file.path(output_dir, "volatility_posterior.png"),
    plot_sigma,
    width = 6,
    height = 4,
    dpi = 300
  )

  cat("Temporal visualization complete. Plots saved to", output_dir, "\n")

  list(
    evolution = plot_evolution,
    lines = plot_lines,
    volatility = plot_sigma
  )
}

run_temporal_visualization <- function(
  fit_path = "data/fitted_model_temporal.rds",
  players_path = "data/players.rds",
  game_dates_path = "data/game_dates.rds",
  top_n = 12,
  output_dir = "plots"
) {
  fit <- readRDS(fit_path)
  players <- readRDS(players_path)
  game_dates <- readRDS(game_dates_path)

  render_temporal_evolution(
    fit = fit,
    players = players,
    game_dates = game_dates,
    top_n = top_n,
    output_dir = output_dir
  )
}
