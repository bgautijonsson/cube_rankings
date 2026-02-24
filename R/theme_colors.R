# MTG Mana Color Palette & ggplot2 Theme
# Canonical color definitions for all cube_rankings and mtgkubbur visualizations.

library(ggplot2)

# --- Mana colors ---
mtg_colors <- list(

  # Core mana
  blue       = "#0e68ab",
  green      = "#00733e",
  red        = "#d3202a",
  black      = "#150b00",
  white      = "#f9faf4",
  gold       = "#8b6914",

  # Derived

blue_light = "#e8f1f8",
  blue_dark  = "#0a4f82",
  green_light = "#e6f2ec",
  red_light  = "#fce8e9",

  # Warm neutrals
  text_primary = "#2a1f14",
  text_body    = "#2a1f14",
  text_muted   = "#5c4f42",
  border       = "#d4cfc9",
  grid         = "#e8e3de",
  bg_subtle    = "#f9faf4",
  bg_page      = "#fefefe",

  # Chart palette (8 distinct series colors)
  chart_palette = c(
    "#0e68ab",
    "#d3202a",
    "#00733e",
    "#8b6914",
    "#7b4fa0",
    "#a0673c",
    "#5ba3cf",
    "#e07070"
  )
)

#' MTG Kubbur ggplot2 theme
#'
#' Replaces theme_metill(). Built on theme_minimal with Inter font,
#' warm grid lines, blue-tinted facet strips, and MTG-flavored colors.
theme_mtgkubbur <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "Inter") %+replace%
    theme(
      # Text
      plot.title       = element_text(
        color = mtg_colors$black, face = "bold",
        size = rel(1.15), margin = margin(b = 4)
      ),
      plot.subtitle    = element_text(
        color = mtg_colors$text_muted,
        size = rel(0.85), margin = margin(b = 10)
      ),
      plot.caption     = element_text(
        color = mtg_colors$text_muted,
        size = rel(0.7), hjust = 0
      ),
      axis.title       = element_text(color = mtg_colors$text_muted, size = rel(0.85)),
      axis.text        = element_text(color = mtg_colors$text_body, size = rel(0.8)),

      # Grid
      panel.grid.major = element_line(color = mtg_colors$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),

      # Background
      plot.background  = element_rect(fill = mtg_colors$bg_page, color = NA),
      panel.background = element_rect(fill = mtg_colors$bg_page, color = NA),

      # Facet strips
      strip.background = element_rect(fill = mtg_colors$blue_light, color = NA),
      strip.text       = element_text(
        color = mtg_colors$text_primary, face = "bold", size = rel(0.85)
      ),

      # Legend
      legend.text      = element_text(color = mtg_colors$text_body, size = rel(0.8)),
      legend.title     = element_text(color = mtg_colors$text_primary, size = rel(0.85)),

      # Margins
      plot.margin      = margin(10, 10, 10, 10)
    )
}

#' MTG manual colour scale for multi-series charts
scale_colour_mtg <- function(...) {
  scale_colour_manual(values = mtg_colors$chart_palette, ...)
}

#' MTG manual fill scale for multi-series charts
scale_fill_mtg <- function(...) {
  scale_fill_manual(values = mtg_colors$chart_palette, ...)
}
