#' Map a raw (named) cube to its power tier.
#'
#' Single source of truth, extracted from prepare_cube_data() so that
#' publish.R labels cubes with the exact tier the model fit under.
#' @param cube character vector of cube names (any case)
#' @return character vector of "High" / "Medium" / "Low" / "Other"
cube_tier <- function(cube) {
  cube <- stringr::str_to_lower(cube)
  dplyr::case_when(
    cube %in% c(
      "bolti", "nerva's cube", "vintage aron", "vintage ingvi",
      "vintage victor", "stingvi power max", "diddi's vintage vanilla"
    ) ~ "High",
    cube %in% c(
      "meta memories", "modern stories", "synergy cube", "horror cube",
      "final destination", "flashdance!", "inventors' fair"
    ) ~ "Medium",
    cube %in% c(
      "khans cube", "kaldheim cube", "old border cube", "pauper cube diddi",
      "pauper cube victor", "the ab wheel", "that's totally uncommon"
    ) ~ "Low",
    cube %in% c(
      "stone soup cube", "boltalandð", "genesis", "super turbo time"
    ) ~ "Other",
    TRUE ~ "Other"
  )
}
