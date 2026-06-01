options(width = 120)
suppressPackageStartupMessages({ library(tidyverse) })
source("R/cube_tier.R")

stopifnot(cube_tier("Bolti") == "High")
stopifnot(cube_tier("VINTAGE ARON") == "High")
stopifnot(cube_tier("inventors' fair") == "Medium")
stopifnot(cube_tier("Horror Cube") == "Medium")
stopifnot(cube_tier("Pauper Cube Diddi") == "Low")
stopifnot(cube_tier("the ab wheel") == "Low")
stopifnot(cube_tier("boltaland\u00f0") == "Other")
stopifnot(cube_tier("Some Cube That Does Not Exist") == "Other")
stopifnot(identical(
  cube_tier(c("Bolti", "Synergy Cube", "Khans Cube", "Genesis")),
  c("High", "Medium", "Low", "Other")
))
cat("PASS: cube_tier maps the case_when exactly + vectorises\n")
