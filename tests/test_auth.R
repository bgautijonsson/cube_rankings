options(width = 120)
source("R/sheet_auth.R")

withr::with_envvar(c(GCP_SA_JSON = "/some/key.json"), {
  stopifnot(cube_auth_mode() == "service_account")
})
withr::with_envvar(c(GCP_SA_JSON = ""), {
  stopifnot(cube_auth_mode() == "oauth")
})
cat("PASS: auth mode selection\n")
