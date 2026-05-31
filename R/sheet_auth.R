suppressPackageStartupMessages(library(googlesheets4))

cube_auth_mode <- function() {
  if (nzchar(Sys.getenv("GCP_SA_JSON"))) "service_account" else "oauth"
}

cube_gs4_auth <- function(email = Sys.getenv("GOOGLE_MAIL")) {
  if (cube_auth_mode() == "service_account") {
    gs4_auth(
      path = Sys.getenv("GCP_SA_JSON"),
      scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
    )
  } else {
    gs4_auth(email = email)
  }
  invisible(TRUE)
}
