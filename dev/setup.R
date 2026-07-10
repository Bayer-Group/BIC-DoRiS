# Setup packages/renv

# Depends
# usethis::use_package("R", type = "Depends", min_version = "4.1")

# Imports
usethis::use_package("shiny", type = "Imports")
usethis::use_package("shinyWidgets", type = "Imports")
usethis::use_package("bslib", type = "Imports")
usethis::use_package("shinyBS", type = "Imports")
usethis::use_package("DT", type = "Imports")
usethis::use_package("tidyr", type = "Imports")
usethis::use_package("dplyr", type = "Imports")


# Suggests
# usethis::use_package("cli", type = "Suggests")
# usethis::use_package("devtools", type = "Suggests")
# usethis::use_package("knitr", type = "Suggests")
# usethis::use_package("pak", type = "Suggests")
# usethis::use_package("spelling", type = "Suggests")
# usethis::use_package("testthat", type = "Suggests", min_version = "3.0.0")
# usethis::use_package("usethis", type = "Suggests")

# Renv setup
# renv::init()

# Snapshot
# renv::status()
# renv::update(lock = TRUE)
# renv::snapshot(type = "explicit", dev = TRUE)

# spelling::update_wordlist()

# rsconnect::writeManifest()

# usethis::use_version("dev")

# Add air GitHub actions
usethis::use_github_action()
usethis::use_github_action(
  url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-check.yaml"
)
usethis::use_github_action(
  url = "https://github.com/etiennebacher/setup-jarl/blob/main/examples/jarl-check.yml"
)
