# dependencies.R
# Installs all required runtime and development packages for the Golem package

required_packages <- c(
  "shiny",
  "bslib",
  "bsicons",
  "plotly",
  "DT",
  "dplyr",
  "tidyr",
  "lubridate",
  "ggplot2",
  "scales",
  "htmltools",
  "golem",
  "config",
  "pkgload"
)

installed <- installed.packages()[, "Package"]
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are installed.")
}
