#' Access files in the current app
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the package.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "clinicalmonitoringdashboard")
}

#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. Defaults to system environment
#'   variable, or "default" if not set.
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the configuration file
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
  file = app_sys("golem-config.yaml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
