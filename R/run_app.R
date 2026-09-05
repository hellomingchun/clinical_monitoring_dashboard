#' Run the Shiny Application
#'
#' @param onStart A function that will be called before the app is actually run.
#' @param options Named options that should be passed to the `runApp` call.
#' @param enableBookmarking Can be one of "url", "server", or "disable".
#' @param uiPattern A regular expression that matches the URL paths to be served by the app.
#' @param ... arguments to pass to golem_opts.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
