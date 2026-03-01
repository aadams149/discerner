#' Launch the discerner Shiny app
#'
#' Opens an interactive Shiny application for ranking items by preference.
#' Items are entered in a text area (one per line or comma-separated), then
#' sorted through pairwise "battle" comparisons presented as clickable buttons.
#'
#' @param ... Additional arguments passed to [shiny::runApp()], such as
#'   `port`, `host`, or `launch.browser`.
#'
#' @return This function does not return a value; it launches a Shiny app.
#'
#' @examples
#' if (interactive()) {
#'   run_discerner_app()
#' }
#'
#' @export
run_discerner_app <- function(...) {
  app_dir <- system.file("app", package = "discerner")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Try reinstalling `discerner`.")
  }
  shiny::runApp(app_dir, ...)
}
