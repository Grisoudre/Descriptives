#' @export
Descri <- function() {
  appDir <- system.file("AppDescri", "myapp", package = "Descriptives")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Descriptives`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
