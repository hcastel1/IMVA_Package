


#' @export
runIMVA <- function() {
  appDir <- system.file("shiny", "myapp", package = "IMVA")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `IMVA`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}