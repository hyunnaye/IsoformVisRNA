#' Launch Shiny App for IsoformVisRNA
#'
#' A function that launches the Shiny app for IsoformVisRNA.
#' The shiny app allows the user to generate bar and box plots through the UI.
#' The code has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#'
#' IsoformVisRNA::runIsoformVisRNA()
#' }
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials. \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#' @importFrom shiny runApp

runIsoformVisRNA <- function() {
  appDir <- system.file("./inst/shiny-scripts/app.R",
                        package = "IsoformVisRNA")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}
