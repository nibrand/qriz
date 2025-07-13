launch_CashGame <- function() {
  shiny::runApp(
    appDir = path_CashGame()
  )
}



path_CashGame <- function(...) {
  path_app("CashGame", ...)
}
