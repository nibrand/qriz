path_inst <- function(...) {
  fs::path_package("qriz", ...)
}



path_app <- function(app, ...) {
  path_inst("apps", app, ...)
}
