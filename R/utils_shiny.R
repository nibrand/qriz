get_js_dependencies <- function(app, version = "0.0.0.9000", dependencies) {
  js_sources <- purrr::map(
    dependencies,
    \(e) {
      list(src = e)
    }
  )

  htmltools::htmlDependency(
    name = "qr-js",
    version = version,
    src = path_app(app),
    script = js_sources
  )
}
