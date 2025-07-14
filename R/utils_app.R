set_context <- function(
    key,
    value,
    envir = parent.frame(),
    overwrite = TRUE
) {
  checkmate::assert_string(key)

  namespace <- "qz_"
  key <- paste0(namespace, key)

  if (!overwrite && !is.null(getOption(key))) {
    rlang::abort(glue::glue(
      "Can't overwrite key {key} in context"
    ))
  }

  withr::local_options(
    .new = list(value) |> stats::setNames(key),
    .local_envir = envir
  )


  return(list(key = value))
}



get_context <- function(
    key,
    must_work = FALSE,
    default = NULL
) {
  checkmate::assert_string(key)

  namespace <- "qz_"
  key <- paste0(namespace, key)

  value <- getOption(key)

  if (is.null(value) && must_work) {
    rlang::abort(glue::glue(
      "Key {key} has not been set in context"
    ))
  }

  return(value %||% default)
}
