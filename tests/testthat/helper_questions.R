mock_QuestionMultipleChoice_list <- function(
    n,
    categories = NULL
) {
  purrr::map(
    seq_len(n),
    \(i) {
      mock_QuestionMultipleChoice(category = categories[[i]])
    }
  )
}



mock_QuestionMultipleChoice_categories <- function(n, each = 1) {
  purrr::map_chr(
    seq_len(n),
    \(i) {
      sample(letters, 4L) |> paste0(collapse = "")
    }
  ) |>
    rep(each = each)
}



mock_QuestionMultipleChoice <- function(
    prompt = NULL,
    value = NULL,
    mc_options = NULL,
    category = NULL
) {
  prompt     <- prompt %||% "foo"
  value      <- value %||% 10.0
  mc_options <- mc_options %||% mock_QuestionMultipleChoice_options()

  object <- QuestionMultipleChoice$new(
    prompt     = prompt,
    value      = value,
    mc_options = mc_options,
    category   = category
  )

  object
}



mock_QuestionMultipleChoice_options <- function() {
  list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "1.0002",
      label = "bar",
      is_correct = FALSE
    ),
    list(
      id = "1.0003",
      label = "baz",
      is_correct = FALSE
    ),
    list(
      id = "1.0004",
      label = "badonq",
      is_correct = FALSE
    )
  )
}
