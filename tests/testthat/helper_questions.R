mock_QuestionMultipleChoice <- function(
    prompt = NULL,
    value = NULL,
    mc_options = NULL
) {
  prompt     <- prompt %||% "foo"
  value      <- value %||% 10.0
  mc_options <- mc_options %||% list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0001",
      label = "bar",
      is_correct = FALSE
    ),
    list(
      id = "3.0001",
      label = "baz",
      is_correct = FALSE
    ),
    list(
      id = "4.0001",
      label = "badonq",
      is_correct = FALSE
    )
  )

  object <- QuestionMultipleChoice$new(
    prompt     = prompt,
    value      = value,
    mc_options = mc_options
  )

  object
}
