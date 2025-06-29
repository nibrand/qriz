testthat::test_that("Question class cannot be initialized", {
  testthat::expect_error(
    Question$new(
      prompt = "foo",
      value  = 10.0
    ),
    class = "abstract_class_instance"
  )
})



testthat::test_that("QuestionMultipleChoice can be created with a list of options", {

  mc_options <- list(
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
    )
  )

  object <- QuestionMultipleChoice$new(
    prompt     = "foo",
    value      = 10.0,
    mc_options = mc_options
  )

  testthat::expect_s3_class(object, "QuestionMultipleChoice")
  testthat::expect_equal(object$mc_options, mc_options)
})



testthat::test_that("QuestionMultipleChoice object can be solved", {

  mc_options <- list(
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
    )
  )

  object <- QuestionMultipleChoice$new(
    prompt     = "foo",
    value      = 10.0,
    mc_options = mc_options
  )

  testthat::expect_false(object$is_solved)

  object$submit("1.0001")

  testthat::expect_true(object$is_solved)
})



testthat::test_that("QuestionMultipleChoice cannot be initialized with invalid options", {

  mc_options_ambiguous_id <- list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "1.0001",
      label = "bar",
      is_correct = FALSE
    ),
    list(
      id = "3.0001",
      label = "baz",
      is_correct = FALSE
    )
  )

  testthat::expect_error({
    object <- QuestionMultipleChoice$new(
      prompt     = "foo",
      value      = 10.0,
      mc_options = mc_options_ambiguous_id
    )
  })

  mc_options_ambiguous_solution <- list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0001",
      label = "bar",
      is_correct = TRUE
    ),
    list(
      id = "3.0001",
      label = "baz",
      is_correct = FALSE
    )
  )

  testthat::expect_error({
    object <- QuestionMultipleChoice$new(
      prompt     = "foo",
      value      = 10.0,
      mc_options = mc_options_ambiguous_solution
    )
  })
})
