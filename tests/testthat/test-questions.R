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



testthat::test_that("get_question_prop gets categories", {
  questions <- mock_QuestionMultipleChoice_list(
    n = 4L,
    categories = c("a", "b", "a", "b")
  )

  expectation <- c("a", "b", "a", "b")

  object <- get_question_prop(questions, "category")

  testthat::expect_equal(object, expectation)
})



testthat::test_that("get_question_prop gets values", {
  questions <- list(
    mock_QuestionMultipleChoice(value = 1),
    mock_QuestionMultipleChoice(value = 5)
  )

  expectation <- c(1, 5)

  object <- get_question_prop(questions, "value")

  testthat::expect_equal(object, expectation)
})



testthat::test_that("questions can be sorted by category and value", {
  questions <- list(
    mock_QuestionMultipleChoice(value = 10, category = "b"),
    mock_QuestionMultipleChoice(value = 5,  category = "b"),
    mock_QuestionMultipleChoice(value = 10, category = "a"),
    mock_QuestionMultipleChoice(value = 5,  category = "a")
  )

  expected_categories <- c("a", "a", "b", "b")
  expected_values     <- c(5, 10, 5, 10)

  sorted <- sort_questions_by_category_and_value(questions)

  testthat::expect_equal(get_question_prop(sorted, "category"), expected_categories)
  testthat::expect_equal(get_question_prop(sorted, "value"), expected_values)
})
