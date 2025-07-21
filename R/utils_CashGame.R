launch_CashGame <- function(file = NULL) {

  if (is.null(file)) {
    launch_CashGame_debug()
    return(NULL)
  }

  file <- path_CashGame(file)

  checkmate::assert_file_exists(file)

  meta_data <- jsonlite::fromJSON(file, simplifyDataFrame = FALSE)

  welcome <- shiny::HTML(meta_data$txt_welcome)

  questions <- meta_data$questions |>
    purrr::map(
      \(e) {
        QuestionMultipleChoice$new(
          prompt = e$prompt,
          value = e$value,
          category = e$category,
          mc_options = e$mc_options
        )
      }
    ) |>
    sort_questions_by_category_and_value()


  set_context("txt_welcome", welcome)
  set_context("questions", questions)

  shiny::runApp(
    appDir = path_CashGame()
  )
}



launch_CashGame_debug <- function() {
  welcome <- "Wilkommen"

  questions <- list(
    mock_QuestionMultipleChoice(value = 10, category = "Geschichte"),
    mock_QuestionMultipleChoice(value = 10,  category = "Sport"),
    mock_QuestionMultipleChoice(value = 10, category = "Erdkunde"),
    mock_QuestionMultipleChoice(value = 10,  category = "Kultur"),
    mock_QuestionMultipleChoice(value = 10,  category = "Politik"),
    mock_QuestionMultipleChoice(value = 5, category = "Geschichte"),
    mock_QuestionMultipleChoice(value = 5,  category = "Sport"),
    mock_QuestionMultipleChoice(value = 5, category = "Erdkunde"),
    mock_QuestionMultipleChoice(value = 5,  category = "Kultur"),
    mock_QuestionMultipleChoice(value = 5,  category = "Politik"),
    mock_QuestionMultipleChoice(value = 1, category = "Geschichte"),
    mock_QuestionMultipleChoice(value = 1,  category = "Sport"),
    mock_QuestionMultipleChoice(value = 1, category = "Erdkunde"),
    mock_QuestionMultipleChoice(value = 1,  category = "Kultur"),
    mock_QuestionMultipleChoice(value = 1,  category = "Politik")
  )

  set_context("txt_welcome", welcome)
  set_context("questions", questions)

  shiny::runApp(
    appDir = path_CashGame()
  )
}



path_CashGame <- function(...) {
  path_app("CashGame", ...)
}
