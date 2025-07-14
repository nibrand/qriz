# use_context
#
# set_context <- function(key, value) {
#
# }



launch_CashGame <- function() {

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

  set_context("questions", questions)

  shiny::runApp(
    appDir = path_CashGame()
  )
}



path_CashGame <- function(...) {
  path_app("CashGame", ...)
}
