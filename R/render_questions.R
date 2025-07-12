render_mc_question <- function(
    inputId,
    question
) {

  checkmate::assert_string(inputId)
  checkmate::assert_class(question, "QuestionMultipleChoice")

  prompt      <- question$prompt
  id_question <- question$id
  options     <- question$mc_options

  buttons <- purrr::map(
    options,
    \(e) {
      render_mc_option(
        inputId     = inputId,
        label       = e$label,
        id_question = id_question,
        id_option   = e$id
      )
    }
  ) |>
    shiny::tagList()

  shiny::tags$div(
    shiny::tags$div(
      class = "qz-mc-prompt",
      shiny::tags$span(prompt)
    ),
    shiny::tags$div(
      class = "qz-mc-options-wrapper",
      buttons
    )
  )
}



render_mc_option <- function(
    inputId,
    label,
    id_question,
    id_option
) {

  shiny::tags$button(
    label,
    "id" = "id_option",
    "type" = "button",
    "class" = "qz-mc-option btn btn-default action-button",
    "onclick" = make_mc_option_onclick_handler(inputId, id_question, id_option)
  )
}



make_mc_option_onclick_handler <- function(
    inputId,
    id_question,
    id_option
) {
  sprintf("
    Shiny.setInputValue('%s', {
      id_question: '%s',
      id_option: '%s'
    })
  ", inputId, id_question, id_option)
}
