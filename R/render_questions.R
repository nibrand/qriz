render_question_panel <- function(
  inputId,
  questions
) {
  checkmate::assert_string(inputId)
  checkmate::assert_list(questions)

  categories <- purrr::map_chr(
    questions,
    \(e) {
      e$category %||% NA_character_
    }
  ) |>
    unique()

  stopifnot(!any(is.na(categories)))

  category_panels <- purrr::map(
    categories,
    \(category) {

      questions_by_category <- get_questions_by_category(questions, category)

      buttons <- purrr::map(
        questions_by_category,
        \(e) {
          render_question_panel_button(inputId, e)
        }
      )

      header <- shiny::tags$div(
        class = "qz-question-panel__category-label",
        shiny::tags$span(category)
      )

      shiny::tags$div(
        class = "qz-question-panel__category-buttons",
        header,
        shiny::tags$div(
          class = "qz-question-panel__question-choices",
          !!!buttons
        )
      )
    }
  ) |>
    shiny::tagList()

  shiny::tagList(
    shiny::tags$div(
      class = "qz-question-panel",
      !!!category_panels
    )
  )
}



render_question_panel_button <- function(
    inputId,
    question
) {
  checkmate::assert_string(inputId)
  checkmate::assert_class(question, "QuestionMultipleChoice")

  shiny::tags$button(
    question$value,
    "id" = question$id,
    "type" = "button",
    "class" = "btn btn-default action-button",
    "onclick" = make_question_panel_button_onclick_handler(inputId, question$id)
  )
}



make_question_panel_button_onclick_handler <- function(
    inputId,
    id_question
) {
  sprintf("
    Shiny.setInputValue('%s', {
      id_question: '%s'
    })
  ", inputId, id_question)
}



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
    "id" = id_option,
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
