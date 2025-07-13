library(shiny)

ui <- bslib::page_fillable(
  render_mc_question(
    inputId = "id",
    mock_QuestionMultipleChoice()
  )
)

server <- function(input, output, session) {
  observeEvent(input$id, {
    print(input$id)
  })
}

shiny::shinyApp(ui, server)



ui <- bslib::page_fillable(
  render_question_panel(
    "lol",
    questions = mock_QuestionMultipleChoice_list(
      n = 6,
      categories = mock_QuestionMultipleChoice_categories(2, 3)
    )
  )
)

server <- function(input, output, session) {}

shiny::shinyApp(ui, server)
