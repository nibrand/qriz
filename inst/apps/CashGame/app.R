ui <- function() {

  bslib::page_fillable(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    get_js_dependencies("CashGame", dependencies = "www/main.js"),
    shinyjs::useShinyjs(),
    shiny::tags$body(
      shiny::tags$div(

        class = "qz-content-wrapper",

        tags$div(
          class = "qz-start-menu",

          shiny::tags$h1(
            class = "qz-start-menu__header",
            "Willkommen"
          ),

          shiny::tags$div(
            class = "qz-player-selection",
            shiny::textInput(
              inputId = "inp_player_name",
              label = "Name"
            ) |>
              shiny::tagAppendAttributes(class = "qz-player-selection__inp-player-name"),
            shiny::actionLink(
              inputId = "btn_create_player",
              label = "Spieler hinzufügen"
            ) |>
              shiny::tagAppendAttributes(class = "link-pill link-pill--success")
          ),

          shiny::uiOutput("tbl_players")
        ),

        shiny::tags$div(
          class = "qz-start-game",
          shiny::actionButton(
            inputId = "btn_start_game",
            label = "Spiel starten"
          ) |>
            shiny::tagAppendAttributes(class = "qz-start-game__btn-start") |>
            shinyjs::disabled()
        )
      )
    )
  )
}

server <- function(input, output, session) {

  show_question_panel <- \() {
    shiny::insertUI(
      ".qz-content-wrapper",
      where = "afterBegin",
      ui = render_question_panel(
        inputId = "question_panel",
        questions = get_context("questions")
      )
    )
  }

  show_single_question <- \(id_question) {
    questions <- get_context("questions")
    shiny::insertUI(
      ".qz-content-wrapper",
      where = "afterBegin",
      ui = render_mc_question(
        inputId = "inp_mc_question",
        inputId_backButton = "btn_back_to_panel",
        question = get_question(questions, id_question)
      )
    )
  }

  players <- shiny::reactiveVal(list())
  game    <- NULL

  shiny::observeEvent(
    input$btn_create_player,
    {
      name <- input$inp_player_name
      new_player <- Player$new(name)
      new_players <- add_player(players(), new_player)
      players(new_players)

      shinyjs::enable("btn_start_game")
      shinyjs::addClass("btn_start_game", class = "btn-primary")
      shiny::updateTextInput(
        inputId = "inp_player_name",
        value = ""
      )
    }
  )

  shiny::observeEvent(
    players(),
    {
      output$tbl_players <- renderUI({
        if (length(players()) == 0L) {
          return(NULL)
        }
        render_player_table(
          inputId = "tbl_players",
          players = players()
        )
      })

      is_ready_game <- length(players()) == 1L

      shinyjs::toggleState(
        "btn_start_game",
        condition = is_ready_game
      )

      shinyjs::toggleClass(
        "btn_start_game",
        class = "btn-primary",
        condition = is_ready_game
      )
    }
  )

  shiny::observeEvent(
    input$tbl_players,
    {
      id_player <- input$tbl_players[["id_player"]]
      new_players <- discard_player(players(), id = id_player)
      players(new_players)
    }
  )

  shiny::observeEvent(
    input$btn_start_game,
    {
      game <- CashGame$new(
        players = players(),
        questions = mock_QuestionMultipleChoice() |> list()
      )

      shiny::removeUI(".qz-content-wrapper *", multiple = TRUE)

      show_question_panel()
    }
  )

  shiny::observeEvent(
    input$question_panel,
    {
      id_question <- input$question_panel[["id_question"]]

      shiny::removeUI(".qz-content-wrapper *", multiple = TRUE)
      show_single_question(id_question)
    }
  )

  shiny::observeEvent(
    input$btn_back_to_panel,
    {
      shiny::removeUI(".qz-content-wrapper *", multiple = TRUE)

      show_question_panel()
    }
  )
}

shiny::shinyApp(ui, server)
