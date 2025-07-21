render_player_table <- function(
    inputId,
    players
) {
  checkmate::assert_list(players)

  rows <- purrr::map(
    players,
    \(e) {
      shiny::tags$div(
        class = "qz-player-table__row",
        shiny::tags$span(
          class = "qz-player-table__name",
          e$name
        ),
        shiny::tags$a(
          class = "action-button qz-player-table__delete",
          "onclick" = make_player_table_delete_button_input_handler(inputId, e$id),
          shiny::icon("trash")
        )
      )
    }
  ) |>
    tagList()

  body <- shiny::tags$div(
    class = "qz-player-table__body qz-player-card__body",
    !!!rows
  )

  shiny::tags$div(
    class = "qz-player-table qz-player-card-wrapper",
    shiny::tags$div(
      class = "qz-player-table__header qz-player-card__header",
      "Spieler:innen"
    ),
    body
  )
}



make_player_table_delete_button_input_handler <- function(
    inputId,
    id_player
) {
  sprintf("
    Shiny.setInputValue('%s', {
      id_player: '%s'
    }, {priority: 'event'})
  ", inputId, id_player)
}



render_player_cards <- function(players) {
  checkmate::assert_list(players)

  player_cards <- purrr::map(players, render_player_card) |>
    tagList()

  shiny::tags$div(
    class = "qz-player-card-wrapper",
    player_cards
  )
}



render_player_card <- function(player) {
  checkmate::assert_class(player, "Player")

  shiny::tags$div(
    id = player$id,
    class = "qz-player-card",
    shiny::tags$div(
      class = "qz-player-card__header",
      shiny::tags$span(player$name)
    ),
    shiny::tags$div(
      class = "qz-player-card__body",
      shiny::tags$span(
        "Gewinn: "
      ),
      shiny::tags$span(
        class = "qz-player-card__score",   # currency symbol in CSS
        player$score
      )
    )
  )
}
