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
