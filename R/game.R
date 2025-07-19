CashGame <- R6::R6Class(

  "CashGame",

  public = list(
    initialize = \(
      questions,
      players
    ) {
      checkmate::assert_list(questions)
      checkmate::assert_list(players)
      assert_questions(questions)
      assert_players(players)

      private$.uuid <- ids::uuid(n = 1L, drop_hyphens = TRUE)

      private$.questions <- if (!is.list(questions)) list(questions) else questions
      private$.players   <- if (!is.list(players))   list(players)   else players
    },

    submit = \(
      id_question,
      id_option,
      id_player
    ) {
      question <- get_question(self$questions, id_question)

      if (question$is_answered) {
        return(NULL)
      }

      is_correct <- question$submit(id_option)

      selection <- if (is_correct) get_player else discard_player

      players_to_update <- selection(self$players, id_player)

      purrr::walk(
        players_to_update,
        \(e) {
          e$update(question$value)
        }
      )

      return(players_to_update)
    }
  ),

  private = list(
    .uuid = NULL,
    .questions = NULL,
    .players = NULL
  ),

  active = list(
    id = \() {
      return(private$.uuid)
    },

    questions = \() {
      return(private$.questions)
    },

    players = \() {
      return(private$.players)
    }
  )
)
