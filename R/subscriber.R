ISubscriber <- R6::R6Class(

  "ISubscriber",

  public = list(
    # implement constructor to block any attempt of initializing an interface object
    initialize = \() {
      rlang::abort(
        "ISubscriber is an interface that cannot be initialized on its own",
        class = "interface_instance"
      )
    },

    update = \(score_increment) {
      is_bad_implementation <-
        !checkmate::test_number(score_increment)

      if (is_bad_implementation) {
        rlang::abort(
          "Invalid implementation of ISubscriber",
          class = "interface_invalid_implementation"
        )
      }

      invisible(NULL)
    }
  )
)
