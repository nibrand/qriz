Player <- R6::R6Class(

  "Player",

  inherit = ISubscriber,

  public = list(

    initialize = \(name) {
      checkmate::assert_character(name, len = 1, any.missing = FALSE, null.ok = FALSE)

      private$.uuid  <- ids::uuid(n = 1L, drop_hyphens = TRUE)
      private$.name  <- name
      private$.score <- 0.0

      invisible(self)
    },

    update = \(score_increment) {
      super$update(score_increment)

      old_score <- private$.score
      new_score <- old_score + score_increment

      private$.score <- new_score

      invisible(old_score)
    }
  ),

  private = list(
    .uuid  = NULL,
    .name  = NULL,
    .score = NULL
  ),

  active = list(
    id = \() {
      return(private$.uuid)
    },

    name = \() {
      return(private$.name)
    },

    score = \() {
      return(private$.score)
    }
  )
)



assert_players <- function(x) {

  if (!is.list(x)) {
    return(checkmate::assert_class(x, "Player"))
  }

  purrr::walk(
    x,
    \(e) {
      checkmate::assert_class(e, "Player")
    }
  )
}



get_player <- function(x, id) {
  checkmate::assert_list(x)

  res <- purrr::keep(
    x,
    \(e) {
      e$id == id
    }
  )

  if (length(res) == 0L) {
    rlang::abort(glue::glue(
      "Unable to find player with id {id}"
    ))
  }

  res
}



discard_player <- function(x, id) {
  checkmate::assert_list(x)

  res <- purrr::discard(
    x,
    \(e) {
      e$id == id
    }
  )

  if (length(res) == length(x)) {
    rlang::warn(glue::glue(
      "Unable to find player with id {id}"
    ))
  }

  res
}
