Question <- R6::R6Class(

  "Question",

  public = list(

    initialize = \(
      prompt,
      value
    ) {

      if (class(self)[1L] == "Question") {
        rlang::abort(
          "The Question class is an abstract class. Only child classes may be initialized",
          class = "abstract_class_instance"
        )
      }

      checkmate::assert_character(prompt, len = 1, any.missing = FALSE, null.ok = FALSE)
      checkmate::assert_number(value)


      private$.uuid   <- ids::uuid(n = 1L, drop_hyphens = TRUE)
      private$.prompt <- prompt
      private$.value  <- value

      private$.is_solved <- FALSE

      return(self)
    },

    submit = \() {
      rlang::abort(
        "submit method needs to be implemented in concrete child class",
        class = "abstract_class_invalid_implementation"
      )
    },

    unsolve = \() {
      private$.is_solved <- FALSE
      invisible(self)
    }
  ),

  private = list(
    .uuid      = NULL,
    .prompt    = NULL,
    .value     = NULL,
    .is_solved = NULL
  ),

  active = list(
    id = \() {
      return(private$.uuid)
    },

    prompt = \() {
      return(private$.prompt)
    },

    value = \() {
      return(private$.value)
    },

    is_solved = \() {
      return(private$.is_solved)
    }
  )
)



QuestionMultipleChoice <- R6::R6Class(

  "QuestionMultipleChoice",

  inherit = Question,

  public = list(

    initialize = \(
      prompt,
      value,
      mc_options
    ) {
      super$initialize(prompt, value)

      assert_multiple_choice_options(mc_options)

      private$.mc_options <- mc_options
    },

    submit = \(id) {
      selected <- purrr::detect(
        private$.mc_options,
        \(e) {
          e$id == id
        }
      )
      if (is.null(selected)) {
        rlang::abort(glue::glue(
          "Unable to find option with id {id}"
        ))
      }
      private$.is_solved <- selected$is_correct
      return(selected$is_correct)
    }
  ),

  private = list(
    .mc_options = NULL
  ),

  active = list(
    mc_options = \() {
      return(private$.mc_options)
    }
  )
)



assert_multiple_choice_option_single <- function(x) {
  stopifnot(is.list(x))
  stopifnot(setequal(names(x), c("id", "label", "is_correct")))

  checkmate::assert_character(x$id, len = 1L, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_character(x$label, len = 1L, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_flag(x$is_correct)

  invisible(x)
}



assert_multiple_choice_options <- function(x) {
  stopifnot(is.list(x))

  purrr::walk(x, assert_multiple_choice_option_single)

  stopifnot(!any(duplicated(purrr::map_chr(x, \(e) e$id))))
  stopifnot(sum(purrr::map_lgl(x, \(e) e$is_correct)) == 1L)

  invisible(x)
}



assert_questions <- function(x) {

  if (!is.list(x)) {
    return(checkmate::assert_class(x, "Question"))
  }

  purrr::walk(
    x,
    \(e) {
      checkmate::assert_class(e, "Question")
    }
  )
}



get_question <- function(x, id) {
  checkmate::assert_list(x)

  res <- purrr::detect(
    x,
    \(e) {
      x$id == id
    }
  )

  if (length(res) == 0L) {
    rlang::abort(glue::glue(
      "Unable to find question with id {id}"
    ))
  }

  res
}
