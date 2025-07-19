testthat::test_that("Player in CashGame gets rewarded after submitting correct answer", {
  mc_options <- list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0001",
      label = "bar",
      is_correct = FALSE
    )
  )

  question <- QuestionMultipleChoice$new(
    prompt     = "foo",
    value      = 10.0,
    mc_options = mc_options
  )

  player <- Player$new("Bob")

  id_question <- question$id
  id_player   <- player$id

  game <- CashGame$new(questions = list(question), players = list(player))

  game$submit(
    id_question = id_question,
    id_option   = "1.0001",
    id_player   = id_player
  )

  testthat::expect_equal(player$score, 10.0)
})



testthat::test_that("Player in CashGame gets no reward after submitting incorrect answer", {
  mc_options <- list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0001",
      label = "bar",
      is_correct = FALSE
    )
  )

  question <- QuestionMultipleChoice$new(
    prompt     = "foo",
    value      = 10.0,
    mc_options = mc_options
  )

  player <- Player$new("Bob")

  id_question <- question$id
  id_player   <- player$id

  game <- CashGame$new(questions = list(question), players = list(player))

  game$submit(
    id_question = id_question,
    id_option   = "2.0001",
    id_player   = id_player
  )

  testthat::expect_equal(player$score, 0.0)
})



testthat::test_that("Player in CashGame gets no reward after submitting correct answer to a question that has been answered before", {
  mc_options <- list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0001",
      label = "bar",
      is_correct = FALSE
    )
  )

  question <- QuestionMultipleChoice$new(
    prompt     = "foo",
    value      = 10.0,
    mc_options = mc_options
  )

  player <- Player$new("Bob")

  id_question <- question$id
  id_player   <- player$id

  game <- CashGame$new(questions = list(question), players = list(player))

  game$submit(
    id_question = id_question,
    id_option   = "2.0001",
    id_player   = id_player
  )

  testthat::expect_equal(player$score, 0.0)

  game$submit(
    id_question = id_question,
    id_option   = "1.0001",
    id_player   = id_player
  )

  testthat::expect_equal(player$score, 0.0)
})



testthat::test_that("Player in CashGame gets multiple rewards after submitting correct answers back-to-back", {
  mc_options_1 <- list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0001",
      label = "bar",
      is_correct = FALSE
    )
  )

  mc_options_2 <- list(
    list(
      id = "1.0002",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0002",
      label = "bar",
      is_correct = FALSE
    )
  )

  question_1 <- QuestionMultipleChoice$new(
    prompt     = "foo",
    value      = 10.0,
    mc_options = mc_options_1
  )

  question_2 <- QuestionMultipleChoice$new(
    prompt     = "bar",
    value      = 20.0,
    mc_options = mc_options_2
  )

  player <- Player$new("Bob")

  id_question_1 <- question_1$id
  id_question_2 <- question_2$id
  id_player     <- player$id

  game <- CashGame$new(questions = list(question_1, question_2), players = list(player))

  game$submit(
    id_question = id_question_1,
    id_option   = "1.0001",
    id_player   = id_player
  )

  testthat::expect_equal(player$score, 10.0)

  game$submit(
    id_question = id_question_2,
    id_option   = "1.0002",
    id_player   = id_player
  )

  testthat::expect_equal(player$score, 30.0)
})



testthat::test_that("Player in CashGame gets no reward after submitting correct answer to solved question", {
  mc_options <- list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0001",
      label = "bar",
      is_correct = FALSE
    )
  )

  question <- QuestionMultipleChoice$new(
    prompt     = "foo",
    value      = 10.0,
    mc_options = mc_options
  )

  player_1 <- Player$new("Bob")
  player_2 <- Player$new("Alice")

  id_question <- question$id
  id_player_1 <- player_1$id
  id_player_2 <- player_2$id

  game <- CashGame$new(questions = list(question), players = list(player_1, player_2))

  game$submit(
    id_question = id_question,
    id_option   = "1.0001",
    id_player   = id_player_1
  )

  testthat::expect_equal(player_1$score, 10.0)
  testthat::expect_equal(player_2$score,  0.0)

  game$submit(
    id_question = id_question,
    id_option   = "1.0001",
    id_player   = id_player_2
  )

  testthat::expect_equal(player_1$score, 10.0)
  testthat::expect_equal(player_2$score,  0.0)
})



testthat::test_that("Player in CashGame does not earn reward for others after submitting incorrect answer to solved question", {
  mc_options <- list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0001",
      label = "bar",
      is_correct = FALSE
    )
  )

  question <- QuestionMultipleChoice$new(
    prompt     = "foo",
    value      = 10.0,
    mc_options = mc_options
  )

  player_1 <- Player$new("Bob")
  player_2 <- Player$new("Alice")

  id_question <- question$id
  id_player_1 <- player_1$id
  id_player_2 <- player_2$id

  game <- CashGame$new(questions = list(question), players = list(player_1, player_2))

  game$submit(
    id_question = id_question,
    id_option   = "1.0001",
    id_player   = id_player_1
  )

  testthat::expect_equal(player_1$score, 10.0)
  testthat::expect_equal(player_2$score,  0.0)

  game$submit(
    id_question = id_question,
    id_option   = "2.0001",
    id_player   = id_player_2
  )

  testthat::expect_equal(player_1$score, 10.0)
  testthat::expect_equal(player_2$score,  0.0)
})



testthat::test_that("Game submit method returns players that have been updated", {
  mc_options_1 <- list(
    list(
      id = "1.0001",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0001",
      label = "bar",
      is_correct = FALSE
    )
  )

  mc_options_2 <- list(
    list(
      id = "1.0002",
      label = "foo",
      is_correct = TRUE
    ),
    list(
      id = "2.0002",
      label = "bar",
      is_correct = FALSE
    )
  )

  question_1 <- QuestionMultipleChoice$new(
    prompt     = "foo",
    value      = 10.0,
    mc_options = mc_options_1
  )

  question_2 <- QuestionMultipleChoice$new(
    prompt     = "bar",
    value      = 20.0,
    mc_options = mc_options_2
  )

  player_1 <- Player$new("Bob")
  player_2 <- Player$new("Alice")
  player_3 <- Player$new("Ronathan")

  game <- CashGame$new(
    questions = list(question_1, question_2),
    players = list(player_1, player_2, player_3)
  )

  # player 1 solves question 1
  expectation <- list(player_1)

  object <- game$submit(
    id_question = question_1$id,
    id_option   = "1.0001",
    id_player   = player_1$id
  )

  testthat::expect_identical(object, expectation)


  # player 1 fails question 2
  expectation <- list(player_2, player_3)

  object <- game$submit(
    id_question = question_2$id,
    id_option   = "2.0002",
    id_player   = player_1$id
  )

  testthat::expect_identical(object, expectation)
})
