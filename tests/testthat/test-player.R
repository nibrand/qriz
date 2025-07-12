testthat::test_that("add_player adds a player object to a list", {
  player <- mock_Player("Bob")

  expectation <- list(
    player
  )

  player_list <- add_player(list(), player)
  testthat::expect_equal(player_list, expectation)


  player_2 <- mock_Player("Alice")

  expectation <- list(
    player,
    player_2
  )

  player_list <- add_player(player_list, player_2)
  testthat::expect_equal(player_list, expectation)
})



testthat::test_that("discard_player removes a player object from a list", {
  player   <- mock_Player("Bob")
  player_2 <- mock_Player("Alice")

  player_list <- list(
    player,
    player_2
  )

  expectation <- list(
    player
  )

  player_list <- discard_player(player_list, player_2$id)
  testthat::expect_equal(player_list, expectation)

  expectation <- list()

  player_list <- discard_player(player_list, player$id)
  testthat::expect_equal(player_list, expectation)
})
