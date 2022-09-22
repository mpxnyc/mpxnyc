test_that("Get Place Data works", {
  result <- get_place_data()
  expect_equal(dim(result), c(1,7))
})
