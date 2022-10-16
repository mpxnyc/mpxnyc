test_that("Get Place Data works", {
  result <- get_place_data(password ="L8upDYa8MrJ6Cl2AFfe0")
  expect_equal(dim(result), c(1,7))
})
