test_that("Get Person Data works", {
  test_result <- get_person_data()
  expect_equal(dim(test_result), c(1,44))
})
