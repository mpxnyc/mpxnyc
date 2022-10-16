test_that("Get Person Data works", {
  test_result <- get_person_data(password ="L8upDYa8MrJ6Cl2AFfe0")
  expect_equal(dim(test_result), c(1,44))
})
