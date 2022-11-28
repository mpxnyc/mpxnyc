test_that("get referral data workds", {
  test_result <- get_referral_data(limit = 10, password ="L8upDYa8MrJ6Cl2AFfe0")
  expect_equal(dim(test_result), c(10, 4))
})
