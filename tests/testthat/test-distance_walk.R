test_that("distance.walk works", {
  expect_equal(distance.walk(census_tract_from = "36005028800", census_tract_to = "36005017100", key="AIzaSyDU6ij5jqFj0HVuqlmvUH_sYgA05LZVZZc"), c(distance=6130, duration=4665))
})
