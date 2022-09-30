test_that("multiplication works", {
  expect_equal(distance.subway(census_tract_from = "36005012701", census_tract_to = "36085006700", key="AIzaSyDU6ij5jqFj0HVuqlmvUH_sYgA05LZVZZc"), c(distance=42561, duration=6738))
})
