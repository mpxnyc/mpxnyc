test_that("distance.subway works", {
  expect_equal(distance.subway(census_tract_from = "36005012701", census_tract_to = "36085006700", key="AIzaSyDU6ij5jqFj0HVuqlmvUH_sYgA05LZVZZc") |> names(), c("distance", "duration"))
})
