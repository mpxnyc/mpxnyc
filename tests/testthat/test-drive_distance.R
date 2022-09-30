test_that("drive_distance works", {
  expect_equal(
    distance.drive(
      census_tract_from = "36005028800",
      census_tract_to = "36005017100",
      key="AIzaSyDU6ij5jqFj0HVuqlmvUH_sYgA05LZVZZc"
      ),
    c(distance=7403, duration=1353))

})
