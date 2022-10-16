test_that("drive_distance works", {

  dist <-
    distance.drive(
    census_tract_from = "36005028800",
    census_tract_to = "36005017100",
    key="AIzaSyDU6ij5jqFj0HVuqlmvUH_sYgA05LZVZZc"
  )

  expect_equal(
    names(dist)
    ,
   c("distance", "duration"))


})
