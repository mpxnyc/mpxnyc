test_that("travel_distance works", {
  expect_equal(
    travel_distance(
      census_tract_from = c("36005028800", "36005017100"),
      census_tract_to = c("36005017100", "36005028800"),
      mode_of_transport="walk",
      key="AIzaSyDU6ij5jqFj0HVuqlmvUH_sYgA05LZVZZc"
      ),
    data.frame(distance=c(6130, 6131), duration=c(4665, 4687))
    )

})
