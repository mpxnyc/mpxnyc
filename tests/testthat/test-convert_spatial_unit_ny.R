test_that("convert spatial unit works", {

  a <- convert_spatial_unit_ny(input_census_tract  = c("36005036400", "36085031902", "36085017014"), convert_to = "neighborhood")
  b <- convert_spatial_unit_ny(input_census_tract  = c("36005036400", "36085031902", "36085017014"), convert_to = "community")
  c <- convert_spatial_unit_ny(input_census_tract  = c("36005036400", "36085031902", "36085017014"), convert_to = "borough")

  d <- convert_spatial_unit_ny(input_neighborhood_name  = a, convert_to = "community")
  e <- convert_spatial_unit_ny(input_neighborhood_name  = a, convert_to = "borough")

  f <- convert_spatial_unit_ny(input_community_district  = d, convert_to = "borough")


  expect_equal(b, d)
  expect_equal(c, e)
  expect_equal(e, f)

})
