test_that("convert spatial unit works", {

  a <- convert_spatial_unit_ny(input_census_tract  = c("36005036400", "36085031902", "36085017014"), convert_to = "neighborhood")
  b <- convert_spatial_unit_ny(input_census_tract  = c("36005036400", "36085031902", "36085017014"), convert_to = "borough")
  c <- convert_spatial_unit_ny(input_neighborhood_name = c("Eastchester-Edenwald-Baychester", "Mariner's Harbor-Arlington-Graniteville", "Arden Heights-Rossville" ))

  expect_equal(b, c)
  expect_equal(a, c("Eastchester-Edenwald-Baychester", "Mariner's Harbor-Arlington-Graniteville", "Arden Heights-Rossville" ))

})
