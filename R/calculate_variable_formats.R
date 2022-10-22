

calculate_variable_formats <- function(data) {

  character_variables <- c("userId", "censusTractHome", "anotherQuestion", "userId", "censusTractPlace")
  numeric_variables   <- c("countFriends", "countPhysical",  "countSex", "num_symptoms", "travelTime", "placeFreqAttend", "placeFreqHaveSex")
  date_variables      <- c("createdAt", "vaccinationDate")
  logical_variables   <- c(
    "raceAsian", "raceBlack", "racePacific", "raceWhite", "raceLatinx", "raceOther",
    "referrerEnglish",
    "symptomBackAche", "symptomBodyRash", "symptomChills", "symptomExhaustion", "symptomFacialRash", "symptomFever", "symptomHeadache", "symptomMouth", "symptomMuscleAche", "symptomOther", "symptomRectalDiscomfort", "symptomSorePenis", "symptomSoreThroat", "symptomSoresAnus", "symptomSwollenGlands"
    )

  factor_variables    <- names(data) |>
    setdiff(character_variables) |>
    setdiff(numeric_variables) |>
    setdiff(date_variables) |>
    setdiff(logical_variables)


summary_variables <- c(factor_variables, numeric_variables, logical_variables)

clean_and_factor <- function(x) {
  x <- as.character(x)
  clean <- ifelse(x == "", NA, x)
  as.factor(clean)
}


  result <- data |>
    dplyr::mutate(dplyr::across(dplyr::any_of(numeric_variables), as.numeric)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(logical_variables), as.logical)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(factor_variables), clean_and_factor)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(date_variables), as.Date)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("channel")), function(x) {factor(
                                                                                  x,
                                                                                  c("9999", "pt", "gs", "tw", "ig", "gr", "fr", "em", "cm", "lnyc"),
                                                                                  c("unkown", "partner_toolkit", "google_search", "twitter", "instagram", "grindr", "ferry", "earned_media", "cameo_promotion", "link_nyc")
                                                                                    )}))


  attr(result, "character_variables") <- character_variables
  attr(result, "numeric_variables")   <- numeric_variables
  attr(result, "date_variables")      <- date_variables
  attr(result, "logical_variables")   <- logical_variables
  attr(result, "summary_variables")   <- summary_variables

  return(result)

}
