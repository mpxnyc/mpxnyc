

calculate_variable_formats <- function(data) {

  character_variables <- c("userId", "censusTractHome", "anotherQuestion")
  numeric_variables   <- c("countFriends", "countPhysical",  "countSex", "num_symptoms", "travelTime")
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


  result <- data |>
    dplyr::mutate(across(all_of(numeric_variables), as.numeric)) %>%
    dplyr::mutate(across(all_of(logical_variables), as.logical)) %>%
    dplyr::mutate(across(all_of(factor_variables), as.factor))

  attr(result, "character_variables") <- character_variables
  attr(result, "numeric_variables")   <- numeric_variables
  attr(result, "date_variables")      <- date_variables
  attr(result, "logical_variables")   <- logical_variables

  return(result)

}
