#' Get Person Data
#'
#' @param url URL for neo4j server
#' @param user URL Username for neo4j server
#' @param password URL Passwortd for neo4j server
#'
#' @return data.frame Person Level Data
#' @export
#'


get_person_data <- function(url = "https://core01.respndmi.app:7473", user = "mp_read_user", password = "L8upDYa8MrJ6Cl2AFfe0"){

    paste(
      "MATCH (a:CensusTract)-[l:LIVES_IN]-(n:Person)",
      "RETURN",
      "a.identifier as censusTractHome,",
      "n.age as age,",
      "n.anotherQuestion as anotherQuestion,",
      "n.sex as sex,",
      "n.channel as channel,",
      "n.countFriends as countFriends,",
      "n.countPhysical as countPhysical,",
      "n.countSex as countSex,",
      "n.covidTestPositive as covidTestPositive,",
      "n.createdAt as createdAt,",
      "n.gender as gender,",
      "n.groupSex as groupSex,",
      "n.hivPrep as hivPrep,",
      "n.hivStatus as hivStatus,",
      "n.hivSuppressed as hivSuppressed,",
      "n.monkeypoxCare as monkeypoxCare,",
      "n.monkeypoxTest as monkeypoxTest,",
      "n.monkeypoxVaccine as monkeypoxVaccine,",
      "n.raceAsian as raceAsian,",
      "n.raceBlack as raceBlack,",
      "n.racePacific as racePacific,",
      "n.raceWhite as raceWhite,",
      "n.raceOther as raceOther,",
      "n.referralType as referralType,",
      "n.referrerEnglish as referrerEnglish,",
      "n.returnParticipant as returnParticipant,",
      "n.sexOrientation as sexOrientation,",
      "n.symptomBackAche as symptomBackAche,",
      "n.symptomBodyRash as symptomBodyRash,",
      "n.symptomChills as symptomChills," ,
      "n.symptomExhaustion as symptomExhaustion,",
      "n.symptomFacialRash as symptomFacialRash,",
      "n.symptomFever as symptomFever,",
      "n.symptomHeadache as symptomHeadache,",
      "n.symptomMouth as symptomMouth,",
      "n.symptomMuscleAche as symptomMuscleAche,",
      "n.symptomOther as symptomOther,",
      "n.symptomRectalDiscomfort as symptomRectalDiscomfort,",
      "n.symptomSorePenis as symptomSorePenis,",
      "n.symptomSoreThroat as symptomSoreThroat,",
      "n.symptomSoresAnus as symptomSoresAnus,",
      "n.symptomSwollenGlands as symptomSwollenGlands,",
      "n.travelTime as travelTime,",
      "n.vaccinationDate as vaccinationDate;"
    ) |>

      neo4r::call_neo4j(neo4r::neo4j_api$new(url = url, user = user, password = password)) |>
      purrr::pluck() |>
      purrr::pluck() |>
      lapply(function(x) dplyr::pull(x, value)) |>
      data.frame()
  }


