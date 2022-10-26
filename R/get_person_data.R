#' Get Person Data
#'
#' @param limit Maximum number of cases to fetch
#' @param url URL for neo4j server
#' @param user URL Username for neo4j server
#' @param password URL Password for neo4j server
#'
#' @return data.frame Person-level data
#' @export
#'


get_person_data <- function(limit=1, url = "https://core01.respndmi.app:7473", user = "mp_read_user", password = ""){

  if (password == "") password = getPass::getPass(msg = "Neo4j Password:")

    paste(
      "MATCH (a:CensusTract)-[l:LIVES_IN]-(n:Person)",
      "RETURN",
      "n.userId as userId,",
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
      "n.raceLatinx as raceLatinx,",
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
      "n.vaccinationDate as vaccinationDate",
      "LIMIT ", limit
    ) |>

      neo4r::call_neo4j(neo4r::neo4j_api$new(url = url, user = user, password = password)) |>
      purrr::pluck() |>
      purrr::pluck() |>
      lapply(function(x) dplyr::pull(x, value)) |>
      data.frame() |>
      calculate_symptomcount_variable() |>
      calculate_gender_variable() |>
      calculate_race_variable() |>
      calculate_variable_formats() |>
      sjlabelled::set_label(label = c("User Identifier",
                                      "Where in the city do you live? Tap on the map to show your home.",
                                      "How old are you?",
                                      "What question do you wish we had asked in the survey?",
                                      "What is your sex assigned at birth?",
                                      "Recruitment Channel",
                                      "How many queer and trans friends do you have who are important to you for any reason? Count only those you have been in touch with over the past 4 weeks.",
                                      "How many individuals have you had prolonged physical contact (but no sexual contact) with in the past 4 weeks, excluding group settings?",
                                      "How many individuals have you had sex with in the past 4 weeks, excluding group sex partners?",
                                      "In the past 4 weeks, did you test positive for COVID-19?",
                                      "Timestamp",
                                      "What is your gender identity?",
                                      "Over the past 4 weeks, have you had sex with two or more people at the same time or had close physical contact with multiple people at the same time? (like at a party, sport game, concert, show)",
                                      "Are you on PrEP?",
                                      "What is your HIV status?",
                                      "Have you been told that your viral load is suppressed or undetectable?",
                                      "Did you receive care by a medical provider for your symptoms?",
                                      "In the past 4 weeks, did you receive testing for monkeypox?",
                                      "Have you received at least 1 dose of the monkeypox vaccine?",
                                      "What’s your race and/or ethnicity?: Asian",
                                      "What’s your race and/or ethnicity?: Black",
                                      "What’s your race and/or ethnicity?: Pacific Islander",
                                      "What’s your race and/or ethnicity?: White",
                                      "What’s your race and/or ethnicity?: Latino/a/x",
                                      "What’s your race and/or ethnicity?: Other",
                                      "What kind of referral into survey?",
                                      "Did the referrer answer their survey in English?",
                                      "Have you taken the MPX NYC survey before?",
                                      "What is your sexual orientation?",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Fever",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Body Rash",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Chills",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Exhaustion",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Facial Rash",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Fever",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Headache",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Sores in or around the mouth",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Muscle ache",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Other",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Rectal Discomfort",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Sores on the penis",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Sore Throat",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Sores in the buttocks or anus",
                                      "In the past 4 weeks, have you experienced symptoms that are not related to a confirmed COVID-19 infection?: Swollen glands",
                                      "By the way, how long are you willing to travel to hook up?",
                                      "When were you vaccinated against monkeypox?",
                                      "Number of symptoms (calculated)",
                                      "Gender Identity (calculated)",
                                      "Race (calculated)"))

  }


