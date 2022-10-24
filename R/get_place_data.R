#' Get Place Data
#'
#' @param limit Maximum number of cases to fetch
#' @param url URL for neo4j server
#' @param user URL Username for neo4j server
#' @param password URL Password for neo4j server
#'
#' @return data.frame Place-level data
#' @export
#'
get_place_data <- function(limit=1, url = "https://core01.respndmi.app:7473", user = "mp_read_user", password = ""){

  if (password == "") password = getPass::getPass(msg = "Neo4j Password:")

  paste(
    "MATCH (a:CensusTract)-[l:LIVES_IN]-(p:Person)-[r:GROUP_SEX_IN]-(c:CensusTract)",
    "RETURN",
    "p.userId as userId,",
    "a.identifier as censusTractHome,",
    "c.identifier as censusTractPlace,",
    "r.createdAt as createdAt,",
    "r.placeType as placeType,",
    "r.placeSex as placeSex,",
    "r.placeFreqAttend as placeFreqAttend,",
    "r.placeFreqHaveSex as placeFreqHaveSex",
    "LIMIT ", limit
  ) |>

    neo4r::call_neo4j(neo4r::neo4j_api$new(url = url, user = user, password = password)) |>
    purrr::pluck() |>
    purrr::pluck() |>
    lapply(function(x) dplyr::pull(x, value)) |>
    data.frame() |>
    calculate_variable_formats() |>
    sjlabelled::set_label(label = c("user Identifier",
                                    "Where in the city do you live? Tap on the map to show your home.",
                                    "Over the past 4 weeks, where have you had sex with two or more people at the same time or had close physical contact with multiple people at the same time? Place a pin on the location by tapping the map.",
                                    "Time stamp",
                                    "What kind of place is this?",
                                    "Did you have sexual contact while at this venue?",
                                    "Over the past 4 weeks, how many times have you attended this venue?",
                                    "Over the past 4 weeks, how many times have you had sexual contact at this venue?"))

}
