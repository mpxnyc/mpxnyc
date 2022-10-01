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
get_place_data <- function(limit=1, url = "https://core01.respndmi.app:7473", user = "mp_read_user", password = "L8upDYa8MrJ6Cl2AFfe0"){
  paste(
    "MATCH (a:CensusTract)-[l:LIVES_IN]-(p:Person)-[r:GROUP_SEX_IN]-(c:CensusTract)",
    "RETURN",
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
    data.frame()
}
