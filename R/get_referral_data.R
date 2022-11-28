#' Get Referral Data
#'
#' @param limit Maximum number of cases to fetch
#' @param url URL for neo4j server
#' @param user URL Username for neo4j server
#' @param password URL Password for neo4j server
#'
#' @return data.frame Person-to-person referral data
#' @export
#'
get_referral_data <- function(limit=1, url = "https://core01.respndmi.app:7473", user = "mp_read_user", password = ""){

  if (password == "") password = getPass::getPass(msg = "Neo4j Password:")

  paste(
    "MATCH (a:Person)-[x:FRIENDS_WITH]->(b:Person)",
    "MATCH (c:Person)-[y:HAD_SEX_WITH]->(d:Person)",
    "MATCH (e:Person)-[z:HANGOUT_WITH]->(f:Person)",
    "WITH [{sender: a.userId, receiver: b.userId, createdAt: x.createdAt, type: 'FRIENDS_WITH'}, {sender: c.userId, receiver: d.userId, createdAt: y.createdAt, type: 'HAD_SEX_WITH'}, {sender: e.userId, receiver: f.userId, createdAt: z.createdAt,type: 'HANGOUT_WITH'}] as list",
    "UNWIND list as object",
    "WITH DISTINCT object.createdAt as createdAt, object.sender as sender, object.receiver as reciever, object.type as type",
    "RETURN createdAt,  sender,  reciever,  type",
    "LIMIT ", limit
  ) |>

    neo4r::call_neo4j(neo4r::neo4j_api$new(url = url, user = user, password = password)) |>
    purrr::pluck() |>
    purrr::pluck() |>
    lapply(function(x) dplyr::pull(x, value)) |>
    data.frame()

}
