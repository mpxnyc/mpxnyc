#' Backup Data
#'
#'@param directory Directory to save backup (full path)
#' @param limit Maximum number of cases to fetch
#' @param url URL for neo4j server
#' @param user URL Username for neo4j server
#' @param password URL Password for neo4j server
#'
#' @return data.frame Person-to-person referral data
#' @export
#'
backup_data <- function(directory="", limit=1000000, url = "https://core01.respndmi.app:7473", user = "mp_read_user", password = ""){

  if (directory == "") stop("Please specify directory")
  if (password == "") password = getPass::getPass(msg = "Neo4j Password:")

  person_data <- get_person_data(limit = limit, url = url, user = user, password = password)
  place_data <- get_place_data(limit = limit, url = url, user = user, password = password)
  referral_data <- get_referral_data(limit = limit, url = url, user = user, password = password)

  backup_data <- list(person_data = person_data, place_data = place_data, referral_data = referral_data)
  #saveRDS(backup_data, file = paste0(directory, "/mpxnyc_backup_data.R"))


}
