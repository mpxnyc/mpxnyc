
#' Create Gender Variable
#'
#' @param data Person data (data.frame)
#'
#' @return data.frame Person data with genderId variable

calculate_gender_variable <- function(data) {
  cisgender_man       <- data$gender == "man" & data$sex == "male"
  cisgender_woman     <- data$gender == "woman" & data$sex == "female"
  transgender_man     <- (data$gender == "man" & data$sex == "female") | data$gender == "trans-man"
  transgender_woman   <- (data$gender == "woman" & data$sex == "male") | data$gender == "trans-woman"
  non_binary          <- data$gender == "non-binary"
  other               <- data$gender == "other"

  data$genderId <- ifelse(cisgender_man, "cisgender-man",
                          ifelse(cisgender_woman, "cisgender-woman",
                                 ifelse(transgender_man, "transgender-man",
                                        ifelse(transgender_woman, "transgender-woman",
                                               ifelse(non_binary, "non-binary",
                                                      ifelse(other, "other", "missing"))))))

  data
}
