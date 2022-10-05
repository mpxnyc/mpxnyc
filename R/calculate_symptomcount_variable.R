

#' Calculate Symptom Count variable
#'
#' @param data Person data set
#'
#' @return data.frame with num_symptoms variable
#'
#' @examples
  calculate_symptomcount_variable <- function(data) {

    symptom_vars <- names(data[,grepl("symptom",names(data))])

    for (x in symptom_vars) {
      temp <- data[,symptom_vars]
      temp[,x] <- ifelse(temp[,x]=="TRUE",1,0)
      num_symptoms <- rowSums(temp)
    }
    data$num_symptoms <- num_symptoms
    return(data)
  }

