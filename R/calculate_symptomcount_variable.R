

#' Calculate SymptomCount variable
#'
#' @param data Person data set
#' @param symptvars Name of symptom variables (character vector)
#'
#' @return data.frame
#' @export
#'
#' @examples
  calculate_symptomcount_variable <- function(data, symptvars) {
    #This function takes in a participants_data data.frame, and returns an integer vector

    for (x in symptom_vars) {
      temp <- data[,symptom_vars]
      temp[,x] <- ifelse(temp[,x]=="TRUE",1,0)
      num_symptoms <- rowSums(temp)
    }
    data$num_symptoms <- num_symptoms
    return(data)
  }

