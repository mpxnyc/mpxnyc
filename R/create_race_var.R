#' Cr4eate Race Variable
#'
#' @param data Person data (data.frame)
#' @param racevars Names of Race Variables (character vector)
#' @param racelabels Race Labels (character vector)
#'
#' @return data.frame Updated dataset
#' @export
#'
create_race_var <- function(data , racevars, racelabels) {
  #This function takes in a participants_data data.frame, and returns a categorical vector

  for (x in racevars) {
    data[,x] <- ifelse(data[,x]=="TRUE",1,0)
  }
  data$sum_race <- rowSums(data[,racevars])
  #
  data$race <- factor(ifelse(data$sum_race==1 & data[,racevars[1]]==1, racelabels[1],
                             ifelse(data$sum_race==1 &  data[,racevars[2]]==1, racelabels[2],
                                    ifelse(data$sum_race==1 &  data[,racevars[3]]==1, racelabels[3],
                                           ifelse(data$sum_race==1 &  data[,racevars[4]]==1, racelabels[4],
                                                  ifelse((data$sum_race==1 &  data[,racevars[5]]==1)
                                                         |
                                                           (data$sum_race==1 &  data[,racevars[6]]==1), racelabels[5], #Combining Pacific with Other race
                                                         ifelse(data$sum_race > 1, racelabels[6],NA)))))))

  data$sum_race <- NULL
  return(data)
}
