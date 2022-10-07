#' Descriptive Table
#'
#' @description This function helps to generte tables. It returns an object somewhere between a kable and a data.frame.
#'
#' @param dat Data set (data.frame)
#' @param dig How many digits in the decimal place (integer)
#'
#' @return data.frame What is this going to return
#' @export
#'
descriptivetable <- function(dat, dig) {


  # Top row for later
  toprow <- c(paste("(", "n=", as.vector(nrow(dat)), ")", sep = ""))

  rnames <- NULL

  for (p in 1:dim(dat)[2]) {

    # Row names for factor variables include the variable name and then the names of the levels
    if (is.factor(dat[[ p]])) {

      #if (length(table(dat[ , p])) == 2) {
      #   temp <- paste("<span style='padding-left:1000px'>", " ", names(table(dat[ , p], exclude = names(table(dat[ , p]))[1])),  "</span>", sep = "")
      #} else {
      temp <- paste("<span style='padding-left:1000px'>", " ", names(table(dat[ , p])), "</span>", sep = "")
      #}

      temp <- c(colnames(dat)[p], temp)
      ##### This version uses get_label() to use the labels of each variable as the row names instead of the variable names themselves, which are simple abbreviations
      # temp <- c(get_label(dat)[p], temp)

      # Row names for continuous variables include the variable name and then rows for the number of observations and the median, Q1, and Q3
    } else {

      temp <- c(colnames(dat)[p], paste("<span style='padding-left:1000px'>", " ", c("N", "Median (Q1, Q3)"), "</span>", sep = ""))
      # temp <- c(get_label(dat)[p], paste("<span style='padding-left:1000px'>", " ", c("N", "Median (Q1, Q3)"), "</span>", sep = ""))
    }

    rnames <- c(rnames, temp)

  }

  rounding <- dig

  get_summary <- function(x) {
    # x<-data$Female
    # For variables that are factors in the data frame
    if (is.factor(x)) {

      # Number of levels of x and number of levels of f
      nx <- length(table(x, useNA = "no"))


      #print(stat_method)
      res_tot <-paste(as.vector(table(x)),
                      "/",
                      sum(!is.na(x)),

                      " ",

                      "(",
                      round(as.vector(table(x) / sum(!is.na(x))*100), digits = rounding),
                      "%", ")",

                      sep = "")

      res_na <- paste(sum(is.na(x)),
                      "/",
                      sum(nrow(dat)),

                      " ",

                      "(",
                      round(sum(is.na(x) / sum(nrow(dat))*100), digits = rounding),
                      "%", ")",

                      sep = "")


      res <- cbind(
        c(NA, matrix(unlist(res_tot), byrow = FALSE)),
        c(NA, matrix(unlist(res_na), byrow = FALSE),rep(NA,times=length(c(NA, matrix(unlist(res_tot), byrow = FALSE)))-2)))

      # Continuous variables
    } else {

      count_tot <- sum(!is.na(x))
      median_tot <- round(median(x, na.rm=TRUE), digits = rounding)
      p25_tot <- round(quantile(x, probs = 0.25, na.rm = TRUE), digits = rounding)
      p75_tot <- round(quantile(x, probs = 0.75, na.rm = TRUE), digits = rounding)

      res_tot <- c(count_tot, paste(median_tot, " ", "(", p25_tot, ",", " ", p75_tot, ")", sep=""))

      res_na <- paste(sum(is.na(x)),
                      "/",
                      sum(nrow(dat)),

                      " ",

                      "(",
                      round(sum(is.na(x) / sum(nrow(dat))*100), digits = rounding),
                      "%", ")",

                      sep = "")


      # 2 is fixed based on the fact that the continuous variable will have a row for N and a row for the median (Q1, Q3)
      res <- cbind(
        c(NA, matrix(unlist(res_tot), byrow = FALSE)),
        c(NA, matrix(unlist(res_na), byrow = FALSE),rep(NA,times=length(c(NA, matrix(unlist(res_tot), byrow = FALSE)))-2)))



    } # End continuous variable

    # Save res in addition to the stat_method that was applied to each variable in the df
    list(res)

  }
  # Run the get_summary function
  res <- lapply(dat, get_summary)

  # Get the table summary by accessing the list object
  tableSummary <- do.call("rbind", lapply(res, function(y) y[[1]]))
  tableSummary <- rbind(c(toprow,rep(NA,length(tableSummary))-1), tableSummary)
  #
  colnames(tableSummary) <- c("","Missing")
  rownames(tableSummary) <- c("",rnames)
  #
  tableSummary[is.na(tableSummary)] <- ""

  rownames(tableSummary) <- gsub("<span style='padding-left:1000px'>","",rownames(tableSummary))
  rownames(tableSummary) <- gsub("<X.>","",rownames(tableSummary))
  rownames(tableSummary) <- gsub("..span.","",rownames(tableSummary))

  # Return
  tableSummary
}
