#' @title Chi-squared Test Result
#' @description Tidy the printout of a chi-squared test in a R markdown.
#'
#' @param test list of the result from a chi-squared test
#'
#' @return none
#' @export

print_chisq <- function(test) {
  cat(" \n",
      paste0("**", test$method, " of ",
            stringr::str_replace_all(test$data.name, "[a-z]*\\$", ""), "**"),
      " \n", names(test$statistic), "=", test$statistic,
      " \n", names(test$parameter), "=", test$parameter,
      " \n")
  if (test$p.value < 0.001) {
    cat(" p-value < 0.001 \n")
  } else {
    cat("p-value = ", test$p.value, " \n")
  }
}


#' @title Student's T-test Result
#' @description Tidy the printout of a student's t-test in a R markdown.
#'
#' @param test list of the result from a student's t-test
#'
#' @return none
#' @export

print_ttest <- function(test) {
  cat(" \n", paste0("**", test$method, " of ", test$data.name, "**"),
      " \n", names(test$statistic), "=", test$statistic, " \n")
  if (test$p.value < 0.001) {
    cat(" p-value < 0.001 \n")
  } else {
    cat(" p-value = ", round(test$p.value, 3), " \n")
  }
  cat(paste0(" ", names(test$estimate[1])), ":", round(test$estimate[1], 2), " \n",
      names(test$estimate[2]), ":", round(test$estimate[2], 2), " \n")
}
