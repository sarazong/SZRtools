#' @title Chi-squared Test Result
#' @description Print the result of a chi-squared test in a R markdown.
#'
#' @param test list of result from a chi-squared test
#'
#' @return none
#' @export

print_chisq <- function(test) {
  cat(" \n", paste("**", test$method, " of ",
                   stringr::str_replace_all(test$data.name, "[a-z]*\\$", ""), "**", sep = ""),
      " \n", names(test$statistic), "=", test$statistic,
      " \n", "p-value =", test$p.value,
      " \n")
}
