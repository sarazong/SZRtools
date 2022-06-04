#' @title Counting Categories
#' @description Count the number of categories in a categorical variable, or count
#' the categories of multiple categorical variables.
#'
#' @param data dataset that contains the variables
#' @param cat_vars names of categorical variables in a character vector
#'
#' @return number of categories in the dataset
#' @examples
#' count_cats(ggplot2::diamonds, c("cut"))
#' count_cats(ggplot2::diamonds, c("cut", "color"))
#' @export

count_cats <- function(data, cat_vars) {
  num_cat <- data %>%
    dplyr::count(dplyr::across(dplyr::all_of(cat_vars))) %>%
    nrow()
  return(num_cat)
}


#' @title Counting Observations in a Category
#' @description Count the number of observations in the smallest/largest category
#' within a categorical variable, or the smallest/largest category defined by multiple
#' categorical variables.
#'
#' @param data dataset that contains the variables
#' @param cat_vars names of categorical variables in a character vector
#' @param fxn the min or max function
#'
#' @return number of observation
#' @examples
#' count_obs(ggplot2::diamonds, c("cut"), min)
#' count_obs(ggplot2::diamonds, c("cut", "color"), max)
#' @export

count_obs <- function(data, cat_vars, fxn = c(min, max)) {
  num_obs <- data %>%
    dplyr::count(dplyr::across(dplyr::all_of(cat_vars))) %>%
    dplyr::summarize(num_obs = fxn(n)) %>%
    dplyr::pull()

  return(num_obs)
}
