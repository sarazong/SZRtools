#' @title Counting Categories in a Var
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


#' @title Labeling the Plot
#' @description Label the axses of a plot with formatted/unformatted variable names
#' and label the plot with a formatted/unformatted plot title.
#'
#' @param plot_type the type of plot used to visualize data in a string
#' @param ... variable names in strings
#' @param label_fmt Boolean to indicate whether plot labels should be formatted
#'
#' @return labels for a plot
#' @examples
#'label_plot("Scatter Plot", "price", "carat", "cut", label_fmt = TRUE)
#'
#' @export

label_plot <- function(plot_type, ..., label_fmt = TRUE) {
  # Convert variable names and plot type to title case when label_fmt is TRUE
  if (label_fmt ==  TRUE) {
    vars <- list(...) %>%
      lapply(stringr::str_replace, "[:punct:]", " ") %>%
      lapply(stringr::str_to_title)
    plot_type <- stringr::str_to_title(plot_type)
  } else {
    vars <- list(...)
    plot_type <- stringr::str_to_lower(plot_type)
  }

  num_vars <- length(list(...))
  if (num_vars == 2) {
    plot_title <- paste0(plot_type, " for ", vars[[1]], " and ", vars[[2]])
  } else {
    if (stringr::str_detect(plot_type, "[sS]catter")) {
      plot_title <- paste0(plot_type, " for ", vars[[1]], " and ", vars[[2]],
                           " by ", vars[[3]])
    } else {
      plot_title <- paste0(plot_type, " for ", vars[[1]], " by ", vars[[2]],
                           " and ", vars[[3]])
    }
  }

  return(list(vars, plot_title))
}
