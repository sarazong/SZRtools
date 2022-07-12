#' @title Two by Two for Categorical Var
#' @description Create a nicely formatted 2x2 table for categorical variables.
#'
#' @param data dataset that contains the categorical variables
#' @param cat_var1 name of the categorical variable as a string
#' @param cat_var2 name of the categorical variable as a string
#'
#' @return none
#' @examples
#' make_xtab(ggplot2::diamonds, "cut", "color")
#' @export

make_xtab <- function(data, cat_var1, cat_var2) {
  data %>%
    dplyr::select(cat_var1, cat_var2) %>%
    gtsummary::tbl_summary(
      by = cat_var1, # row variable
      label = cat_var2 ~ stringr::str_to_title(cat_var2),
      missing_text = "Missing", # default is "Unknown"
      ) %>%
    gtsummary::modify_header(label ~ "**Variable**") %>%
    gtsummary::modify_spanning_header(
      gtsummary::all_stat_cols() ~ paste0("**", stringr::str_to_title(cat_var1), "**")
      ) %>%
    gtsummary::bold_labels() %>%
    gtsummary::add_overall(last = TRUE)
}



#' @title Table One
#' @description Create a nicely formatted table for variables of interest.
#'
#' @param data dataset that contains the variables of interest
#' @param  names of variables of interest as strings, with grouping variable being
#' the first
#'
#' @return none
#' @examples
#'
#' @export

make_tab1 <- function(data, grp_var,
                      all = TRUE, select = c(),
                      labs = FALSE, fmt_labs = list()) {

  if (!all) {
    if (length(select) == 0) stop("Please provide variables to be selected!")
  }

  if (labs) {
    if (length(fmt_labs) == 0) stop("Please provide customized labels for variables!")
  }

  if (!all) {
    data <- data %>%
      dplyr::select(all_of(select))
    }

  vars <- names(data)
  ind <- which(vars == grp_var, arr.ind = TRUE)
  pair <- vector("list", length(vars))

  if (labs) { # Use customized variable labels in the table
    labels <- fmt_labs
  } else { # Format variable names as labels
    labels <- stringr::str_to_title(stringr::str_replace(vars, "[:punct:]", " "))
  }

  for (i in seq_along(vars)) {
    pair[[i]] <- as.formula(paste0(vars[i], " ~ ", paste0("'", labels[i], "'")))
  }

  data %>%
    gtsummary::tbl_summary(
      by = grp_var, # row variable
      label = pair[-ind],
      missing_text = "Missing", # default is "Unknown"
    ) %>%
    gtsummary::modify_header(label ~ "**Variables**") %>%
    gtsummary::modify_spanning_header(
      gtsummary::all_stat_cols() ~ paste0("**", stringr::str_to_title(grp_var), "**")
    ) %>%
    gtsummary::bold_labels() %>%
    gtsummary::add_overall(last = TRUE)
}
