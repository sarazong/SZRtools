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
#' @param grp_var name of grouping variable for data summary calculation as a string
#' @param all boolean to indicate whether select all variables from the dataset
#' @param select list of variables names to be selected from the dataset in strings
#' @param labs boolean to indicate whether customized labels are being used in the
#' table
#' @param fmt_labs list of customized labels either in strings or in formula format
#'
#' @return none
#' @examples
#' make_tab1(ggplot2::diamonds, "cut")
#'
#' @export

make_tab1 <- function(data, grp_var,
                      all = TRUE, select = c(),
                      labs = FALSE, fmt_labs = c()) {

  if (!all) {
    if (length(select) == 0) stop("Please provide variables to be selected!")
  }

  if (labs) {
    if (length(fmt_labs) == 0) stop("Please provide customized labels for variables!")
  }

  if (!all) {
    select <- c(grp_var, select)
    data <- data %>%
      dplyr::select(all_of(select))
    }

  # Get all variables names except for the grouping variable
  vars <- names(data)[-which(names(data) == grp_var, arr.ind = TRUE)]
  pair <- vector("list", length(vars))

  if (labs) { # Use customized variable labels in the table
    if (typeof(fmt_labs[1]) == "character") {
      labels <- fmt_labs
      # When labels are strings, pair them with corresponding variables
      for (i in seq_along(vars)) {
        pair[[i]] <- as.formula(paste0(vars[i], " ~ ", paste0("'", labels[i], "'")))
        }
      } else {
        pair <- fmt_labs
      }
  } else { # Format variable names as labels
    labels <- stringr::str_to_title(stringr::str_replace(vars, "[:punct:]", " "))
    for (i in seq_along(vars)) {
      pair[[i]] <- as.formula(paste0(vars[i], " ~ ", paste0("'", labels[i], "'")))
    }
  }

  data %>%
    gtsummary::tbl_summary(
      by = grp_var, # row variable
      label = pair,
      missing_text = "Missing", # default is "Unknown"
    ) %>%
    gtsummary::modify_header(label ~ "**Variables**") %>%
    gtsummary::modify_spanning_header(
      gtsummary::all_stat_cols() ~ paste0("**", stringr::str_to_title(grp_var), "**")
    ) %>%
    gtsummary::bold_labels() %>%
    gtsummary::add_overall(last = TRUE)
}
