#' @title Two by Two for Categorical Var
#' @description Create a nicely formatted 2x2 table for categorical variables.
#'
#' @param data dataset that contains the categorical variable
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
