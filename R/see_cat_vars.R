#' @title Barplot for Categorical Var
#' @description Create a barplot for a categorical variable with percent and count
#' labels. Good for variable that has ten or less categories. Label size is adjusted
#' based on the number of categories.
#'
#' @param data dataset that contains the categorical variable
#' @param cat_var name of the categorical variable as a string
#' @param xlab x-axis label as a string
#'
#' @return none
#' @examples
#' make_barplot1(ggplot2::diamonds, "cut", "Cut of The Diamonds")
#' @export

make_barplot1 <- function(data, cat_var, xlab) {
  num_cat <- szrtools::count_cats(data, c(cat_var))

  if (num_cat > 10) {
    stop("\U0001F611 Too many categories, try horizontal barplot!")
  } else {
    font_size <- switch(as.character(num_cat),
                        "2" = 4.3, "3" = 4.0, "4" = 3.7, "5" = 3.4,
                        "6" = 3.1, "7" = 2.8, "8" = 2.5, "9" = 2.1, "10" = 2)

    ggplot2::ggplot(data, ggplot2::aes(get(cat_var), fill = get(cat_var))) +
      ggplot2::geom_bar(ggplot2::aes(y = ..count..)) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(..count.., " (", scales::percent(..prop.., accuracy = 0.1), ")"),
          group = 1
          ),
        stat = "count",
        size = font_size,
        vjust = -0.5
        ) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = xlab)
  }
}


#' @title Barplot for Categorical by Categorical
#' @description Create a barplot for a categorical variable grouped by another
#' categorical variable, with percent and count labels. Good for plotting ten or
#' less total categories.
#'
#' @param data dataset that contains the categorical variables
#' @param cat_var name of a categorical variable to be plotted as a string
#' @param grp_var name of a grouping categorical variable as a string
#' @param xlab a string that labels the x-axis
#'
#' @return none
#' @export

make_barplot2 <- function(data, cat_var, grp_var, xlab) {
  total <- szrtools::count_cats(data, c(cat_var,  grp_var))

  if (total > 10) {
    stop("\U0001F611 Too many categories, need to further customize the barplot!")
  } else {
    font_size <- switch(as.character(total),
                        "4" = 3.5, "6" = 3, "8" = 2.5, "9" = 2.1, "10" = 2)

    ggplot2::ggplot(data, ggplot2::aes(x = get(cat_var), fill = get(cat_var))) +
      ggplot2::geom_bar(ggplot2::aes(y = ..count..), position = "dodge") +
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(..count.., " (", scales::percent(..prop.., accuracy = 0.1), ")"),
          group = 1
          ),
        size = font_size,
        stat = "count",
        vjust = -0.5
      ) +
      # Expand the y limits by 10% to display bar labels properly
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::facet_grid(~ get(grp_var)) +
      ggplot2::scale_fill_discrete(name = cat_var) +
      ggplot2::labs(x = xlab)
  }
}
