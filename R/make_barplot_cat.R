#' @title Barplot for categorical variable.
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
#' make_barplot_one(diamonds, "cut", "Cut of The Diamonds")
#' @export

make_barplot_one <- function(data, cat_var, xlab) {
  num_cat <- data %>%
    dplyr::count(!!as.symbol(cat_var)) %>%
    nrow()
  if (num_cat > 4 && num_cat <= 8) {
    font_size = 3
  } else {
    font_size = 3.88 # Default font size for geom_text()
  }

  ggplot(data, aes(get(cat_var), fill = get(cat_var))) +
    geom_bar(aes(y = ..count..)) +
    geom_text(
      aes(label = paste0(..count.., " (", scales::percent(..prop.., accuracy = 0.1), ")"),
          group = 1),
      stat = "count",
      size = font_size,
      vjust = -0.5) +
    coord_cartesian(clip = "off") +
    theme_classic() +
    theme(legend.position = "none") +
    labs(x = xlab)
}



#' @title Create a barplot for a categorical variable grouped by another categorical
#' variable.
#' @description Create a barplot for a categorical variable grouped by another
#' categorical variable, with percent and count labels. Good for variable that have
#' two to three categories while grouping variable has up to five categories.
#'
#' @param data dataset that contains the categorical variables
#' @param cat_var name of a categorical variable to be plotted as a string
#' @param grp_var name of a grouping categorical variable as a string
#' @param xlab a string that labels the x-axis
#'
#' @return none
#' @examples
#' make_barplot_two()
#' @export

make_barplot_two <- function(data, cat_var, grp_var, xlab) {
  num_cat <- data %>%
    dplyr::count(!!as.symbol(grp_var)) %>%
    nrow()
  if (num_cat > 3 && num_cat <= 5) {
    font_size = 2
  } else {
    font_size = 3
  }

  ggplot(data, aes(x = get(cat_var), fill = get(cat_var))) +
    geom_bar(aes(y = ..count..), position = "dodge") +
    geom_text(
      aes(label = paste0(..count.., " (",
                         scales::percent(..prop.., accuracy = 0.1), ")"),
          group = 1),
      size = font_size,
      stat = "count",
      vjust = -0.5
    ) +
    # Expand the y limits by 10% to display bar labels properly
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_grid(~ get(grp_var)) +
    scale_fill_discrete(name = cat_var) +
    labs(x = xlab)
}
