#' @title Create barplot for categorical variable.
#' @description Create a barplot for a categorical variables with percent and count labels.
#'
#' @param data dataset that contains the categorical variables
#' @param cat_var name of a categorical variable to be plotted as a string
#' @param xlab a string that labels the x-axis
#'
#' @return non
#' @examples
#' make_barplot_cat(diamonds, "cut", "Cut of The Diamonds")
#' @export

make_barplot_cat <- function(data, cat_var, xlab) {
  num_cat <- data %>%
    dplyr::count(!!as.symbol(cat_var)) %>%
    dplyr::nrow()
  if (num_cat >= 4 && num_cat <= 8) {
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
    labs(x = xlab) +
    theme(legend.position = "none")
}
