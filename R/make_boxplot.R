#' @title Boxplot for numeric by categorical variables
#' @description Create a boxplot to display the distribution of a numeric variable
#' grouped by a categorical variable. Good for categorical variable that has ten
#' or less categories. When the minimum observations for a category is < 200, each
#' observation will be plotted and outliers will be hidden, vice versa for observations
#' >= 200. Median for each category is labeled in the box.
#'
#' @param data dataset that contains the variables
#' @param num_var name of the numeric variable as a string
#' @param cat_var name of the categorical variable as a string
#' @param plot_title title for the plot as a string
#' @param vjust position of the median labels
#'
#' @return none
#' @examples
#' make_boxplot(ggplot2::diamonds, "price", "cut", "Price of Diamonds by Cuts")
#' @export

make_boxplot <- function(data, num_var, cat_var, plot_title, vjust = 1.5) {
  num_cat <- data %>%
    dplyr::count(!!as.symbol(cat_var)) %>%
    nrow()

  if (num_cat > 10) {
    stop("\U0001F611 Too many categories, try horizontal barplot!")
  } else {
    font_size <- switch(as.character(num_cat),
                        "2" = 4.3, "3" = 4.0, "4" = 3.7, "5" = 3.4,
                        "6" = 3.1, "7" = 2.8, "8" = 2.5, "9" = 2.1, "10" = 2)
    # "[:punct:]" are punctuation characters
    ylab <- stringr::str_to_title(stringr::str_replace(num_var, "[:punct:]", " "))

    obs <- data %>%
      dplyr::count(!!as.symbol(cat_var)) %>%
      dplyr::summarize(min = min(n)) %>%
      dplyr::pull()

    if (obs < 200) {
      p <- ggplot2::ggplot(data,
                      ggplot2::aes(x = get(cat_var), y = get(num_var), color = get(cat_var))) +
        ggplot2::geom_boxplot(outlier.shape = NA) +
        ggplot2::geom_jitter(alpha = 0.3)
    } else {
      p <- ggplot2::ggplot(data,
                      ggplot2::aes(x = get(cat_var), y = get(num_var), color = get(cat_var))) +
        ggplot2::geom_boxplot()
    }

    p + ggplot2::stat_summary(ggplot2::aes(label = ..y..), fun = "median",
                              geom = "text", size = font_size,
                              color = "black", vjust = vjust) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = "", y = ylab, title = plot_title)
  }
}
