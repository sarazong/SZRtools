#' @title Create barplots for categorical vars
#' @description Create a barplot for a categorical variable with percent and count
#' labels. Good for variable that has ten or less categories. Label size is adjusted
#' based on the number of categories.
#'
#' @template cat_var
#' @param label_fmt boolean to indicate whether plot labels should be formatted
#'
#' @return none
#' @examples
#' make_barplot1(ggplot2::diamonds, "cut")
#' @export

make_barplot1 <- function(data, cat_var, label_fmt = TRUE) {
  num_cat <- szrtools::count_cats(data, c(cat_var))

  if (label_fmt) {
    xlab <- stringr::str_to_title(stringr::str_replace(cat_var, "[:punct:]", " "))
    plot_title <- paste0("Distribution of ", xlab)
  } else {
    xlab <- cat_var
    plot_title <- paste0("Distribution of ", stringr::str_to_title(cat_var))
  }

  if (num_cat > 10) {
    stop("\U0001F611 Too many categories, try horizontal barplot!")
  } else {
    font_size <- switch(as.character(num_cat),
                        "2" = 4.3, "3" = 4.0, "4" = 3.7, "5" = 3.4,
                        "6" = 3.1, "7" = 2.8, "8" = 2.5, "9" = 2.1, "10" = 2)

    ggplot2::ggplot(data, ggplot2::aes(get(cat_var), fill = get(cat_var))) +
      ggplot2::geom_bar(ggplot2::aes(y = after_stat(count))) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(after_stat(count), " (", scales::percent(after_stat(prop), accuracy = 0.1), ")"),
          group = 1
          ),
        stat = "count",
        size = font_size,
        vjust = -0.5
        ) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = xlab, title = plot_title)
  }
}


#' @title Horizontal Barplot for Categorical Var
#' @description Create a horizontal barplot for a categorical variable with percent
#' and count labels. Good for variable that has greater than ten categories.
#'
#' @template cat_var
#' @param label_fmt boolean to indicate whether plot labels should be formatted
#'
#' @return none
#' @examples
#' make_hbarplot(ggplot2::diamonds, "cut")
#' @export

make_hbarplot <- function(data, cat_var, label_fmt = TRUE) {

  if (label_fmt) {
    xlab <- stringr::str_to_title(stringr::str_replace(cat_var, "[:punct:]", " "))
    plot_title <- paste0("Distribution of ", xlab)
  } else {
    xlab <- cat_var
    plot_title <- paste0("Distribution of ", stringr::str_to_title(cat_var))
  }

  levels <- as.character(data %>%
    dplyr::count(!!as.symbol(cat_var)) %>%
    dplyr::arrange(n) %>%
    dplyr::pull(!!as.symbol(cat_var)))
  data <- data %>%
    dplyr::mutate(!!as.symbol(cat_var) := factor(!!as.symbol(cat_var), levels = levels))

  font_size <- dplyr::if_else(length(levels) > 20, 3, 5)

  ggplot2::ggplot(data, ggplot2::aes(get(cat_var), fill = get(cat_var))) +
    ggplot2::geom_bar(ggplot2::aes(y = after_stat(count))) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(after_stat(count), " (", scales::percent(after_stat(prop), accuracy = 0.1), ")"),
        group = 1
      ),
      stat = "count",
      hjust = -0.05,
      size = font_size
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = xlab, title = plot_title) +
    ggplot2::coord_flip()
}


#' @title Barplot for Categorical by Categorical
#' @description Create a barplot for a categorical variable grouped by another
#' categorical variable, with percent and count labels. Good for plotting ten or
#' less total categories.
#'
#' @template cat_var
#' @param grp_var name of a grouping categorical variable as a string
#'
#' @return none
#' @export

make_barplot2 <- function(data, cat_var, grp_var) {
  # Count the total number of categories between cat_var and grp_var
  total <- szrtools::count_cats(data, c(cat_var,  grp_var))

  if (total > 12) {
    stop("\U0001F611 Too many categories, need to further customize the barplot!")
  } else {
    font_size <- switch(as.character(total),
                        "4" = 3.5, "6" = 3, "8" = 2.5, "9" = 2.1, "10" = 2, "12" = 2)


    vars <- list(cat_var, grp_var) %>%
      lapply(stringr::str_replace, "[:punct:]", " ") %>%
      lapply(stringr::str_to_title)
    plot_title <- paste0("Distribution of ", vars[[1]], " by ", vars[[2]], " Categories")

    ggplot2::ggplot(data, ggplot2::aes(x = get(cat_var), fill = get(cat_var))) +
      ggplot2::geom_bar(ggplot2::aes(y = after_stat(count)), position = "dodge") +
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(after_stat(count), " (", scales::percent(after_stat(prop), accuracy = 0.1), ")"),
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
      ggplot2::labs(x = "", title = plot_title)
  }
}
