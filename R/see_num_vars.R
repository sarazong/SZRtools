#' @title Histogram for Numeric Variable
#' @description Create a histogram to display the distribution of a numeric variable.
#'
#' @param data dataset that contains the variables
#' @param num_var name of the numeric variable as a string
#' @param bins number of bins, default to 30
#'
#' @return none
#' @examples
#' make_histogram(ggplot2::diamonds, "price")
#' @export

make_histogram <- function(data, num_var, bins = 30) {
  xlab <- stringr::str_to_title(stringr::str_replace(num_var, "[:punct:]", " "))

  ggplot2::ggplot(data, ggplot2::aes(x = get(num_var))) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                            bins = bins,
                            color = "black",
                            fill = "grey") +
    ggplot2::geom_density(fill = "red", alpha = 0.2) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = xlab,
                  title = paste0("Distribution of ", xlab))
}


#' @title Boxplot for Numeric by Categorical Variables
#' @description Create a boxplot to display the distribution of a numeric variable
#' grouped by a categorical variable. Good for categorical variable that has ten
#' or less categories. When the maximum observations for a category is < 200, each
#' observation will be plotted and outliers will be hidden, vice versa for
#' observations >= 200. Median for each category is labeled in the box.
#'
#' @param data dataset that contains the variables
#' @param num_var name of the numeric variable as a string
#' @param cat_var name of the categorical variable as a string
#' @param plot_title title for the plot as a string
#' @param vjust position of the median labels
#'
#' @return none
#' @examples
#' make_boxplot1(ggplot2::diamonds, "price", "cut", "Price of Diamonds by Cuts")
#' @export

make_boxplot1 <- function(data, num_var, cat_var, plot_title, vjust = 1.5) {
  num_cat <- szrtools::count_cats(data, c(cat_var))

  if (num_cat > 10) {
    stop("\U0001F611 Too many categories, need a customized boxplot!")
  } else {
    font_size <- switch(as.character(num_cat),
                        "2" = 4.3, "3" = 4.0, "4" = 3.7, "5" = 3.4,
                        "6" = 3.1, "7" = 2.8, "8" = 2.5, "9" = 2.1, "10" = 2)
    # "[:punct:]" are punctuation characters
    ylab <- stringr::str_to_title(stringr::str_replace(num_var, "[:punct:]", " "))

    obs <- szrtools::count_obs(data, c(cat_var), max)

    p <- ggplot2::ggplot(data, ggplot2::aes(x = get(cat_var),
                                            y = get(num_var),
                                            color = get(cat_var)))
    if (obs < 200) {
      p <- p + ggplot2::geom_boxplot(outlier.shape = NA) +
        ggplot2::geom_jitter(alpha = 0.3)
    } else {
      p <- p + ggplot2::geom_boxplot()
    }

    p + ggplot2::stat_summary(ggplot2::aes(label = ..y..), fun = "median",
                              geom = "text", size = font_size,
                              color = "black", vjust = vjust) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(x = "", y = ylab, title = plot_title)
  }
}


#' @title Boxplot for Numeric by Two Categorical Variables
#' @description Create a boxplot to compare the distribution of a numeric variable
#' grouped by a categorical variable among the different levels of another categorical
#' variable. Good for categorical variables that has ten or less categories. When
#' the maximum observations for a category is < 200, each observation will be plotted
#' and outliers will be hidden, vice versa for observations >= 200.
#'
#' @param data dataset that contains the variables
#' @param num_var name of the numeric variable as a string
#' @param cat_var name of the categorical variable as a string
#' @param grp_var name of the categorical variable as a string
#' @param label_fmt boolean to indicate whether plot labels should be formatted
#'
#' @return none
#' @examples
#' make_boxplot2(ggplot2::diamonds, "price", "cut", "color")
#' @export

make_boxplot2 <- function(data, num_var, cat_var, grp_var, label_fmt = TRUE) {
  num_cat1 <- szrtools::count_cats(data, c(cat_var))

  if (num_cat1 > 7) {
    stop("\U0001F611 Too many categories, need a customized boxplot!")
  } else {
    num_cat2 <- szrtools::count_cats(data, c(grp_var))
    total <- num_cat1 * num_cat2

    per_row <- switch(as.character(num_cat1),
                      "2" = 6, "3" = 5, "4" = 4, "5" = 3, "6" = 2, "7" = 2)
    num_rows <- ceiling(num_cat2 / per_row)

    obs <- szrtools::count_obs(data, c(cat_var, grp_var), max)

    if (label_fmt) {
      labels <- list(num_var, cat_var, grp_var) %>%
        lapply(stringr::str_replace, "[:punct:]", " ") %>%
        lapply(stringr::str_to_title)
      plot_title <- paste0(labels[[1]], " by ", labels[[2]], " and ", labels[[3]])
    } else {
      labels <- list(num_var, cat_var, grp_var)
      plot_title <- paste0(labels[[1]], " by ", labels[[2]], " and ", labels[[3]])
    }

    p <- ggplot2::ggplot(data, ggplot2::aes(x = get(cat_var),
                                            y = get(num_var),
                                            color = get(cat_var)))

    if (total <= 16) {
      if (obs < 200) {
        p <- p + ggplot2::geom_boxplot(outlier.shape = NA) +
          ggplot2::geom_jitter(alpha = 0.3) +
          ggplot2::facet_grid(~ get(grp_var))
      } else {
        p <- p + ggplot2::geom_boxplot() +
          ggplot2::facet_grid(~ get(grp_var))
      }
    } else {
      if (obs < 200) {
        p <- p + ggplot2::geom_boxplot(outlier.shape = NA) +
          ggplot2::geom_jitter(alpha = 0.3) +
          ggplot2::facet_wrap(~ get(grp_var), nrow = num_rows)
      } else {
        p <- p + ggplot2::geom_boxplot() +
          ggplot2::facet_wrap(~ get(grp_var), nrow = num_rows)
      }
    }

    p + ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(x = labels[[3]], y = labels[[1]],
                    title = plot_title, color = labels[[2]])
      # ggplot2::guides(color = ggplot2::guide_legend(title = legend))
      # ggplot2::annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1) +
      # ggplot2::annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1)
  }
}


#' @title Scatter Plot for Numeric Variables
#' @description Create a scatter plot to examine relationship between two numeric
#' variables.
#'
#' @param data dataset that contains the variables
#' @param num_var1 name of the numeric variable as a string
#' @param num_var2 name of the numeric variable as a string
#' @param label_fmt boolean to indicate whether plot labels should be formatted
#'
#' @return none
#' @examples
#' make_scatter_plot(ggplot2::diamonds, "price", "carat")
#' @export

make_scatter_plot <- function(data, num_var1, num_var2, label_fmt = TRUE) {

    if (label_fmt) {
      labels <- list(num_var1, num_var2) %>%
        lapply(stringr::str_replace, "[:punct:]", " ") %>%
        lapply(stringr::str_to_title)
      plot_title <- paste0("Scatter Plot for ", labels[[1]], " and ", labels[[2]])
    } else {
      labels <- list(num_var1, num_var2)
      plot_title <- paste0("Scatter Plot for ", labels[[1]], " and ", labels[[2]])
    }

    ggplot2::ggplot(data, ggplot2::aes(x = get(num_var1), y = get(num_var2))) +
      ggplot2::geom_point(alpha = 0.1) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = labels[[1]], y = labels[[2]], title = plot_title)
}
