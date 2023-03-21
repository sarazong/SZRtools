#' @title Create a Canvas for a Plot
#'@description Create a canvas for a plot with ggplot2

#' @param w width of the canvas
#' @param h height of the canvas

#' @return an empty canvas for drawing plots
#' @export

create_canvas <- function(w = 100, h = 100) {
  df <- tibble::tibble(x = seq(0, w, 10), y = seq(0, h, 10))

  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
    ggplot2::xlim(0, 0 + w) +
    ggplot2::ylim(0, 0 + h) +
    ggplot2::theme_classic()

  return(p)
}



#' @title Create a Ribbon Plot
#'@description Create a fun plot with geom_ribbon from ggplot2

#' @param p ggplot2 object where the ribbon plot should be added to
#' @param w width of the canvas
#' @param h height of the canvas
#' @param legend boolean indicating whether the legend should be displayed

#' @return a plot with ribbons
#' @export
create_ribbon <- function(p, w = 50, h = 30, legend = TRUE) {
  df <- tibble::tibble(x = c(w - 25, w + 25), y = c(50 + h / 2, 50 + h / 2))
  p <- p +
    ggplot2::geom_ribbon(ggplot2::aes(x = x, ymin = y - 10, ymax = y, fill = "Top"), df) +
    ggplot2::geom_ribbon(ggplot2::aes(x = x, ymin = y - 20, ymax = y - 10, fill = "Middle"), df) +
    ggplot2::geom_ribbon(ggplot2::aes(x = x, ymin = y - 30, ymax = y - 20, fill = "Bottom"), df) +
    ggplot2::scale_fill_manual(name = "Color",
                               # labels = c("x", "y", "z") labels each color
                               values = c("Top" = "black", "Middle" = "red", "Bottom" = "gold"))

  if (!legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}
