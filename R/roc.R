#' @title ROC for Model Performance
#' @description Plot ROC curves for logistic regression models to assess model performance.
#' Curve is labeled with AUC and 95% CI.
#'
#' @param ... ROC objects
#' @param plot_title title for the plot as a string
#'
#' @return none
#' @export

plot_roc <- function(..., plot_title) {
  rocs <- list(...)
  # Name each ROC object with AUC and 95% CIs
  for (i in seq_along(rocs)) {
    names(rocs)[i] <- paste0("AUC of model ", i, ": ",
                             round(rocs[[i]][["ci"]][2], 2),
                             " (", round(rocs[[i]][["ci"]][1], 2),
                             ", ", round(rocs[[i]][["ci"]][3], 2), ")")
  }

  # Use linetype to differentiate curves "aes = 'linetype'"
  pROC::ggroc(rocs, legacy.axes = TRUE, size = 0.8) +
    ggplot2::labs(title = plot_title) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = c(1, 0.05),
                   legend.justification = c(1, 0.05))

}
