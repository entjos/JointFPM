#' Summarises a `JointFPM` objects
#'
#' This is a summary function for `JointFPM` objects, created with
#' `JointFPM()`. The function improves the readability of the output.
#'
#' @param object
#'    An `JointFPM` object.
#'
#' @param ...
#'    Other arguments that should be passed to print.
#'
#' @return
#'    No return value, called for side effects.
#'
#' @method summary JointFPM
#'
#' @export

summary.JointFPM <- function(object, ...){

  # Preparing printing output
  estimates <- as.data.frame(summary(object$model)@coef)

  estimates_ce           <- estimates[grepl("^ce", rownames(estimates)), ]
  rownames(estimates_ce) <- gsub("^ce:", "", rownames(estimates_ce))
  rownames(estimates_ce) <- gsub("^ce", "(Intercept)", rownames(estimates_ce))

  estimates_re           <- estimates[grepl("^re", rownames(estimates)), ]
  rownames(estimates_re) <- gsub("^re:", "", rownames(estimates_re))
  rownames(estimates_re) <- gsub("^re", "(Intercept)", rownames(estimates_re))

  # Print model estimates
  cat("Coefficients CE model -----------------------------------------------")
  cat("---------------------\n")
  print(estimates_ce)
  cat("\n")

  cat("Coefficients RE model -----------------------------------------------")
  cat("---------------------\n")
  print(estimates_re)
  cat("\n")

  # Print log likelihood
  cat("-2 Log-likelihood:", -2*x$model@details$value)

}
