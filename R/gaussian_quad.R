#' Wrapper for `statmod::gauss.quad()`
#'
#' This is a helper function for `predict.JointFPM`().
#'
#' @noRd
#'
#' @param .f
#'    A `function` that is to be integrated.
#'
#' @param lower
#'    The lower bounds of the integral.
#'
#' @param upper
#'    The upper bounds of the integral.
#'
#' @param nodes
#'    The number of nodes used for the Gaussian quadrature.
#'
#' @return
#'    A numeric vector of integral with the same length as t.

# Function for integration using Gaussian Quadrature
gaussian_quad <- function(.f,
                          lower,
                          upper,
                          nodes){

  # Define weights
  w        <- statmod::gauss.quad(nodes)

  # Obtain length of integration interval
  diff     <- c(upper - lower)

  # Map x values to -1 to 1 spaceS
  x_trans  <- matrix(rep((1 + w$nodes) / 2,
                         length(diff)),
                     nrow = length(diff),
                     ncol = nodes,
                     byrow = TRUE) * diff

  # Apply function f to transformed x-values
  y <- apply(x_trans, c(1, 2), FUN = .f)

  # Estimate integral
  integral <- rowSums(t(w$weights * t(y)) * (diff/2))

  return(integral)

}
