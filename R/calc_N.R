#' Estimation of the mean number of events using Gaussian quadrature
#'
#' This is a helper function for `predict.JointFPM`().
#'
#' @noRd
#'
#' @param obj
#'    A `stpm2` object extracted from a `JointFPM` model.
#'
#' @param newdata
#'    An internal argument for `rstpm2::predictnl()`.
#'
#' @param t
#'    A vector of times for which the estimate should be predicted.
#'
#' @param lambda_dta
#'    A `data.frame` used to predict the intensity function for the recurrent
#'    event process.
#'
#' @param st_dta
#'    A `data.frame` used to predict the survival function for the competing
#'    event process.
#'
#' @param nodes
#'    The number of nodes used for the Gaussian quadrature.
#'
#' @return
#'    A numeric vector of integral with the same length as t.

calc_N <- function(obj, newdata, t, lambda_dta, st_dta, nodes){

  N <- gaussian_quad(.f = function(x) predict_n(obj,
                                                t = x,
                                                lambda_dta,
                                                st_dta),
                     lower = 0,
                     upper = t,
                     nodes = nodes)

  return(N)

}
