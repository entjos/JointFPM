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
#' @return
#'    A numeric vector of integral with the same length as t.

calc_N <- function(obj, t, lambda_dta, st_dta) {

    if(0 %in% t) t[which(t == 0)] <- t[which(t == 0)] + .Machine$double.xmin

    N <- rmutil::int(
      f = function(x) predict_n(obj, t = x, lambda_dta, st_dta),
      a = 0,
      b = t
    )

  return(N)
}
