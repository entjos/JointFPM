#' Estimation of the derivative of the mean number of events
#'
#' This is a helper function for `predict.JointFPM`().
#'
#' @noRd
#'
#' @param obj
#'    A `stpm2` object extracted from a `JointFPM` model.
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
#'    A numeric vector the same length as t including the estimates of the
#'    derivative of E[N(t)].
#'
#' @import rstpm2

predict_n <- function(obj,
                      t,
                      lambda_dta,
                      st_dta){

  # Extend datasets for predicting different time points
  lambda_dta <- cbind(lambda_dta, t)
  colnames(lambda_dta)[ncol(lambda_dta)] <- obj@timeVar

  st_dta <- cbind(st_dta, t)
  colnames(st_dta)[ncol(st_dta)] <- obj@timeVar

  # Predict survival function
  surv   <- rstpm2::predict(obj,
                            type = "surv",
                            newdata = st_dta)

  # Predict intensity function
  lambda <- rstpm2::predict(obj,
                            type = "hazard",
                            newdata = lambda_dta)

  # Setting lambda to 0 in case of negative prediction for the hazard
  lambda <- ifelse(lambda <= 0, 0, lambda)

  # Calculate n
  n <- surv * lambda

  return(n)

}
