#' Converts the `newdata` argument from `predict.JointFPM()` into a dataframe
#' that can be used for the prediction. This is especially includes creating
#' all the interaction terms requiered for the prediction call.
#'
#' This is a helper function for `predict.JointFPM()`.
#'
#' @noRd
#'
#' @param newdata
#'    A `data.frame` passed to the `newdata` argument of `predict.JointFPM()`.
#'
#' @param re_terms
#'    The variable name of the recurrent event indicator passed to the
#'    `re_term` argument of `predict.JointFPM()`.
#'
#' @param ce_terms
#'    The variable name of the competing event indicator passed to the
#'    `ce_term` argument of `predict.JointFPM()`.
#'
#' @return
#'    A `list` with two `data.frames`:
#'    \itemize{
#'       \item{`st_dta`: }{The dataset used for predicting the survival
#'       function.}
#'       \item{`lambda_dta`: }{The dataset used for predicting the intensity
#'       function.}
#'    }
#'
#' @import rstpm2

create_newdata <- function(newdata, re_terms, ce_terms){

  dta_re <- newdata[, colnames(newdata) %in% re_terms, drop = FALSE]
  dta_ce <- newdata[, colnames(newdata) %in% ce_terms, drop = FALSE]

  colnames(dta_re) <- paste0(colnames(dta_re), "_re")
  colnames(dta_ce) <- paste0(colnames(dta_ce), "_ce")

  tmp <- cbind(dta_re, dta_ce)

  # Create dataset for predicting the intensity function
  lambda_dta <- tmp * !grepl("_ce$", colnames(tmp))

  # Creating dataset for predicting the survival function
  st_dta <- tmp * !grepl("_re$", colnames(tmp))

  return(list(st_dta     = st_dta,
              lambda_dta = lambda_dta))

}
