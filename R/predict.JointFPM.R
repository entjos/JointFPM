#' Post-estimation function for JointFPMs
#'
#' @description
#' Predicts different estimates from a joint flexible parametric model.
#' Currently only the estimation of the mean number of events at different
#' time points is supported.
#'
#' @param object
#'    A joint flexible parametric model of class `JointFPM`.
#'
#' @param type
#'    A character vector defining the estimate of interest. Currently available
#'    options are:
#'    \itemize{
#'       \item{`mean_no`: }{Estimates the mean number of events at time(s) `t`.}
#'       \item{`diff`: }{Estimates the difference in mean number of events
#'       between exposed and unexposed at time(s) `t`.}
#'    }
#'
#' @param newdata
#'    A `data.frame` including the variable parameters for the prediction. One
#'    value for each variable used in either the recurrent or competing event
#'    model is requierd
#'
#' @param t
#'    A vector defining the time points for the prediction.
#'
#' @param exposed
#'    A function that takes `newdata` as an argument and creates a new dataset
#'    for the exposed group. This argument is required if `type = 'diff'`.
#'    Please see details for more information.
#'
#' @param ci_fit
#'    Logical indicator for whether confidence intervalls should be obtained
#'    for the fitted estimated using the delta method.
#'
#' @param ...
#'    Added for compatibility with other predict functions.
#'
#' @details
#'    The function required for the `exposed` argument must take the `newdata`
#'    dataset as argument and transform it to a new dataset that defines the
#'    exposed group. If we assume that we have a model with one variable `trt`,
#'    which is a 0/1 coded treatment indicator. If we would like to obtain
#'    the difference in mean number of events comparing untreated to treated
#'    we could use the following function assuming that
#'    `newdata = data.frame(trt = 0)`:
#'    ```
#'    function(x){transform(x, trt = 1)}
#'    ```
#'
#' @return
#'    A `data.frame` with the following columns:
#'    \itemize{
#'      \item{`t`: }{The time for the prediction,}
#'      \item{`fit`: }{The point estimate of the prediction,}
#'      \item{`lci`: }{The lower confidence interval limit,}
#'      \item{`uci`: }{The upper confidence interval limit.}
#'    }
#'
#' @examples
#' library(data.table) # For data preparations
#'
#' # Load bladder cancer dataset from survival package
#' bldr_df <- as.data.table(survival::bladder1)
#' bldr_df <- bldr_df[, .(id, treatment, start, stop, status)]
#'
#' # Define dataset for competing event times
#' bldr_ce <- bldr_df[, .SD[stop == max(stop)],
#'                    by = id]
#'
#' bldr_ce[, `:=`(ce = 1,
#'                re = 0,
#'                event = as.numeric(status %in% 2:3),
#'                start = 0)]
#'
#' # Define dataset for bladder cancer recurrences
#' bldr_re <- bldr_df[,
#'                    `:=`(ce = 0,
#'                         re = 1,
#'                         event = as.numeric(status == 1))]
#'
#' # Combine datasets into one stacked dataset
#'
#' bldr_stacked <- rbindlist(list(bldr_ce, bldr_re))
#'
#' bldr_stacked[, `:=`(pyridoxine = as.numeric(treatment == "pyridoxine"),
#'                     thiotepa   = as.numeric(treatment == "thiotepa"))]
#'
#' bldr_stacked$stop[bldr_stacked$stop == 0] <- 1 # Add one day survival
#'
#' # Print stacked dataset
#' head(bldr_stacked)
#'
#' bldr_model <- JointFPM(Surv(time  = start,
#'                             time2 = stop,
#'                             event = event,
#'                             type  = 'counting') ~ 1,
#'                        re_model = ~ pyridoxine + thiotepa,
#'                        ce_model = ~ pyridoxine + thiotepa,
#'                        re_indicator = "re",
#'                        ce_indicator = "ce",
#'                        df_ce = 3,
#'                        df_re = 3,
#'                        cluster  = "id",
#'                        data     = bldr_stacked)
#'
#' predict(bldr_model,
#'         newdata = data.frame(pyridoxine = 1,
#'                              thiotepa   = 0),
#'         t       =  c(10, 20),
#'         ci_fit  = FALSE)
#'
#' @import rstpm2
#'
#' @method predict JointFPM
#'
#' @export

predict.JointFPM <- function(object,
                             type = "mean_no",
                             newdata,
                             t,
                             exposed,
                             ci_fit = TRUE,
                             ...){

  if(type == "mean_no"){

    tmp_newdata <- list()

    tmp_newdata$st_dta <- cbind(newdata, 1, 0)
    colnames(tmp_newdata$st_dta) <- c(names(newdata),
                                      object$ce_indicator,
                                      object$re_indicator)

    tmp_newdata$lambda_dta <- cbind(newdata, 0, 1)
    colnames(tmp_newdata$lambda_dta) <- c(names(newdata),
                                          object$ce_indicator,
                                          object$re_indicator)

    # Use Delta Method to obtain confidence intervals for E[N]
    if(ci_fit){

      est <- rstpm2::predictnl(object$model,
                               fun = function(obj, ...){

                                 calc_N(obj, t,
                                        lambda_dta = tmp_newdata$lambda_dta,
                                        st_dta     = tmp_newdata$st_dta)

                               })

    } else {

      est <- calc_N(object$model, t,
                    lambda_dta = tmp_newdata$lambda_dta,
                    st_dta     = tmp_newdata$st_dta)

    }


  }

  if(type == "diff"){

    tmp_newdata_e0 <- list()

    tmp_newdata_e0$st_dta <- cbind(newdata, 1, 0)
    colnames(tmp_newdata_e0$st_dta) <- c(names(newdata),
                                         object$ce_indicator,
                                         object$re_indicator)

    tmp_newdata_e0$lambda_dta <- cbind(newdata, 0, 1)
    colnames(tmp_newdata_e0$lambda_dta) <- c(names(newdata),
                                             object$ce_indicator,
                                             object$re_indicator)

    tmp_newdata_e1 <- tmp_newdata_e0
    tmp_newdata_e1$st_dta     <- do.call(exposed, list(tmp_newdata_e1$st_dta))
    tmp_newdata_e1$lambda_dta <- do.call(exposed, list(tmp_newdata_e1$st_dta))

    # Use Delta Method to obtain confidence intervals for E[N]
    if(ci_fit){

      est <- rstpm2::predictnl(object$model,
                               fun = function(obj, ...){

                                 e0 <- calc_N(obj, t,
                                              lambda_dta = tmp_newdata_e0$lambda_dta,
                                              st_dta     = tmp_newdata_e0$st_dta)

                                 e1 <- calc_N(obj, t,
                                              lambda_dta = tmp_newdata_e1$lambda_dta,
                                              st_dta     = tmp_newdata_e1$st_dta)

                                 out <- e0-e1

                                 return(out)

                               })

    } else {

      e0 <- calc_N(object$model, t,
                   lambda_dta = tmp_newdata_e0$lambda_dta,
                   st_dta     = tmp_newdata_e0$st_dta)

      e1 <- calc_N(object$model, t,
                   lambda_dta = tmp_newdata_e1$lambda_dta,
                   st_dta     = tmp_newdata_e1$st_dta)

      est <- e0-e1
    }

  }

  if(ci_fit){

    cis <- rstpm2::confint.predictnl(est)

    out <- data.frame(t,
                      fit = est$fit,
                      lci = cis[, 1],
                      uci = cis[, 2])

  } else {

    out <- data.frame(t,
                      fit = est)

  }


  colnames(out)[1] <- object$model@timeVar

  return(out)

}
