#' Post-estimation function for JointFPMs
#'
#' Predicts different estimates from a joint flexible parametric model.
#' Currently only the estimation of the mean number of events at different
#' time points is supported.
#'
#' @param JointFPM
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
#' @param gauss_init_nodes
#'    Number of nodes used for the initial Gaussian quadrature approximation
#'    of the integral (default = 50):
#'    \deqn{E[N(t)] = \int_{0}^{t} S(t)\lambda(t)}
#'
#' @param gauss_max_iter
#'    The maximum number of iterations for the Gaussian quadrature
#'    (default = 5).
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
#' @import rstpm2
#'
#' @method predict JointFPM
#'
#' @export

predict.JointFPM <- function(JointFPM,
                             type = "mean_no",
                             newdata,
                             t,
                             exposed,
                             ci_fit = TRUE,
                             gauss_init_nodes = 50,
                             gauss_max_iter   = 5){

  if(type == "mean_no"){

    tmp_newdata <- create_newdata(newdata,
                                  re_term = JointFPM$re_terms,
                                  ce_term = JointFPM$ce_terms)

    gauss_nodes <- gauss_test_cnvrgnc(JointFPM$model, t,
                                      lambda_dta = tmp_newdata$lambda_dta,
                                      st_dta     = tmp_newdata$st_dta,
                                      gauss_init_nodes,
                                      gauss_max_iter)

    # Use Delta Method to obtain confidence intervals for E[N]

    if(ci_fit){

      est <- rstpm2::predictnl(JointFPM$model,
                               fun = function(obj, ...){

                                 calc_N(obj, t,
                                        lambda_dta = tmp_newdata$lambda_dta,
                                        st_dta     = tmp_newdata$st_dta,
                                        nodes      = gauss_nodes)

                               })

    } else {

      est <- calc_N(JointFPM$model, t,
                    lambda_dta = tmp_newdata$lambda_dta,
                    st_dta     = tmp_newdata$st_dta,
                    nodes      = gauss_nodes)

    }


  }

  if(type == "diff"){

    newdata_e0 <- create_newdata(newdata,
                                 re_term = JointFPM$re_terms,
                                 ce_term = JointFPM$ce_terms)

    newdata_e1 <- create_newdata(do.call(exposed, list(newdata)),
                                 re_term = JointFPM$re_terms,
                                 ce_term = JointFPM$ce_terms)

    gauss_nodes_e0 <- gauss_test_cnvrgnc(JointFPM$model, t,
                                         lambda_dta = newdata_e0$lambda_dta,
                                         st_dta     = newdata_e0$st_dta,
                                         gauss_init_nodes,
                                         gauss_max_iter)

    gauss_nodes_e1 <- gauss_test_cnvrgnc(JointFPM$model, t,
                                         lambda_dta = newdata_e1$lambda_dta,
                                         st_dta     = newdata_e1$st_dta,
                                         gauss_init_nodes,
                                         gauss_max_iter)

    # Use Delta Method to obtain confidence intervals for E[N]

    if(ci_fit){

      est <- rstpm2::predictnl(JointFPM$model,
                               fun = function(obj, ...){

                                 e0 <- calc_N(obj, t,
                                              lambda_dta = newdata_e0$lambda_dta,
                                              st_dta     = newdata_e0$st_dta,
                                              nodes      = gauss_nodes_e0)

                                 e1 <- calc_N(obj, t,
                                              lambda_dta = newdata_e1$lambda_dta,
                                              st_dta     = newdata_e1$st_dta,
                                              nodes      = gauss_nodes_e1)

                                 out <- e0-e1

                                 return(out)

                               })

    } else {

      e0 <- calc_N(JointFPM$model, t,
                   lambda_dta = newdata_e0$lambda_dta,
                   st_dta     = newdata_e0$st_dta,
                   nodes      = gauss_nodes_e0)

      e1 <- calc_N(JointFPM$model, t,
                   lambda_dta = newdata_e1$lambda_dta,
                   st_dta     = newdata_e1$st_dta,
                   nodes      = gauss_nodes_e1)

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


  colnames(out)[1] <- JointFPM$model@timeVar

  return(out)

}
