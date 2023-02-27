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
#'    A character vector defining the estimant of interest. Currently the only
#'    option for this is "mean_no" for the mean number of events.
#'
#' @param newdata
#'    A `data.frame` including the variable parameters for the prediction. One
#'    value for each variable used in either the recurrent or competing event
#'    model is requierd
#'
#' @param t
#'    A vector defining the time points for the prediction.
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
                             gauss_init_nodes = 50,
                             gauss_max_nodes  = 5 ){

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
    est <- rstpm2::predictnl(JointFPM$model,
                             fun = function(obj, ...){

                               calc_N(obj, t,
                                      lambda_dta = tmp_newdata$lambda_dta,
                                      st_dta     = tmp_newdata$st_dta,
                                      nodes      = gauss_nodes)

                             })

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

  }

  cis <- rstpm2::confint.predictnl(est)

  out <- data.frame(t,
                    fit = est$fit,
                    lci = cis[, 1],
                    uci = cis[, 2])

  colnames(out)[1] <- JointFPM$model@timeVar

  return(out)

}
