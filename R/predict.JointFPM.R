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
#' @param nodes_start
#'    Number of nodes used for the initial Gaussian quadrature approximation
#'    of the integral (default = 50):
#'    \deqn{E[N(t)] = \int_{0}^{t} S(t)\lambda(t)}
#'
#' @param max_iter
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
                             nodes_start = 50,
                             max_iter    = 5 ){

  dta_re <- newdata[, colnames(newdata) %in% JointFPM$re_terms, drop = FALSE]
  dta_ce <- newdata[, colnames(newdata) %in% JointFPM$ce_terms, drop = FALSE]

  colnames(dta_re) <- paste0(colnames(dta_re), "_re")
  colnames(dta_ce) <- paste0(colnames(dta_ce), "_ce")

  tmp <- cbind(dta_re, dta_ce)

  # Create dataset for predicting the intensity function
  lambda_dta <- tmp * !grepl("_ce$", colnames(tmp))

  # Creating dataset for predicting the survival function
  st_dta <- tmp * !grepl("_re$", colnames(tmp))

  # Test convergence of Gaussian quadrature
  N1 <- calc_N(JointFPM$model, newdata, t,
               lambda_dta = lambda_dta,
               st_dta     = st_dta,
               nodes      = nodes_start)

  tmp_nodes <- nodes_start + 10

  N2 <- calc_N(JointFPM$model, newdata, t,
               lambda_dta = lambda_dta,
               st_dta     = st_dta,
               nodes      = tmp_nodes)

  if (!identical(round(N1, 2), round(N2, 2))) {

    cat("Starting iteration for Gaussian quadrature: ")

    i <- 0

    while(!identical(round(N1, 4), round(N2, 4))){

      tmp_nodes <- tmp_nodes + 10

      cat(tmp_nodes, ", ")

      N1 <- N2

      N2 <- calc_N(JointFPM$model, newdata, t,
                   lambda_dta = lambda_dta,
                   st_dta     = st_dta,
                   nodes      = tmp_nodes)

      i <- i + 1

      if(i == max_iter){
        stop("Gaussian quadrature reached maximum no. of iterations without
             success. Change the `max_iter` argument and try again.")
      }
    }
    cat("\n")
  }

  cat("Convergence criteria reached.\n")

  # Use Delta Method to obtain confidence intervals for E[N]
  est <- rstpm2::predictnl(JointFPM$model,
                           newdata = newdata,
                           fun = function(obj, newdata, ...){

                             calc_N(obj, newdata, t,
                                    lambda_dta = lambda_dta,
                                    st_dta     = st_dta,
                                    nodes    = tmp_nodes - 10)

                           })

  cis <- rstpm2::confint.predictnl(est)

  out <- data.frame(t,
                    fit = est$fit,
                    lci = cis[, 1],
                    uci = cis[, 2])

  colnames(out)[1] <- JointFPM$model@timeVar

  return(out)

}
