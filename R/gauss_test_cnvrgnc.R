#' Tests convergence of the Gaussian quadrature
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
#' @param gauss_init_nodes
#'    Number of nodes used for the initial Gaussian quadrature approximation
#'    of the integral. Passed from `predict.JointFPM()`.
#'
#' @param gauss_max_iter
#'    The maximum number of iterations for the Gaussian quadrature. Passed
#'    from `predict.JointFPM()`.
#'
#' @return
#'    An integer with the number of nodes that lead to convergence.

gauss_test_cnvrgnc <- function(obj, t, lambda_dta, st_dta,
                               gauss_init_nodes, gauss_max_iter){

  # Test convergence of Gaussian quadrature
  N1 <- calc_N(obj, t, lambda_dta, st_dta, gauss_init_nodes)

  tmp_nodes <- gauss_init_nodes + 10

  N2 <- calc_N(obj, t, lambda_dta, st_dta, tmp_nodes)

  if (!identical(round(N1, 2), round(N2, 2))) {

    cat("Starting iteration for Gaussian quadrature: ")

    i <- 0

    while(!identical(round(N1, 4), round(N2, 4))){

      tmp_nodes <- tmp_nodes + 10

      cat(tmp_nodes, ", ")

      N1 <- N2

      N2 <- calc_N(obj, t, lambda_dta, st_dta, tmp_nodes)

      i <- i + 1

      if(i == gauss_max_iter){

        stop("Gaussian quadrature reached maximum no. of iterations without
             success. Change the `max_iter` argument and try again.")

      }
    }
    cat("\n")
  }

  cat("Convergence criteria reached.\n")

  return(tmp_nodes - 10)
}


