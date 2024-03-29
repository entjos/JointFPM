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

calc_N <- function(obj, t, lambda_dta, st_dta, method, gq = NULL) {

  t <- pmax(t, .Machine$double.xmin)

  if (method == "romberg") {

    N <- rmutil::int(
      f = function(x) predict_n(obj, t = x, lambda_dta, st_dta),
      a = 0,
      b = t
    )

  } else if (method == "gq") {

    # Define weight matrix
    wmat <- matrix(data = gq$weights,
                   ncol = length(t),
                   nrow = length(gq$weights),
                   byrow = FALSE)

    # Define node matrix
    xmat <- matrix(data = gq$nodes,
                   ncol = length(t),
                   nrow = length(gq$nodes),
                   byrow = FALSE)

    # Transform time to -1 to 1 scale
    scalerv <- t / 2

    scalerm <- matrix(data = scalerv, # Both (b - a) / 2 and (a + b) / 2
                      ncol = length(t),
                      nrow = nrow(xmat),
                      byrow = TRUE)

    xmat <- scalerm * xmat + scalerm

    xmatv <- as.vector(xmat)

    # Predict mean number of events at node points
    fval <- predict_n(obj, t = xmatv, lambda_dta, st_dta)

    fval <- matrix(data = fval,
                   nrow = nrow(xmat),
                   ncol = ncol(xmat),
                   byrow = FALSE)

    # Sum weighted mean number of events at node points
    N <- scalerv * matrixStats::colSums2(x = wmat * fval)

  } else {

    stop("Integration method not supported.",
         call. = FALSE)

  }

  return(N)

}
