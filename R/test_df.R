#' Test the fit of a JointFPM for a specific number of knots
#'
#' This is a helper function for `test_dfs_JointFPM`().
#'
#' @noRd
#'
#' @param formula
#'    A `formula` passed to `rstpm2::stpm2()`.
#'
#' @param df_bh
#'    The number of knots for the baseline hazard function.
#'
#' @param df_tvc
#'    A list with the number of knots for the different time-varying effects.
#'
#' @param cluster
#'    A chacter vector specifying the name of the variable that defines unique
#'    observation in the dataset passed to the function.
#'
#' @param data
#'    A `data.frame` passed to `rstpm2::stpm2()`.
#'
#' @return
#'    A `data.frame` with the following columns:
#'    \itemize{
#'      \item{`df_bh`: }{The number of knots for the baseline hazard function,}
#'      \item{`df_tvs`: }{The number of knots for the different time-varying
#'      effects,}
#'      \item{`aic`: }{The AIC value of the model fit,}
#'      \item{`bic`: }{The BIC value of the model fit,}
#'    }

test_df <- function(formula,
                    df_bh,
                    df_tvc = NULL,
                    cluster,
                    data){

  argument_list <- list(formula = formula,
                        data    = data,
                        df      = df_bh,
                        cluster = cluster,
                        tvc     = NULL)

  # Add tvc argument if tvc > 0
  if(!is.null(df_tvc)){

    argument_list$tvc <- df_tvc

  } else {

    df_tvc <- 0

  }

  #Create model call which return NULL if model does not converge
  model_call <- function(){

    tryCatch(error = function(cnd) NULL,
             {
               do.call(rstpm2::stpm2,
                       args = argument_list)
             })
  }

  model <- model_call()


  # Obtain AIC and BIC criteria
  if(is.null(model)){

    out <- data.frame(df_bh  = df_bh,
                      df_tvc = df_tvc,
                      aic    = Inf,
                      bic    = Inf)

  } else {

    out <- data.frame(df_bh  = df_bh,
                      df_tvc = df_tvc,
                      aic    = stats::AIC(model),
                      bic    = stats::BIC(model))
  }

  return(out)

}
