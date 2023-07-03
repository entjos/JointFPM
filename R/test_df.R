#' Test the fit of a JointFPM for a specific number of knots
#'
#' This is a helper function for `test_dfs_JointFPM`().
#'
#' @noRd
#'
#' @param surv
#'    See `JointFPM()` function documentation for detailed information.
#'
#' @param re_model
#'    See `JointFPM()` function documentation for detailed information.
#'
#' @param ce_model
#'    See `JointFPM()` function documentation for detailed information.
#'
#' @param re_indicator
#'    See `JointFPM()` function documentation for detailed information.
#'
#' @param ce_indicator
#'    See `JointFPM()` function documentation for detailed information.
#'
#' @param df_ce
#'    See `JointFPM()` function documentation for detailed information.
#'
#' @param df_re
#'    See `JointFPM()` function documentation for detailed information.
#'
#'@param tvc_re_terms
#'    See `JointFPM()` function documentation for detailed information.
#'
#' @param tvc_ce_terms
#'    See `JointFPM()` function documentation for detailed information.
#'
#' @param cluster
#'   See `JointFPM()` function documentation for detailed information.
#'
#' @param data
#'    See `JointFPM()` function documentation for detailed information.
#'
#' @return
#'    A `data.frame` with the following columns:
#'    \itemize{
#'      \item{`df_ce`: }{The number of knots for the baseline hazard function
#'      of the competing event,}
#'      \item{`df_re`: }{The number of knots for the baseline intensity function
#'      of the recurrent event,}
#'      \item{`aic`: }{The AIC value of the model fit,}
#'      \item{`bic`: }{The BIC value of the model fit,}
#'    }
#'
#' @import rstpm2

test_df <- function(surv,
                    re_model,
                    ce_model,
                    re_indicator,
                    ce_indicator,
                    df_ce,
                    df_re,
                    tvc_re_terms,
                    tvc_ce_terms,
                    cluster,
                    data){

  argument_list <- rlang::fn_fmls_syms()

  #Create model call which return NULL if model does not converge
  model_call <- function(){

    tryCatch(error = function(cnd) NULL,
             {
               do.call(JointFPM,
                       args = argument_list)
             })
  }

  model <- model_call()


  # Obtain AIC and BIC criteria
  if(is.null(model)){

    out <- data.frame(df_ce = df_ce,
                      df_re = df_re,
                      df_ce = tvc_ce_terms,
                      df_re = tvc_re_terms,
                      aic   = Inf,
                      bic   = Inf)

  } else {

    out <- data.frame(df_ce = df_ce,
                      df_re = df_re,
                      df_ce = tvc_ce_terms,
                      df_re = tvc_re_terms,
                      stats::AIC(model$model),
                      stats::BIC(model$model))
  }

  # Improve naming
  colnames(out) <- c("df_ce", "df_re", paste0("df_ce_", names(tvc_ce_terms)),
                     paste0("df_re_", names(tvc_re_terms)),
                     "AIC", "BIC")

  return(out)

}
