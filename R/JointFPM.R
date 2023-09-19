#' Joint FPMs for recurrent and competing events.
#'
#' @description
#' Fits a joint flexible parametric survival model (FPM) for a recurrent and
#' terminal event. The joint model can be used to predict the mean number of
#' events at different time points. This function is a wrapper around
#' `rstpm2::stpm2()`.
#'
#' @param surv
#'    A formula of the following form `Surv(...) ~ 1`.
#'    The `Surv` objects needs to be of `type ==  'counting'` with the
#'    following arguments:
#'    \itemize{
#'      \item{`time`: }{Start of follow-up time for each event episode, i.e.
#'      usually 0 for the competing event and the first occurrence of the
#'      recurrent event. For every subsequent event the follow-up can either
#'      be 0 if gap time is the underlying time scale or the time of the
#'      previous event if total time is the underlying time scale.}
#'      \item{`time2`: }{End of follow-up, i.e. either occurrence of a terminal
#'      or recurrent event, or time of censoring.}
#'      \item{`status`: }{Event indicator for both terminal and recurrent
#'      event.}
#'      \item{`type`: }{Has to be `counting`.}
#'    }
#'
#' @param re_model
#'    A formula object specifying the model for the recurrent event
#'    with an empty right hand side of the formula. E.g. `~ sex`.
#'
#' @param ce_model
#'    A formula object specifying the model for the competing event
#'    with an empty right hand side of the formula. E.g. `~ sex`.
#'
#' @param re_indicator
#'    Indicator that defined which rows in the dataset belong to the recurrent
#'    event process. These are usually more than one row per observations.
#'    The variable name needs to be passed as a character vector.
#'
#' @param ce_indicator
#'    Indicator that defined which row in the dataset belong to the competing
#'    event process. The variable name needs to be passed as a character vector.
#'
#' @param df_ce
#'    Defines the number of knots used to model the baseline hazard function
#'    for the competing event process.
#'
#' @param df_re
#'    Defines the number of knots used to model the baseline hazard function
#'    for the recurrent event process.
#'
#' @param tvc_re_terms
#'    A named list defining the number of knots used to model potential
#'    time-varying effects of variables included in the recurrent event model.
#'    This list should be of form `list(<var_name> = <no. of knots>)`.
#'
#' @param tvc_ce_terms
#'    A named list defining the number of knots used to model potential
#'    time-varying effects of variables included in the competing event model.
#'    This list should be of form `list(<var_name> = <no. of knots>)`.
#'
#' @param cluster
#'    A chacter vector specifying the name of the variable that defines unique
#'    observation in the dataset passed to the function.
#'
#' @param data
#'    A stacked dataset that including both data on the recurrent and competing
#'    event process. The dataset should have one row for each observation
#'    including the follow-up time and event indicator for the competing event
#'    and possibly multiple rows for each observation including the follow-up
#'    times and event indicator for the recurrent event, e.g.:
#'
#'    ```
#'    id st_start  st_end re status
#'     1      0      6.88  0      1
#'     1      0      6.88  1      0
#'     2      0      8.70  0      1
#'     2      0      8.70  1      0
#'     3      0     10     0      0
#'     3      0      1.78  1      1
#'     3      1.78   6.08  1      1
#'     3      6.08  10     1      0
#'     4      0      6.07  0      1
#'     4      0      6.07  1      0
#'    ```
#'
#' @return
#'    An object of class `JointFPM` with the following elements:
#'    \itemize{
#'      \item{`model`: }{The fitted FPM object,}
#'      \item{`re_terms`: }{The terms used to model the recurrent event model,}
#'      \item{`ce_terms`: }{The terms used to model the competing event model,}
#'      \item{`re_indicator`: }{The name of the indicator variable of the recurrent
#'      event}
#'      }
#'
#' @importFrom data.table `:=`
#' @import rstpm2
#'
#' @examples
#' \dontrun{
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
#'                        tvc_ce_terms = list(pyridoxine = 2,
#'                                            thiotepa   = 2),
#'                        tvc_re_terms = list(pyridoxine = 2,
#'                                            thiotepa   = 2),
#'                        cluster  = "id",
#'                        data     = bldr_stacked)
#'
#' predict(bldr_model,
#'         newdata = data.frame(pyridoxine = 1,
#'                              thiotepa   = 0),
#'         t =  c(10, 20, 50))
#' }
#'
#' @export JointFPM

JointFPM <- function(surv,
                     re_model,
                     ce_model,
                     re_indicator,
                     ce_indicator,
                     df_ce = 3,
                     df_re = 3,
                     tvc_re_terms = NULL,
                     tvc_ce_terms = NULL,
                     cluster,
                     data){
  # Prepare data
  time_var <- all.vars(surv)[[2]]

  ce_model_string <- paste0(labels(stats::terms(ce_model)), ":", ce_indicator,
                            collapse = " + ")

  re_model_string <- paste0(labels(stats::terms(re_model)), ":", re_indicator,
                            collapse = " + ")

  # Prepare tvc argument
  if(!is.null(tvc_re_terms)){

    tvc_re_terms <- paste0(names(tvc_re_terms), ":",
                           "nsx(log(", time_var, "),", tvc_re_terms, ")", ":",
                           re_indicator,
                           collapse = " + ")


  }

  if(!is.null(tvc_ce_terms)){

    tvc_ce_terms <- paste0(names(tvc_ce_terms), ":",
                           "nsx(log(", time_var, "),", tvc_ce_terms, ")", ":",
                           ce_indicator,
                           collapse = " + ")

  }

  comb_model_string <- paste("-1",
                             ce_indicator,
                             re_indicator,
                             sep = " + ")

  bh_formula_string <- paste0("nsx(log(", time_var, "),", df_ce, "):",
                              ce_indicator,
                              " + ",
                              "nsx(log(", time_var, "),", df_re, "):",
                              re_indicator)

  smooth_formula_sting <- paste(ce_model_string,
                                re_model_string,
                                bh_formula_string,
                                sep = " + ")

  tvc_formula_string <-  ""

  if(!is.null(tvc_ce_terms)) {

    tvc_formula_string <- tvc_ce_terms

  }

  if(!is.null(tvc_re_terms)) {

    tvc_formula_string <- paste0(tvc_formula_string,
                                 " + ",
                                 tvc_re_terms)

  }

  # Remove trailing plus
  tvc_formula_string <- gsub("^ \\+ ", "", tvc_formula_string)

  model_formula <- rlang::new_formula(rlang::f_lhs(surv),
                                      rlang::parse_expr(comb_model_string))

  tvc_formula <- rlang::new_formula(NULL, rlang::parse_expr(tvc_formula_string))
  bh_formula  <- rlang::new_formula(NULL, rlang::parse_expr(smooth_formula_sting))

  fpm <- rstpm2::stpm2(model_formula,
                       df             = df_ce,
                       smooth.formula = bh_formula,
                       tvc.formula = tvc_formula,
                       cluster = data[[cluster]],
                       robust  = TRUE,
                       data    = data)

  out <- list(model        = fpm,
              re_indicator = re_indicator,
              ce_indicator = ce_indicator)

  # Define class of output object
  class(out) <- "JointFPM"

  return(out)

}
