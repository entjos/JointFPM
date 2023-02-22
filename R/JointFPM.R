#' Joint FPMs for recurrent and competing events.
#'
#' Fits a joint flexible parametric survival model (FPM) for a recurrent and
#' terminal event. The joint model can be used to predict the mean number of
#' events at different time points. This function is a wrapper around
#' `rstpm2::stpm2()`.
#'
#' @param surv
#'    A `Surv` object as defined in the `survival` package passed in quotation
#'    marks. The `Surv` objects needs to be of `type ==  'counting'` with the
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
#' @param re_terms
#'    A character vector specifying the variables used  to model the recurrent
#'    event process. Please note that if you like to include interactions or
#'    higher order terms plase do so by creating a new variable in your dataset
#'    and pass the variable afterwards to `re_terms`.
#'
#' @param ce_terms
#'    A character vector specifying the variables used  to model the competing
#'    event process. Please note that if you like to include interactions or
#'    higher order terms plase do so by creating a new variable in your dataset
#'    and pass the variable afterwards to `ce_terms`.
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
#' @param tvc_re
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
#'    An object of class `JointFPM` with the follwing elements:
#'    \itemize{
#'      \item{`model`: }{The fitted FPM object,}
#'      \item{`re_terms`: }{The terms used to model the recurrent event model,}
#'      \item{`ce_terms`: }{The terms used to model the competing event model,}
#'      \item{`re_indicator`: }{The name of the indicator variable of the recurrent
#'      event}
#'      }
#'
#' @importFrom data.table `:=`
#'
#' @export JointFPM

JointFPM <- function(surv,
                     re_terms,
                     ce_terms,
                     re_indicator,
                     ce_indicator,
                     df_ce  = 3,
                     tvc_re = 3,
                     tvc_re_terms = NULL,
                     tvc_ce_terms = NULL,
                     cluster,
                     data){

  # Checks
  stopifnot(grepl("type.*=.*counting"),
            "The surv object needs to be of type counting.")

  # Prepare data
  data <-  data.table::copy(data.table::as.data.table(data))

  data[, paste0(re_terms, "_", re_indicator) := lapply(.SD, function(x)
    x * data[[re_indicator]]), .SDcols = re_terms]

  data[, paste0(re_terms, "_", ce_indicator) := lapply(.SD, function(x)
    x * data[[ce_indicator]]), .SDcols = ce_terms]

  # Prepare model formula
  model <- formula(paste(surv,
                         "~",
                         re_indicator,
                         "+",
                         paste0(re_terms, "_", re_indicator, collapse = "+"),
                         "+",
                         paste0(ce_terms, "_", ce_indicator, collapse = "+")))

  # Prepare tvc argument
  if(!is.null(tvc_re_terms)){

    tvc_re_terms <- stats::setNames(tvc_re_terms,
                                    paste0(names(tvc_re_terms), "_re"))

  }

  if(!is.null(tvc_ce_terms)){

    tvc_ce_terms <- stats::setNames(tvc_ce_terms,
                                    paste0(names(tvc_ce_terms), "_ce"))

  }

  tvc <- c(list(re = tvc_re),
           tvc_re_terms,
           tvc_ce_terms)

  fpm <- rstpm2::stpm2(model,
                       df      = df_ce,
                       tvc     = tvc,
                       cluster = data[[cluster]],
                       data    = data)

  out <- list(model        = fpm,
              re_terms     = re_terms,
              ce_terms     = ce_terms,
              re_indicator = re_indicator)

  # Define class of output object
  class(out) <- "JointFPM"

  return(out)
}
