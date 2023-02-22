#' Tests DFs for JointFPMs.
#'
#' Test of degrees of freedom (DFs) joint flexible parametric survival models
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
#'    A named list defining the numbers of knots used to model potential
#'    time-varying effects of variables included in the recurrent event model.
#'    This list should be of form `list(<var_name> = <no. of knots>)`.
#'
#' @param tvc_ce_terms
#'    A named list defining the numbers of knots used to model potential
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
#' @param by_vars
#'    A character vector of factor variable names used to fit stratified FPMs.
#'    One FPM will be fitted for each combination of factor levels specified.
#'    This is especially useful, when testing stratified FPMs.
#'
#' @param same_dfs_tvc
#'    If `TRUE` no combinations of DFs between the different tvc variables will
#'    be tested. Instead only DFs from the minimum DF specified until the
#'    largest specified DF in `dfs_tvc` will be tested for all variables
#'    specified in `dfs_tvc` at the same time.
#'
#' @return
#'    A `data.frame` with one row per combination of baseline hazards
#'    and tvc dfs and the corresponding AIC and BIC. If the `by_vars`
#'    argument is specified a `list` of `data.frame`s with one `data.frame`
#'    for each strata will be returned.
#'
#' @export test_dfs_JointFPM

test_dfs_JointFPM <- function(surv,
                              re_terms,
                              ce_terms,
                              re_indicator,
                              ce_indicator,
                              df_ce ,
                              tvc_re,
                              tvc_re_terms = NULL,
                              tvc_ce_terms = NULL,
                              cluster,
                              data,
                              same_dfs_tvc = FALSE,
                              by_vars = NULL){

  # Test model
  JointFPM_model <-
    JointFPM(surv,
             re_terms,
             ce_terms,
             re_indicator,
             ce_indicator,
             df_ce  = df_ce[[1]],
             tvc_re = df_ce[[1]],
             tvc_re_terms = lapply(tvc_re_terms, function(x)`[[`(x, 1)),
             tvc_ce_terms = lapply(tvc_ce_terms, function(x)`[[`(x, 1)),
             cluster,
             data)

  formula <- JointFPM_model$model@call.formula
  data    <- as.data.frame(JointFPM_model$model@data)

  # Prepare tvc argument
  if(!is.null(tvc_re_terms)){

    tvc_re_terms <- setNames(tvc_re_terms, paste0(names(tvc_re_terms), "_re"))

  }

  if(!is.null(tvc_ce_terms)){

    tvc_ce_terms <- setNames(tvc_ce_terms, paste0(names(tvc_ce_terms), "_ce"))

  }

  dfs_tvc     <- c(list(re = tvc_re),
                   tvc_re_terms,
                   tvc_ce_terms)

  # Check that tvc variables are icluded in dataset
  if(!all(names(dfs_tvc) %in% colnames(data))){

    stop("tvc variables need to be included in data")

  }

  if(is.null(by_vars)){

    test_dfs(formula, dfs_bh = df_ce, dfs_tvc, same_dfs_tvc, cluster, data)

    # Test DFs for stratified models
  } else {

    # If only one filter variables is selected
    if(length(by_vars) == 1){

      data$filter_vars <- data[, by_vars]

      # For more than one filter variable
    } else {

      data$filter_vars <- do.call(paste, data[, by_vars])

    }

    by(data,
       data$filter_vars,
       function(x) test_dfs(formula, dfs_bh = df_ce, dfs_tvc, same_dfs_tvc, x))
  }

}
