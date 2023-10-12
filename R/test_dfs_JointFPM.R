#' Tests DFs for JointFPMs.
#'
#' @description
#' Test of different degrees of freedoms (DFs) for joint flexible parametric
#' survival models.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param surv
#'    A formula of the following form `Surv(...) ~ 1`.
#'    The `Surv` objects needs to be of `type ==  'counting'` with the
#'    following arguments:
#'    \describe{
#'      \item{`time`: }{Start of follow-up time for each event episode, i.e.,
#'      usually 0 for the competing event and the first occurrence of the
#'      recurrent event. For every subsequent event the follow-up can either
#'      be 0 if gap time is the underlying time scale or the time of the
#'      previous event if total time is the underlying time scale.}
#'      \item{`time2`: }{End of follow-up, i.e., either occurrence of a terminal
#'      or recurrent event, or time of censoring.}
#'      \item{`status`: }{Event indicator for both terminal and recurrent
#'      event.}
#'      \item{`type`: }{Has to be `counting`.}
#' }
#'
#' @param re_model
#'    A formula object specifying the model for the recurrent event
#'    with an empty right hand side of the formula, e.g. `~ sex`.
#'
#' @param ce_model
#'    A formula object specifying the model for the competing event
#'    with an empty right hand side of the formula, e.g. `~ sex`.
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
#' @param dfs_ce
#'    Defines the number of knots used to model the baseline hazard function
#'    for the competing event process.
#'
#' @param dfs_re
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
#'    A chara vector specifying the name of the variable that defines unique
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
#'    A `data.frame` with one row per combination of baseline hazards DFs,
#'    and the DFs of the time varying covariates, and the corresponding
#'    AIC and BIC.
#'
#' @examples
#' # Test different dfs
#' test_dfs_JointFPM(Surv(time  = start,
#'                        time2 = stop,
#'                        event = event,
#'                        type  = 'counting') ~ 1,
#'                   re_model = ~ pyridoxine + thiotepa,
#'                   ce_model = ~ pyridoxine + thiotepa,
#'                   re_indicator = "re",
#'                   ce_indicator = "ce",
#'                   dfs_ce = 1:3,
#'                   dfs_re = 2,
#'                   tvc_ce_terms = list(thiotepa   = 1:2),
#'                   tvc_re_terms = list(pyridoxine = 2),
#'                   cluster  = "id",
#'                   data     = bladder1_stacked)
#'
#' @import rstpm2
#'
#' @export test_dfs_JointFPM

test_dfs_JointFPM <- function(surv,
                              re_model,
                              ce_model,
                              re_indicator,
                              ce_indicator,
                              dfs_ce ,
                              dfs_re,
                              tvc_re_terms = NULL,
                              tvc_ce_terms = NULL,
                              cluster,
                              data){

  # Test model
  JointFPM_model <-
    JointFPM(surv,
             re_model,
             ce_model,
             re_indicator,
             ce_indicator,
             df_ce = dfs_ce[[1]],
             df_re = dfs_re[[1]],
             tvc_re_terms = if(is.null(tvc_re_terms)) NULL else {
               lapply(tvc_re_terms, function(x)`[[`(x, 1))
             },
             tvc_ce_terms = if(is.null(tvc_ce_terms)) NULL else {
               lapply(tvc_ce_terms, function(x)`[[`(x, 1))
             },
             cluster,
             data)

  # Create data frame of alls combinations for dfs of baseline hazard
  # and tvc
  tmp <- expand.grid(c(list(dfs_ce = dfs_ce,
                            dfs_re = dfs_re),
                       tvc_ce_terms,
                       tvc_re_terms))

  # Test fit for different dfs
  out <- lapply(
    seq_len(nrow(tmp)),
    function(i){

      tvc_ce_terms <- if(is.null(tvc_ce_terms)) NULL else {
        as.list(tmp[i, 2 + seq_len(length(tvc_ce_terms)),
                    drop = FALSE])
      }

      tvc_re_terms <- if(is.null(tvc_re_terms)) NULL else {
        as.list(tmp[i, 2 + length(tvc_ce_terms) +
                      seq_len(length(tvc_re_terms)),
                    drop = FALSE])
      }

      test_df(surv,
              re_model,
              ce_model,
              re_indicator,
              ce_indicator,
              df_ce = tmp$dfs_ce[[i]],
              df_re = tmp$dfs_re[[i]],
              tvc_ce_terms = tvc_ce_terms,
              tvc_re_terms = tvc_re_terms,
              cluster,
              data)
    }
  )

  out <- do.call(rbind, out)

  return(out)

}
