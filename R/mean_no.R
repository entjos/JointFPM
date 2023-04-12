#' Non-parametric estimation of mean number of events
#'
#' @param formula
#'    A formula passed to `survfit`. The formula needs to be of form
#'    `Surv() ~ 1`, where `Surv()` need to be of `type == 'counting'`.
#'
#' @param re_indicator
#'    The name of a variable indicating that these rows in the dataset belongs
#'    to the risksets for the recurrent event.
#'
#' @param ce_indicator
#'    The name of a variable indicating that these rows in the datasets belongs
#'    to the riskset for the competing event.
#'
#' @param data
#'    A `data.frame` in stacked format. The dataset needs to include one row
#'    for the competing event and one row for each risk episode of the
#'    recurrent event
#'
#' @param re_control
#'    An optional `list` with arguments passed to `survfit` when computing
#'    risksets for the recurrent event.
#'
#' @param ce_control
#'    An optional `list` with arguments passed to `survfit` when computing
#'    risksets for the competing event.
#'
#' @return
#'    A `data.frame` including the estimated number of events `expn`
#'    at times `t`.
#'
#' @import survival
#'
#' @export mean_no

mean_no <- function(formula,
                    re_indicator,
                    ce_indicator,
                    data,
                    re_control = list(),
                    ce_control = list()){

  # Define risk table for CB ---------------------------------------------------
  re_control$formula <- formula
  re_control$data    <- data[data[[re_indicator]] == 1, ]

  risktab_re <- do.call(survival::survfit,
                        args = re_control)

  # Define risk table for competing event --------------------------------------
  ce_control$formula <- formula
  ce_control$data    <- data[data[[ce_indicator]] == 1, ]

  risktab_ce <- do.call(survival::survfit,
                        args = ce_control)

  # Combine models -------------------------------------------------------------
  re_df <- data.table::data.table(time = risktab_re$time,
                                  na   = risktab_re$n.event / risktab_re$n.risk)

  ce_df <- data.table::data.table(time = risktab_ce$time,
                                  surv = risktab_ce$surv)

  data.table::setkey(re_df, time)
  data.table::setkey(ce_df, time)

  unique_times <- unique(c(re_df$time), ce_df$time)
  comb_df      <- re_df[ce_df[.(unique_times), on = "time"]]

  # Impute survival function
  if(is.na(comb_df$surv[[1]])) {

    # Set survival to 1 at the beginning
    comb_df$surv[[1]] <- 1

  }

  comb_df[, surv := data.table::nafill(surv, type = "locf")]

  # Estimate E[N(t)] -----------------------------------------------------------
  comb_df[, expn := cumsum(surv * na)]

  # Output ---------------------------------------------------------------------
  out <- data.frame(t = comb_df$time, expn = comb_df$expn)

  return(out)

}
