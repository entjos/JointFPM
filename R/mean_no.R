#' Non-parametric estimation of mean number of events
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param formula
#'    A formula passed to `survfit`.
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
#'    A `data.frame` including the estimated mean number of events `expn`
#'    at times `t` within strata `strata`.
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

  # Define variable shells for data.table --------------------------------------
  time <- n.event <- n.risk <- surv <- na <- expn <- NULL

  # Define risktable for recurrent event ---------------------------------------
  re_control$formula <- formula
  re_control$data    <- data[data[[re_indicator]] == 1, ]

  risktab_re <- do.call(survival::survfit,
                        args = re_control)

  risktab_re <- summary(risktab_re)
  risktab_re <- lapply(c(2:4, 6, 10), function(x) risktab_re[x])
  risktab_re <- do.call(data.frame, risktab_re)
  data.table::setDT(risktab_re)

  # Define risk table for competing event --------------------------------------
  ce_control$formula <- formula
  ce_control$data    <- data[data[[ce_indicator]] == 1, ]

  risktab_ce <- do.call(survival::survfit,
                        args = ce_control)

  risktab_ce <- summary(risktab_ce)
  risktab_ce <- lapply(c(2:4, 6, 10), function(x) risktab_ce[x])
  risktab_ce <- do.call(data.frame, risktab_ce)
  data.table::setDT(risktab_ce)

  # Combine models -------------------------------------------------------------
  stratas <- unique(risktab_re$strata)

  tmp <- lapply(stratas, function(x){

    re_df <- risktab_re[strata == x, list(time, na = n.event / n.risk)]
    ce_df <- risktab_ce[strata == x, list(time, surv)]

    comb_df <- merge(re_df,
                     ce_df,
                     by = "time",
                     all = TRUE)

    # Impute survival function
    if(is.na(comb_df$surv[[1]])) {

      # Set survival to 1 at the beginning
      comb_df$surv[[1]] <- 1

    }

    comb_df[, surv := data.table::nafill(surv, type = "locf")]
    comb_df[, na   := data.table::nafill(na  , type = "const", fill = 0)]

    # Estimate E[N(t)] ---------------------------------------------------------
    comb_df[, expn := cumsum(surv * na)]
    comb_df[, strata := x]

  })

  # Output ---------------------------------------------------------------------
  data.table::rbindlist(tmp)

}
