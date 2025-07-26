#' Non-parametric estimation of mean number of events
#'
#' @description
#' Estimates the mean number of events using the non-parametric estimator
#' described by Cook and Lawless (1997).
#'
#' @param formula
#'    A formula passed to `survfit`.
#'
#' @param re_indicator
#'    The name of a variable indicating that these rows in the dataset belong
#'    to the risksets of the recurrent event process.
#'
#' @param ce_indicator
#'    The name of a variable indicating that these rows in the datasets belong
#'    to the riskset of the competing event process.
#'
#' @param data
#'    A `data.frame` in stacked format. The dataset needs to include one row
#'    for the competing event and one row for each risk episode of the
#'    recurrent event.
#'
#' @param re_control
#'    An optional `list` with arguments passed to `survfit` when computing
#'    risksets for the recurrent event.
#'
#' @param ce_control
#'    An optional `list` with arguments passed to `survfit` when computing
#'    risksets for the competing event.
#'
#' @param cluster
#'    A character vector specifying the name of the variable that defines unique
#'    observations in the dataset passed to the function. This is only used
#'    for data checks
#'
#' @return
#'    A `data.frame` including the estimated mean number of events `expn`
#'    at times `t` within strata `strata`.
#'
#' @import survival
#'
#' @examples
#' library(survival)
#'
#' mean_no(Surv(time  = start,
#'              time2 = stop,
#'              event = event,
#'              type  = 'counting') ~ pyridoxine + thiotepa,
#'         re_indicator = "re",
#'         ce_indicator = "ce",
#'         cluster = "id",
#'         data = bladder1_stacked)
#'
#' @export mean_no

mean_no <- function(formula,
                    re_indicator,
                    ce_indicator,
                    cluster,
                    data,
                    re_control = list(),
                    ce_control = list()){

  # Check user inputs ----------------------------------------------------------

  # Check that formula is of type formula
  if(!inherits(formula, "formula")){
    cli::cli_abort(
      c("x" = "{.code formula} is not a formula.")
    )
  }

  # Check that Surv is of type counting
  if(!any(grepl("counting", as.character(formula)))){
    cli::cli_abort(
      c("x" = "{.code surv} is not of type {.code counting}.",
        "i" = paste("Please check that you specified type == 'counting'",
                    "in your {.code Surv} object."))
    )
  }

  # Check that re and ce indicators exist in dataset
  if(!all(c(re_indicator, ce_indicator) %in% colnames(data))){
    cli::cli_abort(
      c("x" = paste("One or both of {.code re_indicator}, and",
                    "{.code ce_indicator} is/are not included in",
                    "{.code data}."))
    )
  }

  # Check that cluster variable exists
  if(!(cluster %in% colnames(data))){
    cli::cli_abort(
      c("x" = paste("{.code cluster} is not included in {.code data}."))
    )
  }

  # Check that data has at least two rows per id
  if(
    {
      n_min <- 2 * data.table::uniqueN(data[[cluster]])
      n_obs <- data.table::uniqueN(data,
                                   by = c(cluster, re_indicator, ce_indicator))
      n_min > n_obs
    }
  )
  {
    cli::cli_abort(
      c("x" = paste("{.code data} has at least one observation with less than",
                    "2 rows."),
        "i" = paste("Every observation should have at least 2 rows in the",
                    "stacked dataset: at least one row for the recurrent event,",
                    "and one row for the competing event. Please check your",
                    "dataset."))
    )
  }

  # Define variable shells for data.table --------------------------------------
  time <- n.event <- n.risk <- surv <- na <- expn <- strata <- NULL

  # Define risktable for recurrent event ---------------------------------------
  re_control$formula <- formula
  re_control$data    <- data[data[[re_indicator]] == 1, ]

  risktab_re <- do.call(survival::survfit,
                        args = re_control)

  risktab_re <- summary(risktab_re)

  # Extract risksets if strata is specified
  if(rlang::f_rhs(formula) != 1){

    risktab_re <- lapply(
      X = c(
        "time",
        "n.risk",
        "n.event",
        "surv",
        "strata"
        ),
      FUN = function(x) risktab_re[x]
    )

    # Extract risksets if no strata is specified
  } else if(rlang::f_rhs(formula) == 1) {

    risktab_re <- lapply(
      X = c(
        "time",
        "n.risk",
        "n.event",
        "surv"),
      FUN = function(x) risktab_re[x]
    )

    risktab_re$strata <- 1

  }

  risktab_re <- do.call(data.frame, risktab_re)
  data.table::setDT(risktab_re)

  # Define risk table for competing event --------------------------------------
  ce_control$formula <- formula
  ce_control$data    <- data[data[[ce_indicator]] == 1, ]

  risktab_ce <- do.call(survival::survfit,
                        args = ce_control)

  risktab_ce <- summary(risktab_ce)

  # Extract risksets if strata is specified
  if(rlang::f_rhs(formula) != 1){

    risktab_ce <- lapply(
      X = c(
        "time",
        "n.risk",
        "n.event",
        "surv",
        "strata"
        ),
      FUN = function(x) risktab_ce[x]
    )

    # Extract risksets if no strata is specified
  } else if(rlang::f_rhs(formula) == 1) {

    risktab_ce <- lapply(
      X = c(
        "time",
        "n.risk",
        "n.event",
        "surv"),
      FUN = function(x) risktab_ce[x]
    )

    # Set strata to 1 for further processing
    risktab_ce$strata <- 1

  }

  risktab_ce <- do.call(data.frame, risktab_ce)
  data.table::setDT(risktab_ce)

  # Combine models -------------------------------------------------------------
  stratas <- unique(risktab_re$strata)

  tmp <- lapply(stratas, function(x){

    re_df <- risktab_re[strata == x, list(time, na = n.event / n.risk)]
    ce_df <- risktab_ce[strata == x, list(time, surv)]

    # Union merge of RE and CE times (keepin all ts)
    comb_df <- merge(re_df,
                     ce_df,
                     by = "time",
                     all = TRUE)

    # Impute survival function
    if(is.na(comb_df$surv[[1]])) {

      # Set survival to 1 at the beginning
      comb_df$surv[[1]] <- 1

    }

    # Assuming a step function for the survival curve
    comb_df[, surv := data.table::nafill(surv, type = "locf")]
    # Filling missing Nelson-Aalen estimate with 0
    comb_df[, na   := data.table::nafill(na  , type = "const", fill = 0)]

    # Estimate E[N(t)] ---------------------------------------------------------
    comb_df[, expn := cumsum(surv * na)]
    comb_df[, strata := x]

  })

  # Output ---------------------------------------------------------------------
  data.table::rbindlist(tmp)

}
