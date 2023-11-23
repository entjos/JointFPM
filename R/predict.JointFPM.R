#' Post-estimation function for JointFPMs
#'
#' @description
#' Predicts different estimates from a joint flexible parametric model.
#' Currently only the estimation of the mean number of events at different
#' time points is supported.
#'
#' @param object
#'    A joint flexible parametric model of class `JointFPM`.
#'
#' @param type
#'    A character vector defining the estimate of interest. Currently available
#'    options are:
#'    \describe{
#'       \item{`mean_no`: }{Estimates the mean number of events at time(s) `t`.}
#'       \item{`diff`: }{Estimates the difference in mean number of events
#'       between exposed and unexposed at time(s) `t`.}
#'       \item{`marg_mean_no`: }{Estimates the marginal mean number of events.}
#'       \item{`marg_diff`: }{Estimates the marginal difference in the mean
#'       number of events.}
#'    }
#'
#' @param newdata
#'    A `data.frame` with one row including the variable values used for t
#'    he prediction. One value for each variable used in either the recurrent or
#'    competing event model is required when predicting `mean_no` or `diff`.
#'    For `marg_mean_no` or `marg_diff`, this includes the variable
#'    that you would like your marginal estimate to be conditioned on.
#'
#' @param t
#'    A vector defining the time points used for the prediction.
#'
#' @param exposed
#'    A function that takes `newdata` as an argument and creates a new dataset
#'    for the exposed group. This argument is required if `type = 'diff'`.
#'    Please see details for more information.
#'
#' @param ci_fit
#'    Logical indicator for whether confidence intervals should be estimated
#'    for the fitted estimates using the delta method.
#'
#' @param ...
#'    Added for compatibility with other predict functions.
#'
#' @details
#'    The function required for the `exposed` argument must take the `newdata`
#'    dataset as argument and transform it to a new dataset that defines the
#'    exposed group. Assume we assume that we have a model with one variable
#'    `trt` which is a 0/1 coded treatment indicator. If we would like to obtain
#'    the difference in mean number of events comparing the untreated to treated
#'    group we could use the following function assuming that
#'    `newdata = data.frame(trt = 0)`:
#'    ```
#'    function(x){transform(x, trt = 1)}
#'    ```
#'
#' @return
#'    A `data.frame` with the following columns:
#'    \describe{
#'      \item{`t`: }{The time for the prediction,}
#'      \item{`fit`: }{The point estimate of the prediction,}
#'      \item{`lci`: }{The lower confidence interval limit,}
#'      \item{`uci`: }{The upper confidence interval limit.}
#'    }
#'
#' @examples
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
#'                        cluster  = "id",
#'                        data     = bladder1_stacked)
#'
#' predict(bldr_model,
#'         newdata = data.frame(pyridoxine = 1,
#'                              thiotepa   = 0),
#'         t       =  c(10, 20),
#'         ci_fit  = FALSE)
#'
#' @import rstpm2
#' @importFrom data.table `:=`
#'
#' @method predict JointFPM
#'
#' @export

predict.JointFPM <- function(object,
                             type = "mean_no",
                             newdata,
                             t,
                             exposed = NULL,
                             ci_fit = TRUE,
                             ...){

  # Initialize objects ---------------------------------------------------------
  tmp_newdata <- list()

  .SD <- .N <- ..pop_weights <- N <- NULL

  # Check user input -----------------------------------------------------------

  type <- match.arg(type, c("mean_no", "diff", "marg_mean_no", "marg_diff"))

  if(!inherits(newdata, "data.frame")){
    cli::cli_abort(
      c("x" = "{.code newdata} is not a {.code data.frame}.",
        "i" = "Please specify a {.code data.frame} as argument to {.code newdata}")
    )
  }

  if(nrow(newdata) > 1){
    cli::cli_abort(
      c("x" = "{.code newdata} has more than one row.",
        "i" = paste("Predictions are so far only supported for one covariate",
                    "pattern at the time. Hence, {.code newdata} is only",
                    "allowed to have one row. Please use multipe",
                    "{.code predict} calls if you want to have",
                    "Predictions for different covariate patterns.")
      )
    )
  }

  if(type %in% c("diff", "marg_diff")){

    if(is.null(exposed)){
      cli::cli_abort(
        c("x" = paste("You selecting prediction one of {.code diff}, or",
                      "{.code marg_diff} without specifing an exposed group",
                      "using the {.code exposed} argument."),
          "i" = paste("Please speficy a function defining your exposure",
                      "group in {.code exposed}"))
      )
    }

    if(!is.function(exposed)){
      cli::cli_abort(
        c("x" = "{.code exposed} is not a function.",
          "i" = "Please specify a function that defines your exposed group.")
      )
    }
  }

  if(type %in% c("mean_no", "diff")){
    temp_vars <- unique(c(all.vars(object$re_model),
                          all.vars(object$ce_model)))

    temp_vars <- temp_vars[!(temp_vars %in% colnames(newdata))]

    if(!identical(temp_vars, character(0))){
      cli::cli_abort(
        c("x" = "{temp_vars} {?is/are} not included in {.code newdata}.",
          "i" = "Please add {temp_vars} to newdata.")
      )
      rm(temp_vars)
    }
  }

  # Prepare data for prediction ------------------------------------------------

  # Additional set up for marginal predictions =================================
  if(type %in% c("marg_mean_no", "marg_diff")){

    vars <- unique(c(all.vars(object$re_model),
                     all.vars(object$ce_model),
                     object$cluster))

    target_pop <- data.table::as.data.table(object$model@data)[, .SD, .SDcols = vars]


    for(i in seq_along(colnames(newdata))){
      data.table::set(target_pop,
                      j = colnames(newdata)[[i]],
                      value = newdata[1,i])
    }

    newdata <- unique(target_pop, by = object$cluster)
    data.table::set(newdata, j = object$cluster, value = NULL)
    newdata <- newdata[, .N, by = names(newdata)]

    pop_weights <- newdata$N

    newdata[, N := NULL]

  }

  # Prepare data for RE model ==================================================

  tmp_newdata$st_dta <- cbind(newdata, 1, 0)
  colnames(tmp_newdata$st_dta) <- c(names(newdata),
                                    object$ce_indicator,
                                    object$re_indicator)

  # Prepare data for competing risk model ======================================
  tmp_newdata$lambda_dta <- cbind(newdata, 0, 1)
  colnames(tmp_newdata$lambda_dta) <- c(names(newdata),
                                        object$ce_indicator,
                                        object$re_indicator)

  # Additional set up for predictions of contrasts =============================
  if(type %in% c("diff", "marg_diff")){

    # Create new data for exposed group
    tmp_newdata_e1 <- tmp_newdata
    tmp_newdata_e1$st_dta     <- do.call(exposed, list(tmp_newdata_e1$st_dta))
    tmp_newdata_e1$lambda_dta <- do.call(exposed, list(tmp_newdata_e1$lambda_dta))

  }

  # Prediction of conditional mean number of evetns ----------------------------
  if(type == "mean_no"){

    # Use Delta Method to obtain confidence intervals for E[N]
    if(ci_fit){

      est <- rstpm2::predictnl(object$model,
                               fun = function(obj, ...){

                                 calc_N(obj, t,
                                        lambda_dta = tmp_newdata$lambda_dta,
                                        st_dta     = tmp_newdata$st_dta)

                               })

    } else {

      est <- calc_N(object$model, t,
                    lambda_dta = tmp_newdata$lambda_dta,
                    st_dta     = tmp_newdata$st_dta)

    }


  }

  # Prediction of conditional difference of mean number of events --------------
  if(type == "diff"){

    # Use Delta Method to obtain confidence intervals for E[N]
    if(ci_fit){

      est <- rstpm2::predictnl(
        object$model,
        fun = function(obj, ...){

          e0 <- calc_N(obj, t,
                       lambda_dta = tmp_newdata$lambda_dta,
                       st_dta     = tmp_newdata$st_dta)

          e1 <- calc_N(obj, t,
                       lambda_dta = tmp_newdata_e1$lambda_dta,
                       st_dta     = tmp_newdata_e1$st_dta)

          out <- e0-e1

          return(out)

        }
      )

    } else {

      e0 <- calc_N(object$model, t,
                   lambda_dta = tmp_newdata$lambda_dta,
                   st_dta     = tmp_newdata$st_dta)

      e1 <- calc_N(object$model, t,
                   lambda_dta = tmp_newdata_e1$lambda_dta,
                   st_dta     = tmp_newdata_e1$st_dta)

      est <- e0-e1
    }

  }

  # Prediction of marginal mean number of events -------------------------------
  if(type == "marg_mean_no"){

    # Use Delta Method to obtain confidence intervals for E[N]
    if(ci_fit){

      est <- rstpm2::predictnl(
        object$model,
        fun = function(obj, ...){

          temp_estimates <- lapply(
            seq_len(nrow(newdata)),
            function(i){
              N <- calc_N( obj, t,
                           lambda_dta = tmp_newdata$lambda_dta[i,],
                           st_dta     = tmp_newdata$st_dta[i, ])

              data.frame(t, N)
            }
          ) |> data.table::rbindlist()

          temp_estimates[,
                         list(est = stats::weighted.mean(N, ..pop_weights)),
                         by = t][["est"]]

        }
      )

    } else {

      temp_estimates <- lapply(
        seq_len(nrow(newdata)),
        function(i){
          N <- calc_N(object$model, t,
                      lambda_dta = tmp_newdata$lambda_dta[i,],
                      st_dta     = tmp_newdata$st_dta[i, ])

          data.frame(t, N)
        }
      ) |> data.table::rbindlist()

      est <- temp_estimates[,
                            list(est = stats::weighted.mean(N, ..pop_weights)),
                            by = t][["est"]]

    }

  }

  # Prediction of marginal difference in mean number of events -----------------
  if(type == "marg_diff"){

    # Use Delta Method to obtain confidence intervals for E[N]
    if(ci_fit){

      est <- rstpm2::predictnl(
        object$model,
        fun = function(obj, ...){

          temp_estimates <- lapply(
            seq_len(nrow(newdata)),
            function(i){

              e0 <- calc_N(obj, t,
                           lambda_dta = tmp_newdata$lambda_dta[i,],
                           st_dta     = tmp_newdata$st_dta[i,])

              e1 <- calc_N(obj, t,
                           lambda_dta = tmp_newdata_e1$lambda_dta[i,],
                           st_dta     = tmp_newdata_e1$st_dta[i,])

              data.frame(t, diff = e0-e1)

            }) |> data.table::rbindlist()

          temp_estimates[,
                         list(est = stats::weighted.mean(diff, ..pop_weights)),
                         by = t][["est"]]

        })

    } else {

      temp_estimates <- lapply(
        seq_len(nrow(newdata)),
        function(i){

          e0 <- calc_N(object$model, t,
                       lambda_dta = tmp_newdata$lambda_dta[i,],
                       st_dta     = tmp_newdata$st_dta[i,])

          e1 <- calc_N(object$model, t,
                       lambda_dta = tmp_newdata_e1$lambda_dta[i,],
                       st_dta     = tmp_newdata_e1$st_dta[i,])

          data.frame(t, diff = e0-e1)

        }) |> data.table::rbindlist()

      est <- temp_estimates[,
                            list(est = stats::weighted.mean(diff, ..pop_weights)),
                            by = t][["est"]]

    }

  }

  # Prepare output -------------------------------------------------------------
  if(ci_fit){

    cis <- rstpm2::confint.predictnl(est)

    out <- data.frame(t,
                      fit = est$fit,
                      lci = cis[, 1],
                      uci = cis[, 2])

  } else {

    out <- data.frame(t,
                      fit = est)

  }


  colnames(out)[1] <- object$model@timeVar

  return(out)

}
