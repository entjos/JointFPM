#' Test the fit of a JointFPM for a range number of knots
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
#' @param same_dfs_tvc
#'    If `TRUE` no combinations of DFs between the different tvc variables will
#'    be tested. Instead only DFs from the minimum DF specified until the
#'    largest specified DF in `dfs_tvc` will be tested for all variables
#'    specified in `dfs_tvc` at the same time.
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


test_dfs <- function(formula,
                     dfs_bh,
                     dfs_tvc,
                     same_dfs_tvc,
                     cluster,
                     data){

  if(is.null(dfs_tvc)){

    # Loop through dfs for baseline hazard
    out <- lapply(dfs_bh,
                  function(i){test_df(df_bh   = i,
                                      df_tvc  = NULL,
                                      formula = formula,
                                      cluster = cluster,
                                      data    = data)})

    out <- do.call(rbind, out)


  } else {

    if(same_dfs_tvc){

      # Create data frame of dfs when dfs are the same for baseline
      # hazard and tvc
      tmp <- expand.grid(dfs_bh  = dfs_bh,
                         dfs_tvc = min(unlist(dfs_tvc)):max(unlist(dfs_tvc)))

      tmp <- cbind(tmp[-2], stats::setNames(rep(data.frame(tmp$dfs_tvc),
                                                length(names(dfs_tvc))),
                                            names(dfs_tvc)))

    } else {

      # Create data frame of alls combinations for dfs of baseline hazard
      # and tvc
      tmp <- expand.grid(c(list(dfs_bh = dfs_bh),
                           dfs_tvc))
    }

    # Test fit for different dfs
    out <- lapply(seq_len(nrow(tmp)),
                  function(i){test_df(df_bh   = tmp$dfs_bh[[i]],
                                      df_tvc  = as.list(tmp[i, ])[-1],
                                      formula = formula,
                                      cluster = cluster,
                                      data    = data)})

    out <- do.call(rbind, out)
  }

  # Improve naming
  if(!any(grepl("df_tvc.*", colnames(out)))){

    out <- stats::setNames(out,
                           gsub(names(dfs_tvc),
                                paste0("dfs_tvc_", names(dfs_tvc)),
                                colnames(out),
                                fixed = TRUE))

  }

  out <- stats::setNames(out,
                         gsub("\\.", "_", colnames(out)))

  return(out)

}
