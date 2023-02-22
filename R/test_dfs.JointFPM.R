#' Tests DFs for FPMs
#'
#' Test of degrees of freedom (DFs) for Flexible Parametric Survival Models
#' (FPMs).
#'
#' @param formula
#'    A R-formula that is passed to the stpm2 call.
#'
#' @param dfs_bh
#'    A vector of DFs to be tested for the baseline hazard function.
#'
#' @param dfs_tvc
#'    A list of variables with a vector of DFs that should be tested for
#'    time varying effects, e.g. `list(case = 1:10, female = 1:3)`.
#'
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
#' @param data
#'    A data set that should be used to fit the FPM.
#'
#' @return
#'    A `data.frame` with one row per combination of baseline hazards
#'    and tvc dfs and the corresponding AIC and BIC. If the `by_vars`
#'    argument is specified a `list` of `data.frame`s with one `data.frame`
#'    for each strata will be returned.
#'
#' @import rstpm2
#'
#' @export test_dfs.JointFPM
#'
#' @examples
#' require(rstpm2)
#'
#' data(brcancer)
#' data(colon)
#'
#' fpm_test_dfs(Surv(rectime, censrec) ~ hormon + x1,
#'    dfs_bh  = 1:5,
#'    dfs_tvc = list(hormon = 1:3,
#'                   x1     = 1:5),
#'    data = brcancer)
#'
#' fpm_test_dfs(Surv(rectime, censrec) ~ hormon + x1,
#'    dfs_bh  = 1:5,
#'    dfs_tvc = list(hormon = 1:3,
#'                   x1     = 1:5),
#'    same_dfs_tvc = TRUE,
#'    data = brcancer)
#'
#' fpm_test_dfs(Surv(surv_mm, status == "Dead: cancer") ~ age + year8594,
#'              dfs_bh  = 1:5,
#'              dfs_tvc = list(age      = 1:5,
#'                             year8594 = 1:5),
#'              by_vars = c("sex", "stage"),
#'              same_dfs_tvc = TRUE,
#'              data = colon)

test_dfs.JointFPM <- function(surv,
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
  re_ce_fpm_model <- 
    re_ce_fpm(surv,
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
  
  formula <- re_ce_fpm_model$model@call.formula
  data    <- as.data.frame(re_ce_fpm_model$model@data)
  
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
  
  # # Test if model converges with 1 df
  # rstpm2::stpm2(formula = formula,
  #               data    = data,
  #               df      = 1,
  #               tvc     = NULL,
  #               cluster = cluster)
  # 
  # Test DFs for non-stratified models
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

test_df <- function(formula,
                    df_bh,
                    df_tvc = NULL,
                    cluster,
                    data){
  
  argument_list <- list(formula = formula,
                        data    = data,
                        df      = df_bh,
                        cluster = cluster,
                        tvc     = NULL)
  
  # Add tvc argument if tvc > 0
  if(!is.null(df_tvc)){
    
    argument_list$tvc <- df_tvc
    
  } else {
    
    df_tvc <- 0
    
  }
  
  #Create model call which return NULL if model does not converge
  model_call <- function(){
    
    tryCatch(error = function(cnd) NULL,
             {
               do.call(rstpm2::stpm2,
                       args = argument_list)
             })
  }
  
  model <- model_call()
  
  
  # Obtain AIC and BIC criteria
  if(is.null(model)){
    
    out <- data.frame(df_bh  = df_bh,
                      df_tvc = df_tvc,
                      aic    = Inf,
                      bic    = Inf)
    
  } else {
    
    out <- data.frame(df_bh  = df_bh,
                      df_tvc = df_tvc,
                      aic    = stats::AIC(model),
                      bic    = stats::BIC(model))
  }
  
  return(out)
  
}