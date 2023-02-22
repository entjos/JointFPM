predict_n <- function(obj,
                      t, 
                      lambda_dta, 
                      st_dta){

  # Extend datasets for predicting different time points
  lambda_dta <- cbind(lambda_dta, re = 1, t)
  colnames(lambda_dta)[ncol(lambda_dta)] <- obj@timeVar
  
  st_dta <- cbind(st_dta, re = 0, t)
  colnames(st_dta)[ncol(st_dta)] <- obj@timeVar
  
  # Predict survival function
  surv   <- rstpm2::predict(obj, 
                            type = "surv",
                            newdata = st_dta)
  
  # Predict intensity function
  lambda <- rstpm2::predict(obj, 
                            type = "hazard",
                            newdata = lambda_dta)
  
  # Calculate n
  n <- surv * lambda
  
  return(n)
  
}

# Function for integration using Gaussian Quadrature
gaussian_quad <- function(.f,
                          lower,
                          upper,
                          nodes){
  
  # Define weights
  w        <- statmod::gauss.quad(nodes)
  
  # Obtain length of integration interval
  diff     <- c(upper - lower) 
  
  # Map x values to -1 to 1 spaceS
  x_trans  <- matrix(rep((1 + w$nodes) / 2, 
                         length(diff)), 
                     nrow = length(diff),
                     ncol = nodes,
                     byrow = TRUE) * diff
  
  # Apply function f to transformed x-values 
  y <- apply(x_trans, c(1, 2), FUN = .f)
  
  # Estimate integral
  integral <- rowSums(t(w$weights * t(y)) * (diff/2))
  
  return(integral)
  
}

calc_N <- function(obj, newdata, t, lambda_dta, st_dta, nodes){

  N <- gaussian_quad(.f = function(x) predict_n(obj, 
                                                t = x,
                                                lambda_dta, 
                                                st_dta),
                     lower = 0,
                     upper = t,
                     nodes = nodes)
  
  return(N)
  
}

predict.re_ce_fpm <- function(re_ce_obj, 
                              newdata, 
                              t, 
                              nodes_start = 50, 
                              max_iter    = 5){
  
  dta_re <- newdata[, colnames(newdata) %in% re_ce_obj$re_terms, drop = FALSE]
  dta_ce <- newdata[, colnames(newdata) %in% re_ce_obj$ce_terms, drop = FALSE]
  
  colnames(dta_re) <- paste0(colnames(dta_re), "_re")
  colnames(dta_ce) <- paste0(colnames(dta_ce), "_ce")
  
  tmp <- cbind(dta_re, dta_ce)
  
  # Create dataset for predicting the intensity function
  lambda_dta <- tmp * !grepl("_ce$", colnames(tmp))
  
  # Creating dataset for predicting the survival function
  st_dta <- tmp * !grepl("_re$", colnames(tmp))

  # Test convergence of Gaussian quadrature
  N1 <- calc_N(re_ce_obj$model, newdata, t,
               lambda_dta = lambda_dta,
               st_dta     = st_dta,
               nodes      = nodes_start)
  
  tmp_nodes <- nodes_start + 10
  
  N2 <- calc_N(re_ce_obj$model, newdata, t,
               lambda_dta = lambda_dta,
               st_dta     = st_dta,
               nodes      = tmp_nodes)
  
  if (!identical(round(N1, 2), round(N2, 2))) {
    
    cat("Starting iteration for Gaussian quadrature: ")
    
    i <- 0
    
    while(!identical(round(N1, 4), round(N2, 4))){
      
      tmp_nodes <- tmp_nodes + 10
      
      cat(tmp_nodes, ", ")
      
      N1 <- N2
      
      N2 <- calc_N(re_ce_obj$model, newdata, t,
                   lambda_dta = lambda_dta,
                   st_dta     = st_dta,
                   nodes      = tmp_nodes)
      
      i <- i + 1
      
      if(i == max_iter){
        stop("Gaussian quadrature reached maximum no. of iterations without 
             success. Change the `max_iter` argument and try again.")
      }
    }
    cat("\n")
  }
  
  cat("Convergence criteria reached.\n")
  
  # Use Delta Method to obtain confidence intervals for E[N]
  est <- predictnl(re_ce_obj$model, 
                   newdata = newdata,
                   fun = function(obj, newdata, ...) calc_N(obj, newdata, t,
                                                            lambda_dta = lambda_dta,
                                                            st_dta     = st_dta,
                                             nodes    = tmp_nodes - 10))
  
  cis <- confint(est)
  
  out <- data.frame(t,
                    fit = est$fit, 
                    lci = cis[, 1],
                    uci = cis[, 2])
  
  colnames(out)[1] <- re_ce_obj$model@timeVar
  
  return(out)
  
}
