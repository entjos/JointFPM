create_newdata <- function(newdata, re_terms, ce_terms){

  dta_re <- newdata[, colnames(newdata) %in% re_terms, drop = FALSE]
  dta_ce <- newdata[, colnames(newdata) %in% ce_terms, drop = FALSE]

  colnames(dta_re) <- paste0(colnames(dta_re), "_re")
  colnames(dta_ce) <- paste0(colnames(dta_ce), "_ce")

  tmp <- cbind(dta_re, dta_ce)

  # Create dataset for predicting the intensity function
  lambda_dta <- tmp * !grepl("_ce$", colnames(tmp))

  # Creating dataset for predicting the survival function
  st_dta <- tmp * !grepl("_re$", colnames(tmp))

  return(list(st_dta     = st_dta,
              lambda_dta = lambda_dta))

}
