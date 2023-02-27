

gauss_test_cnvrgnc <- function(obj, t, lambda_dta, st_dta,
                               gauss_init_nodes, gauss_max_iter){

  # Test convergence of Gaussian quadrature
  N1 <- calc_N(obj, t, lambda_dta, st_dta, gauss_init_nodes)

  tmp_nodes <- gauss_init_nodes + 10

  N2 <- calc_N(obj, t, lambda_dta, st_dta, tmp_nodes)

  if (!identical(round(N1, 2), round(N2, 2))) {

    cat("Starting iteration for Gaussian quadrature: ")

    i <- 0

    while(!identical(round(N1, 4), round(N2, 4))){

      tmp_nodes <- tmp_nodes + 10

      cat(tmp_nodes, ", ")

      N1 <- N2

      N2 <- calc_N(obj, t, lambda_dta, st_dta, tmp_nodes)

      i <- i + 1

      if(i == gauss_max_iter){

        stop("Gaussian quadrature reached maximum no. of iterations without
             success. Change the `max_iter` argument and try again.")

      }
    }
    cat("\n")
  }

  cat("Convergence criteria reached.\n")

  return(tmp_nodes - 10)
}


