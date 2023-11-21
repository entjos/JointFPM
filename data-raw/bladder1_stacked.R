#' Stacked version of the bladder1 dataset included in the survival package
#'
#' This dataset includes the bladder1 dataset included the survival package,
#' which has been transformed into stacked format for use with `JointFPM`. The
#' stacked datset includes one row per individual for the competing event and
#' one rows per individual for each reoccurrence of bladder cancer.
#'
#' For more information please take a look at `?survival::bladder`.
#'
#' @docType data
#' @keywords datasets
#' @name bladder1_stacked
#' @usage
#' bladder1_stacked
#' data(bladder1_stacked)
#' @format A data frame with 412 rows and 11 columns

# Load data.table package for data manipulations
library(data.table)

# Load bladder cancer dataset from survival package
bladder1 <- as.data.table(survival::bladder1)
bladder1 <- bladder1[, .(id, treatment, size, start, stop, status)]

# Define dataset for competing event times
bladder1_ce <- bladder1[, .SD[stop == max(stop)],
                        by = id]

bladder1_ce[, `:=`(ce = 1,
                   re = 0,
                   event = as.numeric(status %in% 2:3),
                   start = 0)]

# Define dataset for bladder cancer recurrences
bladder1_re <- bladder1[,
                        `:=`(ce = 0,
                             re = 1,
                             event = as.numeric(status == 1))]

# Combine datasets into one stacked dataset

bladder1_stacked <- rbindlist(list(bladder1_ce, bladder1_re))

bladder1_stacked[, `:=`(pyridoxine = as.numeric(treatment == "pyridoxine"),
                        thiotepa   = as.numeric(treatment == "thiotepa"))]

bladder1_stacked$stop[bladder1_stacked$stop == 0] <- 1 # Add one day survival

usethis::use_data(bladder1_stacked, overwrite = TRUE)
