
<!-- badges: start -->

[![R-CMD-check](https://github.com/entjos/JointFPM/workflows/R-CMD-check/badge.svg)](https://github.com/entjos/JointFPM/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/JointFPM)](https://CRAN.R-project.org/package=JointFPM)
[![](https://cranlogs.r-pkg.org/badges/JointFPM)](https://cran.r-project.org/package=JointFPM)
[![Codecov test coverage](https://codecov.io/gh/entjos/JointFPM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/entjos/JointFPM?branch=main)
<!-- badges: end -->

# Description

This package includes functions for parametrically estimating the mean
number of events in the presence of competing events. Researchers often
disregard subsequent events in the competing risk setting although the
event of interest might be of recurrent nature, e.g. heart attacks,
strokes, complications etc.. One estimate of interest in this situation
is the mean number of events in the presence of competing events, which
can be estimated using the functions provided in this package.

The estimation of the mean number of events requires two steps:

1.  The user need to fit a joint flexible parametric model using
    `JointFPM()`.
2.  The fitted model object needs to be passed to `predict()` in order
    to estimate the mean number of events.

Please note that this package is currently under development and might
change throughout the process.

# Installation

For installing the package from CRAN please use

``` r
install.packages("JointFPM")
```

If you would like to use the latest development version from GitHub
please use

``` r
remotes::install_github("entjos/JointFPM")
```

# Short Example

We will use a dataset of bladder cancer recurrences for the following
example. The dataset is included in the `{survival}` package and
includes information on bladder cancer patients receiving three
different treatments: placebo, Pyridoxine, and Thiotepa
(cf. `help(survival::bladder1)`).In order to fit a joint FPM we first
need to reshape the dataset into a stacked format, i.e. each observation
needs to have one row for the competing event and possible multiple rows
for the recurrent event. In the example below we use the `{data.table}`
package for the data preparation.

``` r
# Load packages
library(JointFPM)
library(data.table) # For data preparations

# Load bladder cancer dataset from survival package
bldr_df <- as.data.table(survival::bladder1)
bldr_df <- bldr_df[, .(id, treatment, start, stop, status)]

# Define dataset for competing event times
bldr_ce <- bldr_df[, .SD[stop == max(stop)],
                   by = id]

bldr_ce[, `:=`(ce = 1,
               re = 0,
               event = as.numeric(status %in% 2:3),
               start = 0)]

# Define dataset for bladder cancer recurrences
bldr_re <- bldr_df[,
                   `:=`(ce = 0,
                        re = 1,
                        event = as.numeric(status == 1))]

# Combine datasets into one stacked dataset

bldr_stacked <- rbindlist(list(bldr_ce, bldr_re))

bldr_stacked[, `:=`(pyridoxine = as.numeric(treatment == "pyridoxine"),
                    thiotepa   = as.numeric(treatment == "thiotepa"))]

bldr_stacked$stop[bldr_stacked$stop == 0] <- 1 # Add one day survival 

# Print stacked dataset
head(bldr_stacked)
```

       id treatment start stop status ce re event pyridoxine thiotepa
    1:  1   placebo     0    1      3  1  0     1          0        0
    2:  2   placebo     0    1      3  1  0     1          0        0
    3:  3   placebo     0    4      0  1  0     0          0        0
    4:  4   placebo     0    7      0  1  0     0          0        0
    5:  5   placebo     0   10      3  1  0     1          0        0
    6:  6   placebo     0   10      3  1  0     1          0        0

The next step is to fit a joint flexible parametric model using the
stacked dataset.

``` r
bldr_model <- JointFPM(Surv(time  = start,
                            time2 = stop, 
                            event = event,
                            type  = 'counting') ~ 1,
                       re_model = ~ pyridoxine + thiotepa,
                       ce_model = ~ pyridoxine + thiotepa,
                       re_indicator = "re",
                       ce_indicator = "ce",
                       df_ce = 3,
                       df_re = 3,
                       tvc_ce_terms = list(pyridoxine = 2,
                                           thiotepa   = 2),
                       tvc_re_terms = list(pyridoxine = 2,
                                           thiotepa   = 2),
                       cluster  = "id",
                       data     = bldr_stacked)
```

Based on the model we can predict the mean number of events at different
time points and covariate patterns. Please note the estimation of
confidence intervals for the mean number of events is computer
intensive. The following code might take some minutes to run on your
machine.

``` r
predict(bldr_model,
        newdata = data.frame(pyridoxine = 1, 
                             thiotepa   = 0),
        t =  c(10, 20, 50))
```

      stop       fit       lci       uci
    1   10 0.6068452 0.2866913 0.9269992
    2   20 1.1157394 0.5335082 1.6979705
    3   50 2.3793386 1.0346551 3.7240220

# Bugs

If you find any bugs or have any suggestion please don’t hesitate to
file an issue on GitHub.
