# Non-parametric estimation of mean number of events

Estimates the mean number of events using the non-parametric estimator
described by Cook and Lawless (1997).

## Usage

``` r
mean_no(
  formula,
  re_indicator,
  ce_indicator,
  cluster,
  data,
  re_control = list(),
  ce_control = list()
)
```

## Arguments

- formula:

  A formula passed to `survfit`.

- re_indicator:

  The name of a variable indicating that these rows in the dataset
  belong to the risksets of the recurrent event process.

- ce_indicator:

  The name of a variable indicating that these rows in the datasets
  belong to the riskset of the competing event process.

- cluster:

  A character vector specifying the name of the variable that defines
  unique observations in the dataset passed to the function. This is
  only used for data checks

- data:

  A `data.frame` in stacked format. The dataset needs to include one row
  for the competing event and one row for each risk episode of the
  recurrent event.

- re_control:

  An optional `list` with arguments passed to `survfit` when computing
  risksets for the recurrent event.

- ce_control:

  An optional `list` with arguments passed to `survfit` when computing
  risksets for the competing event.

## Value

A `data.frame` including the estimated mean number of events `expn` at
times `t` within strata `strata`.

## Examples

``` r
library(survival)
mean_no(Surv(time  = start,
             time2 = stop,
             event = event,
             type  = 'counting') ~ pyridoxine + thiotepa,
        re_indicator = "re",
        ce_indicator = "ce",
        cluster = "id",
        data = bladder1_stacked)
#>       time         na      surv       expn                   strata
#>      <num>      <num>     <num>      <num>                   <fctr>
#>   1:     1 0.02083333 0.9583333 0.01996528 pyridoxine=0, thiotepa=0
#>   2:     2 0.08695652 0.9583333 0.10329861 pyridoxine=0, thiotepa=0
#>   3:     3 0.15217391 0.9583333 0.24913194 pyridoxine=0, thiotepa=0
#>   4:     5 0.04444444 0.9583333 0.29172454 pyridoxine=0, thiotepa=0
#>   5:     6 0.08888889 0.9583333 0.37690972 pyridoxine=0, thiotepa=0
#>  ---                                                               
#> 108:    44 0.10000000 0.7774462 2.21109295 pyridoxine=1, thiotepa=0
#> 109:    46 0.00000000 0.6802654 2.21109295 pyridoxine=1, thiotepa=0
#> 110:    47 0.16666667 0.6802654 2.32447052 pyridoxine=1, thiotepa=0
#> 111:    48 0.16666667 0.6802654 2.43784809 pyridoxine=1, thiotepa=0
#> 112:    49 0.20000000 0.6802654 2.57390117 pyridoxine=1, thiotepa=0
```
