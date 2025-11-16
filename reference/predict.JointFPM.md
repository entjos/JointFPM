# Post-estimation function for JointFPMs

Predicts different estimates from a joint flexible parametric model.
Currently only the estimation of the mean number of events at different
time points is supported.

## Usage

``` r
# S3 method for class 'JointFPM'
predict(
  object,
  type = "mean_no",
  newdata,
  t,
  exposed = NULL,
  ci_fit = TRUE,
  method = "romberg",
  ngq = 30,
  ...
)
```

## Arguments

- object:

  A joint flexible parametric model of class `JointFPM`.

- type:

  A character vector defining the estimate of interest. Currently
  available options are:

  `mean_no`:

  :   Estimates the mean number of events at time(s) `t`.

  `diff`:

  :   Estimates the difference in mean number of events between exposed
      and unexposed at time(s) `t`.

  `marg_mean_no`:

  :   Estimates the marginal mean number of events.

  `marg_diff`:

  :   Estimates the marginal difference in the mean number of events.

- newdata:

  A `data.frame` with one row including the variable values used for t
  he prediction. One value for each variable used in either the
  recurrent or competing event model is required when predicting
  `mean_no` or `diff`. For `marg_mean_no` or `marg_diff`, this includes
  the variable that you would like your marginal estimate to be
  conditioned on.

- t:

  A vector defining the time points used for the prediction.

- exposed:

  A function that takes `newdata` as an argument and creates a new
  dataset for the exposed group. This argument is required if
  `type = 'diff'`. Please see details for more information.

- ci_fit:

  Logical indicator for whether confidence intervals should be estimated
  for the fitted estimates using the delta method.

- method:

  The method used for the underlying numerical integration procedure.
  Defaults to `"romberg"`, which uses the
  [`rmutil::int()`](https://rdrr.io/pkg/rmutil/man/int.html) function,
  but it is possible to use Gaussian quadrature by setting
  `method = "gq"` instead.

- ngq:

  Number of quadrature nodes used when `method = "gq"`. Defaults to 30,
  which lead to accurate results (compared to `method = "romberg"`) in
  our experience.

- ...:

  Added for compatibility with other predict functions.

## Value

A `data.frame` with the following columns:

- `t`: :

  The time for the prediction,

- `fit`: :

  The point estimate of the prediction,

- `lci`: :

  The lower confidence interval limit,

- `uci`: :

  The upper confidence interval limit.

## Details

The function required for the `exposed` argument must take the `newdata`
dataset as argument and transform it to a new dataset that defines the
exposed group. Assume we assume that we have a model with one variable
`trt` which is a 0/1 coded treatment indicator. If we would like to
obtain the difference in mean number of events comparing the untreated
to treated group we could use the following function assuming that
`newdata = data.frame(trt = 0)`:

    function(x){transform(x, trt = 1)}

## Examples

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
                       cluster  = "id",
                       data     = bladder1_stacked)

predict(bldr_model,
        newdata = data.frame(pyridoxine = 1,
                             thiotepa   = 0),
        t       =  c(10, 20),
        ci_fit  = FALSE)
#>   stop       fit
#> 1   10 0.6100286
#> 2   20 1.1260768
```
