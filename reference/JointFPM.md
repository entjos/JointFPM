# Joint FPMs for recurrent and competing events.

Fits a joint flexible parametric survival model (FPM) for a recurrent
and terminal event. The joint model can be used to predict the mean
number of events at different time points. This function is a wrapper
around [`rstpm2::stpm2()`](https://rdrr.io/pkg/rstpm2/man/gsm.html).

## Usage

``` r
JointFPM(
  surv,
  re_model,
  ce_model,
  re_indicator,
  ce_indicator,
  df_ce = 3,
  df_re = 3,
  tvc_re_terms = NULL,
  tvc_ce_terms = NULL,
  cluster,
  data,
  control = list(),
  ...
)
```

## Arguments

- surv:

  A formula of the following form `Surv(...) ~ 1`. The `Surv` objects
  needs to be of `type == 'counting'` with the following arguments:

  `time`:

  :   Start of follow-up time for each event episode, i.e., usually 0
      for the competing event and the first occurrence of the recurrent
      event. For every subsequent event the follow-up can either be 0 if
      gap time is the underlying time scale or the time of the previous
      event if total time is the underlying time scale.

  `time2`:

  :   End of follow-up, i.e., either occurrence of a terminal or
      recurrent event, or time of censoring.

  `status`:

  :   Event indicator for both terminal and recurrent event.

  `type`:

  :   Has to be `counting`.

- re_model:

  A formula object specifying the model for the recurrent event with an
  empty right hand side of the formula, e.g. `~ sex`.

- ce_model:

  A formula object specifying the model for the competing event with an
  empty right hand side of the formula, e.g. `~ sex`.

- re_indicator:

  Indicator that defines which rows in the dataset belong to the
  recurrent event process. These are usually more than one row per
  observations. The variable name needs to be passed as a character
  vector.

- ce_indicator:

  Indicator that defines which row in the dataset belong to the
  competing event process. The variable name needs to be passed as a
  character vector.

- df_ce:

  Defines the number of knots used to model the baseline hazard function
  for the competing event process.

- df_re:

  Defines the number of knots used to model the baseline hazard function
  for the recurrent event process.

- tvc_re_terms:

  A named list defining the number of knots used to model potential
  time-varying effects of variables included in the recurrent event
  model. This list should be of form
  `list(<var_name> = <no. of knots>)`.

- tvc_ce_terms:

  A named list defining the number of knots used to model potential
  time-varying effects of variables included in the competing event
  model. This list should be of form
  `list(<var_name> = <no. of knots>)`.

- cluster:

  A character vector specifying the name of the variable that defines
  unique observations in the dataset passed to the function.

- data:

  A stacked dataset that includes both data on the recurrent and
  competing event process. The dataset should have one row for each
  observation including the follow-up time and event indicator for the
  competing event and possibly multiple rows for each observation
  including the follow-up times and event indicator for the recurrent
  event, e.g.:

      id st_start  st_end re status
       1      0      6.88  0      1
       1      0      6.88  1      0
       2      0      8.70  0      1
       2      0      8.70  1      0
       3      0     10     0      0
       3      0      1.78  1      1
       3      1.78   6.08  1      1
       3      6.08  10     1      0
       4      0      6.07  0      1
       4      0      6.07  1      0

- control:

  List of arguments passed to
  [rstpm2::gsm.control](https://rdrr.io/pkg/rstpm2/man/gsm.control.html).

- ...:

  Additional arguments to be passed to
  [rstpm2::stpm2](https://rdrr.io/pkg/rstpm2/man/gsm.html).

## Value

An object of class `JointFPM` with the following elements:

- `model`: :

  The fitted FPM object,

- `re_terms`: :

  The terms used to model the recurrent event model,

- `ce_terms`: :

  The terms used to model the competing event model,

- `re_indicator`: :

  The name of the indicator variable of the recurrent event

## Examples

``` r
JointFPM(Surv(time  = start,
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
         data     = bladder1_stacked)
#> $model
#> 
#> Call:
#> rstpm2::stpm2(formula = model_formula, data = data, df = df_ce, 
#>     smooth.formula = bh_formula, tvc.formula = tvc_formula, cluster = data[[cluster]], 
#>     robust = TRUE, control = control)
#> 
#> Coefficients:
#>                               ce                               re 
#>                      -4.26119784                      -3.88216765 
#>                    ce:pyridoxine                      ce:thiotepa 
#>                      -0.59655478                      -0.59900162 
#>                    re:pyridoxine                      re:thiotepa 
#>                       0.16089523                       0.87325227 
#>            ce:nsx(log(stop), 3)1            ce:nsx(log(stop), 3)2 
#>                       2.00351423                       4.30723633 
#>            ce:nsx(log(stop), 3)3            re:nsx(log(stop), 3)1 
#>                       2.72222971                       3.31873438 
#>            re:nsx(log(stop), 3)2            re:nsx(log(stop), 3)3 
#>                       7.52378236                       3.28524605 
#> ce:pyridoxine:nsx(log(stop), 2)1 ce:pyridoxine:nsx(log(stop), 2)2 
#>                       1.44889533                      -0.26834877 
#>   ce:thiotepa:nsx(log(stop), 2)1   ce:thiotepa:nsx(log(stop), 2)2 
#>                       0.90885411                       1.05531401 
#> re:pyridoxine:nsx(log(stop), 2)1 re:pyridoxine:nsx(log(stop), 2)2 
#>                      -0.23787431                      -0.08296329 
#>   re:thiotepa:nsx(log(stop), 2)1   re:thiotepa:nsx(log(stop), 2)2 
#>                      -2.34781263                      -0.42711985 
#> 
#> Log-likelihood: -913.15 
#> 
#> $re_model
#> ~pyridoxine + thiotepa
#> <environment: 0x5641b8ba76a8>
#> 
#> $ce_model
#> ~pyridoxine + thiotepa
#> <environment: 0x5641b8ba76a8>
#> 
#> $re_indicator
#> [1] "re"
#> 
#> $ce_indicator
#> [1] "ce"
#> 
#> $cluster
#> [1] "id"
#> 
#> attr(,"class")
#> [1] "JointFPM"
```
