# Tests DFs for JointFPMs.

Test of different degrees of freedoms (DFs) for joint flexible
parametric survival models.

**\[experimental\]**

## Usage

``` r
test_dfs_JointFPM(
  surv,
  re_model,
  ce_model,
  re_indicator,
  ce_indicator,
  dfs_ce,
  dfs_re,
  tvc_re_terms = NULL,
  tvc_ce_terms = NULL,
  cluster,
  data
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

  Indicator that defined which rows in the dataset belong to the
  recurrent event process. These are usually more than one row per
  observations. The variable name needs to be passed as a character
  vector.

- ce_indicator:

  Indicator that defined which row in the dataset belong to the
  competing event process. The variable name needs to be passed as a
  character vector.

- dfs_ce:

  Defines the number of knots used to model the baseline hazard function
  for the competing event process.

- dfs_re:

  Defines the number of knots used to model the baseline hazard function
  for the recurrent event process.

- tvc_re_terms:

  A named list defining the numbers of knots used to model potential
  time-varying effects of variables included in the recurrent event
  model. This list should be of form
  `list(<var_name> = <no. of knots>)`.

- tvc_ce_terms:

  A named list defining the numbers of knots used to model potential
  time-varying effects of variables included in the competing event
  model. This list should be of form
  `list(<var_name> = <no. of knots>)`.

- cluster:

  A chara vector specifying the name of the variable that defines unique
  observation in the dataset passed to the function.

- data:

  A stacked dataset that including both data on the recurrent and
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

## Value

A `data.frame` with one row per combination of baseline hazards DFs, and
the DFs of the time varying covariates, and the corresponding AIC and
BIC.

## Examples

``` r
# Test different dfs
test_dfs_JointFPM(Surv(time  = start,
                       time2 = stop,
                       event = event,
                       type  = 'counting') ~ 1,
                  re_model = ~ pyridoxine + thiotepa,
                  ce_model = ~ pyridoxine + thiotepa,
                  re_indicator = "re",
                  ce_indicator = "ce",
                  dfs_ce = 1:3,
                  dfs_re = 2,
                  tvc_ce_terms = list(thiotepa   = 1:2),
                  tvc_re_terms = list(pyridoxine = 2),
                  cluster  = "id",
                  data     = bladder1_stacked)
#>   df_ce df_re df_ce_thiotepa df_re_pyridoxine      AIC      BIC
#> 1     1     2              1                2 1855.761 1904.013
#> 2     2     2              1                2 1857.316 1909.590
#> 3     3     2              1                2 1859.121 1915.415
#> 4     1     2              2                2 1856.842 1909.115
#> 5     2     2              2                2 1858.763 1915.057
#> 6     3     2              2                2 1860.577 1920.893
```
