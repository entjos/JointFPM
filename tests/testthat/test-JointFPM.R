test_that("Test model with same varaibles for re and ce model", {
  expect_snapshot({
    test <- JointFPM(Surv(time  = start,
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

    print(coef(test$model), digits = 3)

  })
})

test_that("Test model with different varaibles for re and ce model", {
  expect_snapshot({
    test <- JointFPM(Surv(time  = start,
                          time2 = stop,
                          event = event,
                          type  = 'counting') ~ 1,
                     re_model = ~ thiotepa,
                     ce_model = ~ pyridoxine + thiotepa,
                     re_indicator = "re",
                     ce_indicator = "ce",
                     df_ce = 3,
                     df_re = 3,
                     cluster  = "id",
                     data     = bladder1_stacked)

    print(coef(test$model), digits = 3)

  })
})

test_that("Test model with different dfs for ce and re model", {
  expect_snapshot({
    test <- JointFPM(Surv(time  = start,
                          time2 = stop,
                          event = event,
                          type  = 'counting') ~ 1,
                     re_model = ~ thiotepa,
                     ce_model = ~ pyridoxine + thiotepa,
                     re_indicator = "re",
                     ce_indicator = "ce",
                     df_ce = 3,
                     df_re = 1,
                     cluster  = "id",
                     data     = bladder1_stacked)

    print(coef(test$model), digits = 3)

  })
})

test_that("Test model with re_tvc_terms", {
  expect_snapshot({
    test <- JointFPM(Surv(time  = start,
                          time2 = stop,
                          event = event,
                          type  = 'counting') ~ 1,
                     re_model = ~ pyridoxine + thiotepa,
                     ce_model = ~ pyridoxine + thiotepa,
                     re_indicator = "re",
                     ce_indicator = "ce",
                     df_ce = 3,
                     df_re = 3,
                     tvc_re_terms = list(pyridoxine = 2),
                     cluster  = "id",
                     data     = bladder1_stacked)

    print(coef(test$model), digits = 3)

  })
})

test_that("Test model with re_tvc_term and ce_tvc_term", {
  expect_snapshot({
    test <- JointFPM(Surv(time  = start,
                          time2 = stop,
                          event = event,
                          type  = 'counting') ~ 1,
                     re_model = ~ pyridoxine + thiotepa,
                     ce_model = ~ pyridoxine + thiotepa,
                     re_indicator = "re",
                     ce_indicator = "ce",
                     df_ce = 3,
                     df_re = 3,
                     tvc_re_terms = list(pyridoxine = 2),
                     tvc_ce_terms = list(thiotepa = 2),
                     cluster  = "id",
                     data     = bladder1_stacked)

    print(coef(test$model), digits = 3)

  })
})

test_that("Correct model specification", {
  expect_equal({
    test <- JointFPM(Surv(time  = start,
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

    as.character(formula(test$model@terms))[[3]]
  }, paste0("-1 + ce + re + (pyridoxine:ce + thiotepa:ce + pyridoxine:re + ",
            "thiotepa:re + nsx(log(stop), 3):ce + nsx(log(stop), 3):re)"))
})

test_that("Test error when re_model or ce_model is not a formula", {
  expect_error({
    JointFPM(Surv(time  = start,
                          time2 = stop,
                          event = event,
                          type  = 'counting') ~ 1,
                     re_model = "thiotepa",
                     ce_model = ~ pyridoxine + thiotepa,
                     re_indicator = "re",
                     ce_indicator = "ce",
                     df_ce = 3,
                     df_re = 1,
                     cluster  = "id",
                     data     = bladder1_stacked)

  }, regexp = "`re_model` or `ce_model` is not a formula")
})

test_that("Test error when re_indicator is not in data", {
  expect_error({
    JointFPM(Surv(time  = start,
                  time2 = stop,
                  event = event,
                  type  = 'counting') ~ 1,
             re_model = ~ pyridoxine + thiotepa,
             ce_model = ~ pyridoxine + thiotepa,
             re_indicator = "res",
             ce_indicator = "ce",
             df_ce = 3,
             df_re = 1,
             cluster  = "id",
             data     = bladder1_stacked)

  }, regexp = "re_indicator")
})

test_that("Test error when cluster is not in data", {
  expect_error({
    JointFPM(Surv(time  = start,
                  time2 = stop,
                  event = event,
                  type  = 'counting') ~ 1,
             re_model = ~ pyridoxine + thiotepa,
             ce_model = ~ pyridoxine + thiotepa,
             re_indicator = "re",
             ce_indicator = "ce",
             df_ce = 3,
             df_re = 1,
             cluster  = "idm",
             data     = bladder1_stacked)

  }, regexp = "`cluster` is not included in `data`")
})

test_that("Test error when surv is of type counting", {
  expect_error({
    JointFPM(Surv(time2 = stop,
                  event = event) ~ 1,
             re_model = ~ pyridoxine + thiotepa,
             ce_model = ~ pyridoxine + thiotepa,
             re_indicator = "re",
             ce_indicator = "ce",
             df_ce = 3,
             df_re = 1,
             cluster  = "id",
             data     = bladder1_stacked)

  }, regexp = "`surv` is not of type `counting`")
})

test_that("Test error when data is not in stacked format", {
  expect_error({

    temp_data <- bladder1_stacked[-1, ]

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
             cluster  = "id",
             data     = temp_data)

  }, regexp = "`data` has at least one observation with less than 2 rows.")
})
