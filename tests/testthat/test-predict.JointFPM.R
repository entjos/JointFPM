test_that("Mean number of events in bladder did not change",{
  expect_snapshot({
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
            t       =  c(50),
            ci_fit  = FALSE)
  })
})

test_that("Difference is correct",{
  expect_equal({
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
            type = "diff",
            newdata = data.frame(pyridoxine = 1,
                                 thiotepa   = 0),
            exposed = \(x) transform(x, thiotepa = 1),
            t       =  c(50),
            ci_fit  = FALSE)[["fit"]]
  }, {

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

    e0 <- predict(bldr_model,
                  newdata = data.frame(pyridoxine = 1,
                                       thiotepa   = 0),
                  t       =  c(50),
                  ci_fit  = FALSE)

    e1 <- predict(bldr_model,
                  newdata = data.frame(pyridoxine = 1,
                                       thiotepa   = 1),
                  t       =  c(50),
                  ci_fit  = FALSE)

    e0$fit - e1$fit
  })
})

test_that("Parallel: Check calc CIs for mean number",{
  skip_on_cran()
  expect_snapshot({
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
            t       =  c(50),
            ci_fit  = TRUE) |>
      print(digits = 4)
  })
})

test_that("Parallel: Check calc CIs for diff in mean number",{
  skip_on_cran()
  expect_snapshot({
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
            type = "diff",
            newdata = data.frame(pyridoxine = 1,
                                 thiotepa   = 0),
            exposed = \(x) transform(x, thiotepa = 1),
            t       =  c(50),
            ci_fit  = TRUE) |>
      print(digits = 4)
  })
})

test_that("Parallel: Check calc CIs for marg mean number",{
  skip_on_cran()
  expect_snapshot({
    bldr_model <- JointFPM(Surv(time  = start,
                                time2 = stop,
                                event = event,
                                type  = 'counting') ~ 1,
                           re_model = ~ pyridoxine + thiotepa + size,
                           ce_model = ~ pyridoxine + thiotepa + size,
                           re_indicator = "re",
                           ce_indicator = "ce",
                           df_ce = 3,
                           df_re = 3,
                           cluster  = "id",
                           data     = bladder1_stacked)

    predict(bldr_model,
            newdata = data.frame(pyridoxine = 1,
                                 thiotepa   = 0),
            t       =  c(10),
            type = "marg_mean_no",
            ci_fit  = TRUE) |>
      print(digits = 4)
  })
})

test_that("Parallel: Check calc CIs for diff in marg mean number",{
  skip_on_cran()
  expect_snapshot({
    bldr_model <- JointFPM(Surv(time  = start,
                                time2 = stop,
                                event = event,
                                type  = 'counting') ~ 1,
                           re_model = ~ pyridoxine + thiotepa + size,
                           ce_model = ~ pyridoxine + thiotepa + size,
                           re_indicator = "re",
                           ce_indicator = "ce",
                           df_ce = 3,
                           df_re = 3,
                           cluster  = "id",
                           data     = bladder1_stacked)

    predict(bldr_model,
            type = "marg_diff",
            newdata = data.frame(pyridoxine = 1,
                                 thiotepa   = 0),
            exposed = \(x) transform(x, thiotepa = 1),
            t       =  c(50),
            ci_fit  = TRUE) |>
      print(digits = 4)
  })
})

test_that("S_M(t|x) == S(t|x)", {
  expect_equal({

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
            t       =  c(10),
            type = "mean_no",
            ci_fit  = FALSE)

  }, {

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
            t       =  c(10),
            type = "marg_mean_no",
            ci_fit  = FALSE)

  })
})

test_that("Point estimate is the same if ci_fit == TRUE or FALSE", {
  expect_equal({

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
            t       =  c(10),
            type = "marg_mean_no",
            ci_fit  = TRUE)$est
  }, {

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
            t       =  c(10),
            type = "marg_mean_no",
            ci_fit  = FALSE)$est

  })
})

test_that("S_M(t|x0) - S_M(t|x1) == S(t|x0) - S(t|x1)", {
  expect_equal(
    {
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
              exposed = function(x) transform(x, pyridoxine = 0, thiotepa = 1),
              t       =  c(10),
              type = "diff",
              ci_fit  = FALSE)$est
    } , {
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
              exposed = function(x) transform(x, pyridoxine = 0, thiotepa = 1),
              t       =  c(10),
              type = "marg_diff",
              ci_fit  = FALSE)$est
    })
})

test_that("Error when newdata is not a data.frame",{
  expect_error({
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
            newdata = list(pyridoxine = 1,
                           thiotepa   = 0),
            t       =  c(10),
            type = "mean_no",
            ci_fit  = FALSE)
  }, regexp = "`newdata` is not a `data.frame`")
})

test_that("Error when newdata has more than one row",{
  expect_error({
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
            newdata = data.frame(pyridoxine = 0:1,
                                 thiotepa   = 0),
            t       =  c(10),
            type = "mean_no",
            ci_fit  = FALSE)
  }, regexp = "`newdata` has more than one row")
})

test_that("Error when chosing a wrong prediction type",{
  expect_error({
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
            newdata = data.frame(pyridoxine = 0:1,
                                 thiotepa   = 0),
            t       =  c(10),
            type = "merg_no",
            ci_fit  = FALSE)
  })
})

test_that("Error when forgetting to specify exposed",{
  expect_error({
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
            t       =  c(10),
            type = "marg_diff",
            ci_fit  = FALSE)
  }, regexp = "without specifing an exposed group")
})

test_that("Error when exposed is not a function",{
  expect_error({
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
            exposed = data.frame(pyridoxine = 0,
                                 thiotepa   = 1),
            t       =  c(10),
            type = "marg_diff",
            ci_fit  = FALSE)
  }, regexp = "`exposed` is not a function.")
})

test_that("Error when var is not included in newdata",{
  expect_error({
    bldr_model <- JointFPM(Surv(time  = start,
                                time2 = stop,
                                event = event,
                                type  = 'counting') ~ 1,
                           re_model = ~ pyridoxine + thiotepa + size,
                           ce_model = ~ pyridoxine + thiotepa + size,
                           re_indicator = "re",
                           ce_indicator = "ce",
                           df_ce = 3,
                           df_re = 3,
                           cluster  = "id",
                           data     = bladder1_stacked)

    predict(bldr_model,
            newdata = data.frame(pyridoxine = 1),
            t       =  c(10),
            type = "mean_no",
            ci_fit  = FALSE)
  }, regexp = "thiotepa and size are not included in `newdata`")
})

test_that("Test integration when t == 0",{
  expect_equal({
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
            t       =  c(0),
            type = "mean_no",
            ci_fit  = FALSE)$fit


  }, NA_real_)
})

test_that("Integration with GQ works", {
  expect_snapshot({

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
            t       =  c(1, 50, 100),
            method  = "gq",
            ngq     = 30,
            ci_fit  = FALSE)
  })
})

test_that("GQ gives results similar to Romberg Integration", {
  expect_equal({

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
            t       =  c(1, 50, 100),
            method  = "gq",
            ngq     = 200,
            ci_fit  = FALSE)
  }, {

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
            t       =  c(1, 50, 100),
            ci_fit  = FALSE)

  },
  tolerance = 0.001)
})

