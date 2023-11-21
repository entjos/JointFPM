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
