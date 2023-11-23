test_that("Stable output of test run",{
  expect_snapshot({

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

  })
})

test_that("Stable output of test run",{
  expect_equal({

    temp <- test_dfs_JointFPM(Surv(time  = start,
                                   time2 = stop,
                                   event = event,
                                   type  = 'counting') ~ 1,
                              re_model = ~ pyridoxine + thiotepa,
                              ce_model = ~ pyridoxine + thiotepa,
                              re_indicator = "re",
                              ce_indicator = "ce",
                              dfs_ce = 2,
                              dfs_re = c(2, 200),
                              tvc_ce_terms = list(thiotepa   = 1),
                              tvc_re_terms = list(pyridoxine = 2),
                              cluster  = "id",
                              data     = bladder1_stacked)

    temp$AIC[[2]]

  }, Inf)
})
