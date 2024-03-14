test_that("Summary output is stable",{
  expect_no_condition({
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
                           data     = bladder1_stacked) |> summary()
  })
})
