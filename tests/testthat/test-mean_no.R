test_that("Stable output of test run", {
  expect_snapshot({

    mean_no(Surv(time  = start,
                  time2 = stop,
                  event = event,
                  type  = 'counting') ~ pyridoxine + thiotepa,
            re_indicator = "re",
            ce_indicator = "ce",
            data = bladder1_stacked)

  })
})
