test_that("Stable output of test run", {
  expect_snapshot({

    mean_no(Surv(time  = start,
                  time2 = stop,
                  event = event,
                  type  = 'counting') ~ pyridoxine + thiotepa,
            re_indicator = "re",
            ce_indicator = "ce",
            cluster = "id",
            data = bladder1_stacked)

  })
})

test_that("Stable output of test run with RHS of ~ 1", {
  expect_snapshot({

    mean_no(Surv(time  = start,
                 time2 = stop,
                 event = event,
                 type  = 'counting') ~ 1,
            re_indicator = "re",
            ce_indicator = "ce",
            cluster = "id",
            data = bladder1_stacked)

  })
})

test_that("Formula not of type formula", {
  expect_error({

    mean_no("Surv(time  = start,
                 time2 = stop,
                 event = event,
                 type  = 'counting') ~ 1",
            re_indicator = "re",
            ce_indicator = "ce",
            cluster = "id",
            data = bladder1_stacked)

  },
  regexp = "`formula` is not a formula")
})

test_that("Formula not of type counting", {
  expect_error({

    mean_no(Surv(time2 = stop,
                 event = event) ~ 1,
            re_indicator = "re",
            ce_indicator = "ce",
            cluster = "id",
            data = bladder1_stacked)

  },
  regexp = "`surv` is not of type `counting`")
})

test_that("RE/CE indocator not included in data", {
  expect_error({

    mean_no(Surv(time  = start,
                 time2 = stop,
                 event = event,
                 type  = 'counting') ~ 1,
            re_indicator = "re2",
            ce_indicator = "ce",
            cluster = "id",
            data = bladder1_stacked)

  },
  regexp = "One or both of `re_indicator`, and `ce_indicator`")
})

test_that("Cluster var not included in data", {
  expect_error({

    mean_no(Surv(time  = start,
                 time2 = stop,
                 event = event,
                 type  = 'counting') ~ 1,
            re_indicator = "re",
            ce_indicator = "ce",
            cluster = "id2",
            data = bladder1_stacked)

  },
  regexp = "`cluster` is not included in `data`")
})

test_that("Data has less than 2 rows per individual", {
  expect_error({

    temp <- bladder1_stacked
    temp$ce[[1]] <- 0
    temp$re[[1]] <- 1

    mean_no(Surv(time  = start,
                 time2 = stop,
                 event = event,
                 type  = 'counting') ~ 1,
            re_indicator = "re",
            ce_indicator = "ce",
            cluster = "id",
            data = temp)

  },
  regexp = "`data` has at least one observation with less than 2 rows.")
})
