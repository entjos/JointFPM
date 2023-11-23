# Stable output of test run

    Code
      test_dfs_JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", dfs_ce = 1:3, dfs_re = 2,
      tvc_ce_terms = list(thiotepa = 1:2), tvc_re_terms = list(pyridoxine = 2),
      cluster = "id", data = bladder1_stacked)
    Output
        df_ce df_re df_ce_thiotepa df_re_pyridoxine      AIC      BIC
      1     1     2              1                2 1855.761 1904.013
      2     2     2              1                2 1857.316 1909.590
      3     3     2              1                2 1859.121 1915.415
      4     1     2              2                2 1856.842 1909.115
      5     2     2              2                2 1858.763 1915.057
      6     3     2              2                2 1860.577 1920.893

