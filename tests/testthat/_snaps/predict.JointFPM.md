# Mean number of events in bladder did not change

    Code
      bldr_model <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, cluster = "id",
      data = bladder1_stacked)
      predict(bldr_model, newdata = data.frame(pyridoxine = 1, thiotepa = 0), t = c(
        50), ci_fit = FALSE)
    Output
        stop      fit
      1   50 2.430327

# Check calc CIs for mean number

    Code
      bldr_model <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, cluster = "id",
      data = bladder1_stacked)
      print(predict(bldr_model, newdata = data.frame(pyridoxine = 1, thiotepa = 0),
      t = c(50), ci_fit = TRUE), digits = 4)
    Output
        stop  fit   lci   uci
      1   50 2.43 1.096 3.764

# Check calc CIs for diff in mean number

    Code
      bldr_model <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, cluster = "id",
      data = bladder1_stacked)
      print(predict(bldr_model, type = "diff", newdata = data.frame(pyridoxine = 1,
        thiotepa = 0), exposed = function(x) transform(x, thiotepa = 1), t = c(50),
      ci_fit = TRUE), digits = 4)
    Output
        stop   fit     lci   uci
      1   50 0.878 -0.1133 1.869

# Check calc CIs for marg mean number

    Code
      bldr_model <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa + size, ce_model = ~ pyridoxine +
        thiotepa + size, re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3,
      cluster = "id", data = bladder1_stacked)
      print(predict(bldr_model, newdata = data.frame(pyridoxine = 1, thiotepa = 0),
      t = c(10), type = "marg_mean_no", ci_fit = TRUE), digits = 4)
    Output
        stop    fit    lci   uci
      1   10 0.6101 0.2782 0.942

# Check calc CIs for diff in marg mean number

    Code
      bldr_model <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa + size, ce_model = ~ pyridoxine +
        thiotepa + size, re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3,
      cluster = "id", data = bladder1_stacked)
      print(predict(bldr_model, type = "marg_diff", newdata = data.frame(pyridoxine = 1,
        thiotepa = 0), exposed = function(x) transform(x, thiotepa = 1), t = c(50),
      ci_fit = TRUE), digits = 4)
    Output
        stop    fit     lci   uci
      1   50 0.8813 -0.1132 1.876

