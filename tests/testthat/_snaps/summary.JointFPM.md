# Summary output is stable

    Code
      summary(JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, cluster = "id",
      data = bladder1_stacked))
    Output
      Coefficients CE model --------------------------------------------------------------------
                            Estimate Std. Error    z value        Pr(z)
      (Intercept)        -4.66109572  0.6172954 -7.5508351 4.324762e-14
      pyridoxine         -0.04122802  0.4950614 -0.0832786 9.336300e-01
      thiotepa            0.25721226  0.4192089  0.6135658 5.395023e-01
      nsx(log(stop), 3)1  2.35314415  0.5439871  4.3257354 1.520238e-05
      nsx(log(stop), 3)2  4.88863017  0.6698078  7.2985569 2.908705e-13
      nsx(log(stop), 3)3  3.03025917  0.5047027  6.0040473 1.924586e-09
      
      Coefficients RE model --------------------------------------------------------------------
                            Estimate Std. Error      z value        Pr(z)
      (Intercept)        -3.37345897  0.2634091 -12.80691692 1.499759e-37
      pyridoxine          0.01259029  0.3129425   0.04023196 9.679082e-01
      thiotepa           -0.40373220  0.2872148  -1.40568047 1.598190e-01
      nsx(log(stop), 3)1  2.95784540  0.2202084  13.43202892 3.925081e-41
      nsx(log(stop), 3)2  6.56717183  0.4002125  16.40921356 1.643201e-60
      nsx(log(stop), 3)3  3.07195562  0.1886666  16.28245729 1.314815e-59
      
      -2 Log-likelihood: -1831.05

