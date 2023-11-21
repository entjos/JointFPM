# Test model with same varaibles for re and ce model

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, cluster = "id",
      data = bladder1_stacked)
      summary(test$model)
    Output
      Maximum likelihood estimation
      
      Call:
      rstpm2::stpm2(formula = model_formula, data = data, df = df_ce, 
          smooth.formula = bh_formula, tvc.formula = tvc_formula, cluster = data[[cluster]], 
          robust = TRUE)
      
      Coefficients:
                             Estimate Std. Error  z value     Pr(z)    
      ce                    -4.661096   0.617295  -7.5508 4.325e-14 ***
      re                    -3.373459   0.263409 -12.8069 < 2.2e-16 ***
      ce:pyridoxine         -0.041228   0.495061  -0.0833    0.9336    
      ce:thiotepa            0.257212   0.419209   0.6136    0.5395    
      re:pyridoxine          0.012590   0.312943   0.0402    0.9679    
      re:thiotepa           -0.403732   0.287215  -1.4057    0.1598    
      ce:nsx(log(stop), 3)1  2.353144   0.543987   4.3257 1.520e-05 ***
      ce:nsx(log(stop), 3)2  4.888630   0.669808   7.2986 2.909e-13 ***
      ce:nsx(log(stop), 3)3  3.030259   0.504703   6.0040 1.925e-09 ***
      re:nsx(log(stop), 3)1  2.957845   0.220208  13.4320 < 2.2e-16 ***
      re:nsx(log(stop), 3)2  6.567172   0.400212  16.4092 < 2.2e-16 ***
      re:nsx(log(stop), 3)3  3.071956   0.188667  16.2825 < 2.2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      -2 log L: 1831.05 

# Test model with different varaibles for re and ce model

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~thiotepa, ce_model = ~ pyridoxine + thiotepa, re_indicator = "re",
      ce_indicator = "ce", df_ce = 3, df_re = 3, cluster = "id", data = bladder1_stacked)
      summary(test$model)
    Output
      Maximum likelihood estimation
      
      Call:
      rstpm2::stpm2(formula = model_formula, data = data, df = df_ce, 
          smooth.formula = bh_formula, tvc.formula = tvc_formula, cluster = data[[cluster]], 
          robust = TRUE)
      
      Coefficients:
                             Estimate Std. Error  z value     Pr(z)    
      ce                    -4.661100   0.617297  -7.5508 4.325e-14 ***
      re                    -3.368298   0.242122 -13.9116 < 2.2e-16 ***
      ce:pyridoxine         -0.041225   0.495062  -0.0833    0.9336    
      ce:thiotepa            0.257215   0.419209   0.6136    0.5395    
      re:thiotepa           -0.408684   0.281269  -1.4530    0.1462    
      ce:nsx(log(stop), 3)1  2.353149   0.543989   4.3257 1.520e-05 ***
      ce:nsx(log(stop), 3)2  4.888630   0.669807   7.2986 2.909e-13 ***
      ce:nsx(log(stop), 3)3  3.030262   0.504704   6.0040 1.925e-09 ***
      re:nsx(log(stop), 3)1  2.957433   0.219244  13.4892 < 2.2e-16 ***
      re:nsx(log(stop), 3)2  6.566814   0.399247  16.4480 < 2.2e-16 ***
      re:nsx(log(stop), 3)3  3.072073   0.189038  16.2511 < 2.2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      -2 log L: 1831.055 

# Test model with different dfs for ce and re model

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~thiotepa, ce_model = ~ pyridoxine + thiotepa, re_indicator = "re",
      ce_indicator = "ce", df_ce = 3, df_re = 1, cluster = "id", data = bladder1_stacked)
      summary(test$model)
    Output
      Maximum likelihood estimation
      
      Call:
      rstpm2::stpm2(formula = model_formula, data = data, df = df_ce, 
          smooth.formula = bh_formula, tvc.formula = tvc_formula, cluster = data[[cluster]], 
          robust = TRUE)
      
      Coefficients:
                             Estimate Std. Error  z value     Pr(z)    
      ce                    -4.661097   0.617296  -7.5508 4.325e-14 ***
      re                    -2.856343   0.232604 -12.2799 < 2.2e-16 ***
      ce:pyridoxine         -0.041227   0.495061  -0.0833    0.9336    
      ce:thiotepa            0.257214   0.419209   0.6136    0.5395    
      re:thiotepa           -0.405817   0.281373  -1.4423    0.1492    
      ce:nsx(log(stop), 3)1  2.353147   0.543988   4.3257 1.520e-05 ***
      ce:nsx(log(stop), 3)2  4.888630   0.669808   7.2986 2.909e-13 ***
      ce:nsx(log(stop), 3)3  3.030260   0.504704   6.0040 1.925e-09 ***
      re:nsx(log(stop), 1)   5.075695   0.316452  16.0394 < 2.2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      -2 log L: 1834.777 

# Test model with re_tvc_terms

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, tvc_re_terms = list(
        pyridoxine = 2), cluster = "id", data = bladder1_stacked)
      summary(test$model)
    Output
      Maximum likelihood estimation
      
      Call:
      rstpm2::stpm2(formula = model_formula, data = data, df = df_ce, 
          smooth.formula = bh_formula, tvc.formula = tvc_formula, cluster = data[[cluster]], 
          robust = TRUE)
      
      Coefficients:
                                        Estimate Std. Error  z value     Pr(z)    
      ce                               -4.661100   0.617298  -7.5508 4.325e-14 ***
      re                               -3.255979   0.307164 -10.6001 < 2.2e-16 ***
      ce:pyridoxine                    -0.041226   0.495061  -0.0833    0.9336    
      ce:thiotepa                       0.257213   0.419209   0.6136    0.5395    
      re:pyridoxine                    -0.414840   0.594303  -0.6980    0.4852    
      re:thiotepa                      -0.403832   0.287354  -1.4053    0.1599    
      ce:nsx(log(stop), 3)1             2.353147   0.543988   4.3257 1.520e-05 ***
      ce:nsx(log(stop), 3)2             4.888637   0.669812   7.2985 2.910e-13 ***
      ce:nsx(log(stop), 3)3             3.030259   0.504703   6.0040 1.925e-09 ***
      re:nsx(log(stop), 3)1             2.862311   0.264628  10.8164 < 2.2e-16 ***
      re:nsx(log(stop), 3)2             6.349759   0.508491  12.4875 < 2.2e-16 ***
      re:nsx(log(stop), 3)3             3.029916   0.228346  13.2689 < 2.2e-16 ***
      re:pyridoxine:nsx(log(stop), 2)1  0.824594   1.019319   0.8090    0.4185    
      re:pyridoxine:nsx(log(stop), 2)2  0.105275   0.384616   0.2737    0.7843    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      -2 log L: 1830.732 

# Test model with re_tvc_term and ce_tvc_term

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, tvc_re_terms = list(
        pyridoxine = 2), tvc_ce_terms = list(thiotepa = 2), cluster = "id", data = bladder1_stacked)
      summary(test$model)
    Output
      Maximum likelihood estimation
      
      Call:
      rstpm2::stpm2(formula = model_formula, data = data, df = df_ce, 
          smooth.formula = bh_formula, tvc.formula = tvc_formula, cluster = data[[cluster]], 
          robust = TRUE)
      
      Coefficients:
                                        Estimate Std. Error  z value     Pr(z)    
      ce                               -4.452350   0.676950  -6.5771 4.798e-11 ***
      re                               -3.255980   0.307164 -10.6001 < 2.2e-16 ***
      ce:pyridoxine                    -0.028145   0.487298  -0.0578 0.9539425    
      ce:thiotepa                      -0.406890   1.274839  -0.3192 0.7495980    
      re:pyridoxine                    -0.414831   0.594301  -0.6980 0.4851684    
      re:thiotepa                      -0.403832   0.287354  -1.4053 0.1599181    
      ce:nsx(log(stop), 3)1             2.201540   0.577708   3.8108 0.0001385 ***
      ce:nsx(log(stop), 3)2             4.749673   0.861420   5.5138 3.512e-08 ***
      ce:nsx(log(stop), 3)3             2.653783   0.567399   4.6771 2.910e-06 ***
      re:nsx(log(stop), 3)1             2.862313   0.264628  10.8164 < 2.2e-16 ***
      re:nsx(log(stop), 3)2             6.349763   0.508492  12.4874 < 2.2e-16 ***
      re:nsx(log(stop), 3)3             3.029916   0.228347  13.2689 < 2.2e-16 ***
      ce:thiotepa:nsx(log(stop), 2)1    0.412426   1.822176   0.2263 0.8209394    
      ce:thiotepa:nsx(log(stop), 2)2    1.168942   1.053732   1.1093 0.2672859    
      re:pyridoxine:nsx(log(stop), 2)1  0.824575   1.019317   0.8089 0.4185450    
      re:pyridoxine:nsx(log(stop), 2)2  0.105272   0.384616   0.2737 0.7843094    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      -2 log L: 1829.044 

