# Test model with same varaibles for re and ce model

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, cluster = "id",
      data = bladder1_stacked)
      print(coef(test$model), digits = 3)
    Output
                         ce                    re         ce:pyridoxine 
                    -4.6611               -3.3735               -0.0412 
                ce:thiotepa         re:pyridoxine           re:thiotepa 
                     0.2572                0.0126               -0.4037 
      ce:nsx(log(stop), 3)1 ce:nsx(log(stop), 3)2 ce:nsx(log(stop), 3)3 
                     2.3531                4.8886                3.0303 
      re:nsx(log(stop), 3)1 re:nsx(log(stop), 3)2 re:nsx(log(stop), 3)3 
                     2.9578                6.5672                3.0720 

# Test model with different varaibles for re and ce model

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~thiotepa, ce_model = ~ pyridoxine + thiotepa, re_indicator = "re",
      ce_indicator = "ce", df_ce = 3, df_re = 3, cluster = "id", data = bladder1_stacked)
      print(coef(test$model), digits = 3)
    Output
                         ce                    re         ce:pyridoxine 
                    -4.6611               -3.3683               -0.0412 
                ce:thiotepa           re:thiotepa ce:nsx(log(stop), 3)1 
                     0.2572               -0.4087                2.3531 
      ce:nsx(log(stop), 3)2 ce:nsx(log(stop), 3)3 re:nsx(log(stop), 3)1 
                     4.8886                3.0303                2.9574 
      re:nsx(log(stop), 3)2 re:nsx(log(stop), 3)3 
                     6.5668                3.0721 

# Test model with different dfs for ce and re model

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~thiotepa, ce_model = ~ pyridoxine + thiotepa, re_indicator = "re",
      ce_indicator = "ce", df_ce = 3, df_re = 1, cluster = "id", data = bladder1_stacked)
      print(coef(test$model), digits = 3)
    Output
                         ce                    re         ce:pyridoxine 
                    -4.6611               -2.8563               -0.0412 
                ce:thiotepa           re:thiotepa ce:nsx(log(stop), 3)1 
                     0.2572               -0.4058                2.3531 
      ce:nsx(log(stop), 3)2 ce:nsx(log(stop), 3)3  re:nsx(log(stop), 1) 
                     4.8886                3.0303                5.0757 

# Test model with re_tvc_terms

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, tvc_re_terms = list(
        pyridoxine = 2), cluster = "id", data = bladder1_stacked)
      print(coef(test$model), digits = 3)
    Output
                                    ce                               re 
                               -4.6611                          -3.2560 
                         ce:pyridoxine                      ce:thiotepa 
                               -0.0412                           0.2572 
                         re:pyridoxine                      re:thiotepa 
                               -0.4148                          -0.4038 
                 ce:nsx(log(stop), 3)1            ce:nsx(log(stop), 3)2 
                                2.3531                           4.8886 
                 ce:nsx(log(stop), 3)3            re:nsx(log(stop), 3)1 
                                3.0303                           2.8623 
                 re:nsx(log(stop), 3)2            re:nsx(log(stop), 3)3 
                                6.3498                           3.0299 
      re:pyridoxine:nsx(log(stop), 2)1 re:pyridoxine:nsx(log(stop), 2)2 
                                0.8246                           0.1053 

# Test model with re_tvc_term and ce_tvc_term

    Code
      test <- JointFPM(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        1, re_model = ~ pyridoxine + thiotepa, ce_model = ~ pyridoxine + thiotepa,
      re_indicator = "re", ce_indicator = "ce", df_ce = 3, df_re = 3, tvc_re_terms = list(
        pyridoxine = 2), tvc_ce_terms = list(thiotepa = 2), cluster = "id", data = bladder1_stacked)
      print(coef(test$model), digits = 3)
    Output
                                    ce                               re 
                               -4.4524                          -3.2560 
                         ce:pyridoxine                      ce:thiotepa 
                               -0.0281                          -0.4069 
                         re:pyridoxine                      re:thiotepa 
                               -0.4148                          -0.4038 
                 ce:nsx(log(stop), 3)1            ce:nsx(log(stop), 3)2 
                                2.2015                           4.7497 
                 ce:nsx(log(stop), 3)3            re:nsx(log(stop), 3)1 
                                2.6538                           2.8623 
                 re:nsx(log(stop), 3)2            re:nsx(log(stop), 3)3 
                                6.3498                           3.0299 
        ce:thiotepa:nsx(log(stop), 2)1   ce:thiotepa:nsx(log(stop), 2)2 
                                0.4124                           1.1689 
      re:pyridoxine:nsx(log(stop), 2)1 re:pyridoxine:nsx(log(stop), 2)2 
                                0.8246                           0.1053 

