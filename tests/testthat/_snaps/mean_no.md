# Stable output of test run

    Code
      mean_no(Surv(time = start, time2 = stop, event = event, type = "counting") ~
        pyridoxine + thiotepa, re_indicator = "re", ce_indicator = "ce", data = bladder1_stacked)
    Output
           time         na      surv       expn                   strata
        1:    1 0.02083333 0.9583333 0.01996528 pyridoxine=0, thiotepa=0
        2:    2 0.08695652 0.9583333 0.10329861 pyridoxine=0, thiotepa=0
        3:    3 0.15217391 0.9583333 0.24913194 pyridoxine=0, thiotepa=0
        4:    5 0.04444444 0.9583333 0.29172454 pyridoxine=0, thiotepa=0
        5:    6 0.08888889 0.9583333 0.37690972 pyridoxine=0, thiotepa=0
       ---                                                              
      108:   44 0.10000000 0.7774462 2.21109295 pyridoxine=1, thiotepa=0
      109:   46 0.00000000 0.6802654 2.21109295 pyridoxine=1, thiotepa=0
      110:   47 0.16666667 0.6802654 2.32447052 pyridoxine=1, thiotepa=0
      111:   48 0.16666667 0.6802654 2.43784809 pyridoxine=1, thiotepa=0
      112:   49 0.20000000 0.6802654 2.57390117 pyridoxine=1, thiotepa=0

