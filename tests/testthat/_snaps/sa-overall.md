# unfinalized parameters

    Code
      set.seed(40)
      rf_res_finetune <- tune_sim_anneal(wf_rf, resamples = bt, initial = rf_res)
    Message
      i Creating pre-processing data to finalize 1 unknown parameter: "mtry"
      Optimizing roc_auc
      Initial best: 0.84917
      1 ( ) accept suboptimal  roc_auc=0.84467 (+/-0.007843)
      2 <3 new best           roc_auc=0.84956 (+/-0.007013)
      3 ( ) accept suboptimal  roc_auc=0.84384 (+/-0.008042)
      4 + better suboptimal  roc_auc=0.84952 (+/-0.00757)
      5 ( ) accept suboptimal  roc_auc=0.84357 (+/-0.008148)
      6 + better suboptimal  roc_auc=0.84848 (+/-0.007499)
      7 ( ) accept suboptimal  roc_auc=0.84391 (+/-0.008097)
      8 + better suboptimal  roc_auc=0.84853 (+/-0.007036)
      9 ( ) accept suboptimal  roc_auc=0.84471 (+/-0.007812)
      10 x restart from best  roc_auc=0.84935 (+/-0.007313)

---

    Code
      set.seed(40)
      rf_res_finetune <- tune_sim_anneal(wf_rf, resamples = bt)
    Message
      i Creating pre-processing data to finalize 1 unknown parameter: "mtry"
      Optimizing roc_auc
      Initial best: 0.84369
      1 <3 new best           roc_auc=0.84929 (+/-0.007889)
      2 ( ) accept suboptimal  roc_auc=0.84461 (+/-0.008084)
      3 + better suboptimal  roc_auc=0.84785 (+/-0.007989)
      4 ( ) accept suboptimal  roc_auc=0.84325 (+/-0.007817)
      5 + better suboptimal  roc_auc=0.84917 (+/-0.007376)
      6 ( ) accept suboptimal  roc_auc=0.84458 (+/-0.007566)
      7 + better suboptimal  roc_auc=0.84871 (+/-0.007483)
      8 ( ) accept suboptimal  roc_auc=0.84431 (+/-0.007694)
      9 x restart from best  roc_auc=0.84861 (+/-0.007929)
      10 ( ) accept suboptimal  roc_auc=0.84522 (+/-0.008113)

# incompatible parameter objects

    Code
      res <- tune_sim_anneal(car_wflow, param_info = parameter_set_with_smaller_range,
        resamples = car_folds, initial = tune_res_with_bigger_range, iter = 2)
    Message
      Optimizing rmse
      
    Condition
      Error in `tune_sim_anneal()`:
      ! The range for parameter mtry used when generating initial results isn't compatible with the range supplied in `param_info`.
      i Possible values of parameters in `param_info` should encompass all values evaluated in the initial grid.
    Message
      x Optimization stopped prematurely; returning current results.

