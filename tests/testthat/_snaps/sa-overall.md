# formula interface

    Code
      set.seed(1)
      res <- tune_sim_anneal(f_wflow, cell_folds, iter = 2, control = control_sim_anneal(
        verbose = TRUE))
    Message
      
      >  Generating a set of 1 initial parameter results
      v Initialization complete
      
      Optimizing roc_auc
      Initial best: 0.73008
      i Fold1, Repeat1: preprocessor 1/1
      v Fold1, Repeat1: preprocessor 1/1
      i Fold1, Repeat1: preprocessor 1/1, model 1/1
      v Fold1, Repeat1: preprocessor 1/1, model 1/1
      i Fold1, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold1, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold2, Repeat1: preprocessor 1/1
      v Fold2, Repeat1: preprocessor 1/1
      i Fold2, Repeat1: preprocessor 1/1, model 1/1
      v Fold2, Repeat1: preprocessor 1/1, model 1/1
      i Fold2, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold2, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold3, Repeat1: preprocessor 1/1
      v Fold3, Repeat1: preprocessor 1/1
      i Fold3, Repeat1: preprocessor 1/1, model 1/1
      v Fold3, Repeat1: preprocessor 1/1, model 1/1
      i Fold3, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold3, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold1, Repeat2: preprocessor 1/1
      v Fold1, Repeat2: preprocessor 1/1
      i Fold1, Repeat2: preprocessor 1/1, model 1/1
      v Fold1, Repeat2: preprocessor 1/1, model 1/1
      i Fold1, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold1, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      i Fold2, Repeat2: preprocessor 1/1
      v Fold2, Repeat2: preprocessor 1/1
      i Fold2, Repeat2: preprocessor 1/1, model 1/1
      v Fold2, Repeat2: preprocessor 1/1, model 1/1
      i Fold2, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold2, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      i Fold3, Repeat2: preprocessor 1/1
      v Fold3, Repeat2: preprocessor 1/1
      i Fold3, Repeat2: preprocessor 1/1, model 1/1
      v Fold3, Repeat2: preprocessor 1/1, model 1/1
      i Fold3, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold3, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      1 ( ) accept suboptimal  roc_auc=0.72145 (+/-0.003605)
      i Fold1, Repeat1: preprocessor 1/1
      v Fold1, Repeat1: preprocessor 1/1
      i Fold1, Repeat1: preprocessor 1/1, model 1/1
      v Fold1, Repeat1: preprocessor 1/1, model 1/1
      i Fold1, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold1, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold2, Repeat1: preprocessor 1/1
      v Fold2, Repeat1: preprocessor 1/1
      i Fold2, Repeat1: preprocessor 1/1, model 1/1
      v Fold2, Repeat1: preprocessor 1/1, model 1/1
      i Fold2, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold2, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold3, Repeat1: preprocessor 1/1
      v Fold3, Repeat1: preprocessor 1/1
      i Fold3, Repeat1: preprocessor 1/1, model 1/1
      v Fold3, Repeat1: preprocessor 1/1, model 1/1
      i Fold3, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold3, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold1, Repeat2: preprocessor 1/1
      v Fold1, Repeat2: preprocessor 1/1
      i Fold1, Repeat2: preprocessor 1/1, model 1/1
      v Fold1, Repeat2: preprocessor 1/1, model 1/1
      i Fold1, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold1, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      i Fold2, Repeat2: preprocessor 1/1
      v Fold2, Repeat2: preprocessor 1/1
      i Fold2, Repeat2: preprocessor 1/1, model 1/1
      v Fold2, Repeat2: preprocessor 1/1, model 1/1
      i Fold2, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold2, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      i Fold3, Repeat2: preprocessor 1/1
      v Fold3, Repeat2: preprocessor 1/1
      i Fold3, Repeat2: preprocessor 1/1, model 1/1
      v Fold3, Repeat2: preprocessor 1/1, model 1/1
      i Fold3, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold3, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      2 <3 new best           roc_auc=0.73173 (+/-0.003018)

# variable interface

    Code
      set.seed(1)
      res <- tune_sim_anneal(var_wflow, cell_folds, iter = 2, control = control_sim_anneal(
        verbose = TRUE, verbose_iter = TRUE))
    Message
      
      >  Generating a set of 1 initial parameter results
      v Initialization complete
      
      Optimizing roc_auc
      Initial best: 0.73008
      i Fold1, Repeat1: preprocessor 1/1
      v Fold1, Repeat1: preprocessor 1/1
      i Fold1, Repeat1: preprocessor 1/1, model 1/1
      v Fold1, Repeat1: preprocessor 1/1, model 1/1
      i Fold1, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold1, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold2, Repeat1: preprocessor 1/1
      v Fold2, Repeat1: preprocessor 1/1
      i Fold2, Repeat1: preprocessor 1/1, model 1/1
      v Fold2, Repeat1: preprocessor 1/1, model 1/1
      i Fold2, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold2, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold3, Repeat1: preprocessor 1/1
      v Fold3, Repeat1: preprocessor 1/1
      i Fold3, Repeat1: preprocessor 1/1, model 1/1
      v Fold3, Repeat1: preprocessor 1/1, model 1/1
      i Fold3, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold3, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold1, Repeat2: preprocessor 1/1
      v Fold1, Repeat2: preprocessor 1/1
      i Fold1, Repeat2: preprocessor 1/1, model 1/1
      v Fold1, Repeat2: preprocessor 1/1, model 1/1
      i Fold1, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold1, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      i Fold2, Repeat2: preprocessor 1/1
      v Fold2, Repeat2: preprocessor 1/1
      i Fold2, Repeat2: preprocessor 1/1, model 1/1
      v Fold2, Repeat2: preprocessor 1/1, model 1/1
      i Fold2, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold2, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      i Fold3, Repeat2: preprocessor 1/1
      v Fold3, Repeat2: preprocessor 1/1
      i Fold3, Repeat2: preprocessor 1/1, model 1/1
      v Fold3, Repeat2: preprocessor 1/1, model 1/1
      i Fold3, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold3, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      1 ( ) accept suboptimal  roc_auc=0.72145 (+/-0.003605)
      i Fold1, Repeat1: preprocessor 1/1
      v Fold1, Repeat1: preprocessor 1/1
      i Fold1, Repeat1: preprocessor 1/1, model 1/1
      v Fold1, Repeat1: preprocessor 1/1, model 1/1
      i Fold1, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold1, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold2, Repeat1: preprocessor 1/1
      v Fold2, Repeat1: preprocessor 1/1
      i Fold2, Repeat1: preprocessor 1/1, model 1/1
      v Fold2, Repeat1: preprocessor 1/1, model 1/1
      i Fold2, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold2, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold3, Repeat1: preprocessor 1/1
      v Fold3, Repeat1: preprocessor 1/1
      i Fold3, Repeat1: preprocessor 1/1, model 1/1
      v Fold3, Repeat1: preprocessor 1/1, model 1/1
      i Fold3, Repeat1: preprocessor 1/1, model 1/1 (extracts)
      i Fold3, Repeat1: preprocessor 1/1, model 1/1 (predictions)
      i Fold1, Repeat2: preprocessor 1/1
      v Fold1, Repeat2: preprocessor 1/1
      i Fold1, Repeat2: preprocessor 1/1, model 1/1
      v Fold1, Repeat2: preprocessor 1/1, model 1/1
      i Fold1, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold1, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      i Fold2, Repeat2: preprocessor 1/1
      v Fold2, Repeat2: preprocessor 1/1
      i Fold2, Repeat2: preprocessor 1/1, model 1/1
      v Fold2, Repeat2: preprocessor 1/1, model 1/1
      i Fold2, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold2, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      i Fold3, Repeat2: preprocessor 1/1
      v Fold3, Repeat2: preprocessor 1/1
      i Fold3, Repeat2: preprocessor 1/1, model 1/1
      v Fold3, Repeat2: preprocessor 1/1, model 1/1
      i Fold3, Repeat2: preprocessor 1/1, model 1/1 (extracts)
      i Fold3, Repeat2: preprocessor 1/1, model 1/1 (predictions)
      2 <3 new best           roc_auc=0.73173 (+/-0.003018)

---

    Code
      set.seed(1)
      new_res <- tune_sim_anneal(var_wflow, cell_folds, iter = 2, initial = res,
        control = control_sim_anneal(verbose = FALSE))
    Message
      There were 2 previous iterations
      Optimizing roc_auc
      2 v initial            roc_auc=0.73173 (+/-0.003018)
      3 <3 new best           roc_auc=0.74172 (+/-0.008775)
      4 <3 new best           roc_auc=0.7909 (+/-0.009887)

---

    Code
      set.seed(1)
      new_new_res <- tune_sim_anneal(var_wflow, cell_folds, iter = 2, initial = grid_res,
        control = control_sim_anneal(verbose = FALSE))
    Message
      Optimizing roc_auc
      Initial best: 0.84497
      1 <3 new best           roc_auc=0.84531 (+/-0.005563)
      2 ( ) accept suboptimal  roc_auc=0.83776 (+/-0.007509)

# unfinalized parameters

    Code
      set.seed(40)
      rf_res_finetune <- tune_sim_anneal(wf_rf, resamples = bt, initial = rf_res)
    Message
      i Creating pre-processing data to finalize unknown parameter: mtry
      Optimizing roc_auc
      Initial best: 0.84994
      1 ( ) accept suboptimal  roc_auc=0.84375 (+/-0.007727)
      2 + better suboptimal  roc_auc=0.84943 (+/-0.007036)
      3 ( ) accept suboptimal  roc_auc=0.84371 (+/-0.007903)
      4 + better suboptimal  roc_auc=0.84825 (+/-0.008036)
      5 ( ) accept suboptimal  roc_auc=0.84479 (+/-0.00814)
      6 + better suboptimal  roc_auc=0.84816 (+/-0.007283)
      7 ( ) accept suboptimal  roc_auc=0.84381 (+/-0.007999)
      8 <3 new best           roc_auc=0.85014 (+/-0.007172)
      9 ( ) accept suboptimal  roc_auc=0.84344 (+/-0.007818)
      10 + better suboptimal  roc_auc=0.84802 (+/-0.007281)

---

    Code
      set.seed(40)
      rf_res_finetune <- tune_sim_anneal(wf_rf, resamples = bt)
    Message
      i Creating pre-processing data to finalize unknown parameter: mtry
      Optimizing roc_auc
      Initial best: 0.84418
      1 <3 new best           roc_auc=0.84839 (+/-0.007753)
      2 ( ) accept suboptimal  roc_auc=0.84384 (+/-0.008085)
      3 <3 new best           roc_auc=0.84857 (+/-0.007615)
      4 ( ) accept suboptimal  roc_auc=0.8435 (+/-0.007746)
      5 + better suboptimal  roc_auc=0.84804 (+/-0.00774)
      6 ( ) accept suboptimal  roc_auc=0.84338 (+/-0.007515)
      7 <3 new best           roc_auc=0.84923 (+/-0.007371)
      8 ( ) accept suboptimal  roc_auc=0.84389 (+/-0.007938)
      9 <3 new best           roc_auc=0.84926 (+/-0.007163)
      10 ( ) accept suboptimal  roc_auc=0.84397 (+/-0.00741)

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

