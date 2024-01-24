# formula interface

    Code
      set.seed(1)
      res <- f_wflow %>% tune_sim_anneal(cell_folds, iter = 2, control = control_sim_anneal(
        verbose = TRUE))
    Message
      
      >  Generating a set of 1 initial parameter results
      v Initialization complete
      
      Optimizing roc_auc
      Initial best: 0.82520
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
      1 ( ) accept suboptimal  roc_auc=0.75661 (+/-0.006729)
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
      2 + better suboptimal  roc_auc=0.79946 (+/-0.008393)

# variable interface

    Code
      set.seed(1)
      res <- var_wflow %>% tune_sim_anneal(cell_folds, iter = 2, control = control_sim_anneal(
        verbose = TRUE, verbose_iter = TRUE))
    Message
      
      >  Generating a set of 1 initial parameter results
      v Initialization complete
      
      Optimizing roc_auc
      Initial best: 0.82520
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
      1 ( ) accept suboptimal  roc_auc=0.75661 (+/-0.006729)
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
      2 + better suboptimal  roc_auc=0.79946 (+/-0.008393)

---

    Code
      set.seed(1)
      new_res <- var_wflow %>% tune_sim_anneal(cell_folds, iter = 2, initial = res,
        control = control_sim_anneal(verbose = FALSE))
    Message
      There were 2 previous iterations
      Optimizing roc_auc
      2 v initial            roc_auc=0.8252 (+/-0.005662)
      3 <3 new best           roc_auc=0.82851 (+/-0.004901)
      4 <3 new best           roc_auc=0.83405 (+/-0.004564)

---

    Code
      set.seed(1)
      new_new_res <- var_wflow %>% tune_sim_anneal(cell_folds, iter = 2, initial = grid_res,
        control = control_sim_anneal(verbose = FALSE))
    Message
      Optimizing roc_auc
      Initial best: 0.83924
      1 ( ) accept suboptimal  roc_auc=0.83776 (+/-0.007509)
      2 <3 new best           roc_auc=0.84 (+/-0.00542)

# unfinalized parameters

    Code
      set.seed(40)
      rf_res_finetune <- wf_rf %>% tune_sim_anneal(resamples = bt, initial = rf_res)
    Message
      Optimizing roc_auc
      Initial best: 0.86248
      1 ( ) accept suboptimal  roc_auc=0.86132 (+/-0.007045)
      2 ( ) accept suboptimal  roc_auc=0.85987 (+/-0.007598)
      3 + better suboptimal  roc_auc=0.86155 (+/-0.007228)
      4 + better suboptimal  roc_auc=0.86193 (+/-0.007212)
      5 ( ) accept suboptimal  roc_auc=0.86108 (+/-0.00727)
      6 ( ) accept suboptimal  roc_auc=0.85992 (+/-0.007641)
      7 + better suboptimal  roc_auc=0.86045 (+/-0.007405)
      8 x restart from best  roc_auc=0.85863 (+/-0.007596)
      9 ( ) accept suboptimal  roc_auc=0.8623 (+/-0.007265)
      10 <3 new best           roc_auc=0.86273 (+/-0.007569)

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

