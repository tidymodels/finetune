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
      i Creating pre-processing data to finalize unknown parameter: mtry
      Optimizing roc_auc
      Initial best: 0.84856
      1 ( ) accept suboptimal  roc_auc=0.84375 (+/-0.007727)
      2 <3 new best           roc_auc=0.84943 (+/-0.007036)
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
      rf_res_finetune <- wf_rf %>% tune_sim_anneal(resamples = bt)
    Message
      i Creating pre-processing data to finalize unknown parameter: mtry
      Optimizing roc_auc
      Initial best: 0.84829
      1 ( ) accept suboptimal  roc_auc=0.84326 (+/-0.007765)
      2 <3 new best           roc_auc=0.84843 (+/-0.007903)
      3 ( ) accept suboptimal  roc_auc=0.84438 (+/-0.007904)
      4 <3 new best           roc_auc=0.84881 (+/-0.007951)
      5 ( ) accept suboptimal  roc_auc=0.84404 (+/-0.007787)
      6 <3 new best           roc_auc=0.84912 (+/-0.007272)
      7 ( ) accept suboptimal  roc_auc=0.84333 (+/-0.007685)
      8 + better suboptimal  roc_auc=0.84867 (+/-0.007152)
      9 - discard suboptimal roc_auc=0.84525 (+/-0.007793)
      10 ( ) accept suboptimal  roc_auc=0.84383 (+/-0.00773)

