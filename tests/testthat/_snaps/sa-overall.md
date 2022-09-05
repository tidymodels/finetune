# formula interface

    Code
      set.seed(1)
      res <- f_wflow %>% tune_sim_anneal(cell_folds, iter = 2, control = control_sim_anneal(
        verbose = TRUE))
    Message <rlang_message>
      Optimizing roc_auc
      Initial best: 0.82520
      1 ( ) accept suboptimal  roc_auc=0.75661	(+/-0.006729)
      2 + better suboptimal  roc_auc=0.79946	(+/-0.008393)

# unfinalized parameters

    Code
      set.seed(40)
      rf_res_finetune <- wf_rf %>% tune_sim_anneal(resamples = bt, initial = rf_res)
    Message <rlang_message>
      Optimizing roc_auc
      Initial best: 0.84856
       1 ( ) accept suboptimal  roc_auc=0.84375	(+/-0.007727)
       2 <3 new best           roc_auc=0.84943	(+/-0.007036)
       3 ( ) accept suboptimal  roc_auc=0.84371	(+/-0.007903)
       4 + better suboptimal  roc_auc=0.84825	(+/-0.008036)
       5 ( ) accept suboptimal  roc_auc=0.84479	(+/-0.00814)
       6 + better suboptimal  roc_auc=0.84816	(+/-0.007283)
       7 ( ) accept suboptimal  roc_auc=0.84381	(+/-0.007999)
       8 <3 new best           roc_auc=0.85014	(+/-0.007172)
       9 ( ) accept suboptimal  roc_auc=0.84344	(+/-0.007818)
      10 + better suboptimal  roc_auc=0.84802	(+/-0.007281)

