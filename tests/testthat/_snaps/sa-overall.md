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
      Initial best: 0.86248
       1 ( ) accept suboptimal  roc_auc=0.86132	(+/-0.007045)
       2 ( ) accept suboptimal  roc_auc=0.85987	(+/-0.007598)
       3 + better suboptimal  roc_auc=0.86155	(+/-0.007228)
       4 + better suboptimal  roc_auc=0.86193	(+/-0.007212)
       5 ( ) accept suboptimal  roc_auc=0.86108	(+/-0.00727)
       6 ( ) accept suboptimal  roc_auc=0.85992	(+/-0.007641)
       7 + better suboptimal  roc_auc=0.86045	(+/-0.007405)
       8 x restart from best  roc_auc=0.85863	(+/-0.007596)
       9 ( ) accept suboptimal  roc_auc=0.8623	(+/-0.007265)
      10 <3 new best           roc_auc=0.86273	(+/-0.007569)

