# tune_sim_anneal interfaces

    Code
      set.seed(1)
      f_res_1 <- rda_spec %>% tune_sim_anneal(Class ~ ., rs, iter = 3)
    Message <cliMessage>
      Optimizing roc_auc
      Initial best: 0.85161
      1 ( ) accept suboptimal  roc_auc=0.84064 (+/-0.01096)
      2 ( ) accept suboptimal  roc_auc=0.83789 (+/-0.01086)
      3 ( ) accept suboptimal  roc_auc=0.83261 (+/-0.0109)

---

    Code
      set.seed(1)
      f_res_2 <- rda_spec %>% tune_sim_anneal(Class ~ ., rs, iter = 3, param_info = rda_param)
    Message <cliMessage>
      Optimizing roc_auc
      Initial best: 0.85192
      1 ( ) accept suboptimal  roc_auc=0.848 (+/-0.01072)
      2 ( ) accept suboptimal  roc_auc=0.84678 (+/-0.01067)
      3 ( ) accept suboptimal  roc_auc=0.84534 (+/-0.01097)

---

    Code
      set.seed(1)
      f_rec_1 <- rda_spec %>% tune_sim_anneal(rec, rs, iter = 3)
    Message <cliMessage>
      Optimizing roc_auc
      Initial best: 0.87657
      1 ( ) accept suboptimal  roc_auc=0.87345 (+/-0.008739)
      2 ( ) accept suboptimal  roc_auc=0.87124 (+/-0.008458)
      3 ( ) accept suboptimal  roc_auc=0.86909 (+/-0.008582)

---

    Code
      set.seed(1)
      f_wflow_1 <- wflow %>% tune_sim_anneal(rs, iter = 3)
    Message <cliMessage>
      Optimizing roc_auc
      Initial best: 0.87657
      1 ( ) accept suboptimal  roc_auc=0.87345 (+/-0.008739)
      2 ( ) accept suboptimal  roc_auc=0.87124 (+/-0.008458)
      3 ( ) accept suboptimal  roc_auc=0.86909 (+/-0.008582)

