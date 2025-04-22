# tune_sim_anneal interfaces

    Code
      set.seed(1)
      f_res_1 <- tune_sim_anneal(rda_spec, Class ~ ., rs, iter = 3)
    Message
      Optimizing roc_auc
      Initial best: 0.85731
      1 ( ) accept suboptimal  roc_auc=0.85682 (+/-0.01022)
      2 ( ) accept suboptimal  roc_auc=0.85238 (+/-0.01078)
      3 ( ) accept suboptimal  roc_auc=0.85138 (+/-0.0109)

---

    Code
      set.seed(1)
      f_res_2 <- tune_sim_anneal(rda_spec, Class ~ ., rs, iter = 3, param_info = rda_param)
    Message
      Optimizing roc_auc
      Initial best: 0.85325
      1 ( ) accept suboptimal  roc_auc=0.85313 (+/-0.0106)
      2 ( ) accept suboptimal  roc_auc=0.85181 (+/-0.01065)
      3 ( ) accept suboptimal  roc_auc=0.85165 (+/-0.01055)

---

    Code
      set.seed(1)
      f_rec_1 <- tune_sim_anneal(rda_spec, rec, rs, iter = 3)
    Message
      Optimizing roc_auc
      Initial best: 0.86616
      1 ( ) accept suboptimal  roc_auc=0.86399 (+/-0.01081)
      2 <3 new best           roc_auc=0.86768 (+/-0.009563)
      3 <3 new best           roc_auc=0.87329 (+/-0.008273)

---

    Code
      set.seed(1)
      f_wflow_1 <- tune_sim_anneal(wflow, rs, iter = 3)
    Message
      Optimizing roc_auc
      Initial best: 0.86616
      1 ( ) accept suboptimal  roc_auc=0.86399 (+/-0.01081)
      2 <3 new best           roc_auc=0.86768 (+/-0.009563)
      3 <3 new best           roc_auc=0.87329 (+/-0.008273)

