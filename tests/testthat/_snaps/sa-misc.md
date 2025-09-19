# tune_sim_anneal interfaces

    Code
      set.seed(1)
      f_res_1 <- tune_sim_anneal(rda_spec, Class ~ ., rs, iter = 3)
    Message
      Optimizing roc_auc
      Initial best: 0.85731
      1 ( ) accept suboptimal  roc_auc=0.85432 (+/-0.01039)
      2 <3 new best           roc_auc=0.86674 (+/-0.009097)
      3 <3 new best           roc_auc=0.87245 (+/-0.008363)

---

    Code
      set.seed(1)
      f_res_2 <- tune_sim_anneal(rda_spec, Class ~ ., rs, iter = 3, param_info = rda_param)
    Message
      Optimizing roc_auc
      Initial best: 0.85325
      1 ( ) accept suboptimal  roc_auc=0.85268 (+/-0.01056)
      2 <3 new best           roc_auc=0.85618 (+/-0.01015)
      3 <3 new best           roc_auc=0.85799 (+/-0.009991)

---

    Code
      set.seed(1)
      f_rec_1 <- tune_sim_anneal(rda_spec, rec, rs, iter = 3)
    Message
      Optimizing roc_auc
      Initial best: 0.86616
      1 ( ) accept suboptimal  roc_auc=0.86568 (+/-0.01027)
      2 <3 new best           roc_auc=0.86654 (+/-0.009714)
      3 <3 new best           roc_auc=0.8688 (+/-0.009487)

---

    Code
      set.seed(1)
      f_wflow_1 <- tune_sim_anneal(wflow, rs, iter = 3)
    Message
      Optimizing roc_auc
      Initial best: 0.86616
      1 ( ) accept suboptimal  roc_auc=0.86568 (+/-0.01027)
      2 <3 new best           roc_auc=0.86654 (+/-0.009714)
      3 <3 new best           roc_auc=0.8688 (+/-0.009487)

# tune_sim_anneal with wrong type

    Code
      tune_sim_anneal(1)
    Condition
      Error in `tune_sim_anneal()`:
      ! The first argument to `tune_sim_anneal()` should be either a model or workflow.

