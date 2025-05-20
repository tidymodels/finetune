# anova filtering and logging

    Code
      set.seed(129)
      anova_mod <- tune_race_anova(spec, mpg ~ ., folds, grid = grid)

---

    Code
      finetune:::log_racing(control_race(verbose_elim = TRUE), anova_res,
      ames_grid_search$splits, 10, "rmse")
    Message
      i Fold10: 7 eliminated; 3 candidates remain.

---

    Code
      finetune:::log_racing(control_race(verbose_elim = TRUE), anova_res,
      ames_grid_search$splits, 10, "rmse")
    Message
      i Fold10: 7 eliminated; 3 candidates remain.

---

    Code
      finetune:::log_racing(control_race(verbose_elim = TRUE), anova_res,
      ames_grid_search$splits, 10, "rmse")
    Message
      i Fold10: 7 eliminated; 3 candidates remain.

