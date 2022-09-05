# formula interface

    Code
      set.seed(1)
      res <- f_wflow %>% tune_race_anova(cell_folds, grid = grid_mod, control = control_race(
        verbose_elim = TRUE))
    Message <rlang_message>
      i Racing will maximize the roc_auc metric.
      i Resamples are analyzed in a random order.
      i Fold3, Repeat1: 2 eliminated; 2 candidates remain.
      i Fold2, Repeat2: All but one parameter combination were eliminated.

