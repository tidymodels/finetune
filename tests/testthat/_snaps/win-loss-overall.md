# formula interface

    Code
      set.seed(1)
      res <- f_wflow %>% tune_race_win_loss(cell_folds, grid = 5, control = control_race(
        verbose_elim = TRUE))
    Message
      i Racing will maximize the roc_auc metric.
      i Resamples are analyzed in a random order.
      i Fold3, Repeat1: 0 eliminated; 5 candidates remain.
      i Fold2, Repeat2: 0 eliminated; 5 candidates remain.
      i Fold3, Repeat2: 0 eliminated; 5 candidates remain.

# one player is really bad

    Code
      best_res <- show_best(tuning_results)
    Condition
      Warning in `show_best()`:
      No value of `metric` was given; "roc_auc" will be used.

