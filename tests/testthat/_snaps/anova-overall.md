# formula interface

    Code
      set.seed(1)
      res <- tune_race_anova(f_wflow, cell_folds, grid = grid_mod, control = control_race(
        verbose_elim = TRUE))
    Message
      i Evaluating against the initial 3 burn-in resamples.
      i Racing will maximize the roc_auc metric.
      i Resamples are analyzed in a random order.
      i Fold3, Repeat1: 2 eliminated; 2 candidates remain.
      i Fold2, Repeat2: All but one parameter combination were eliminated.

# too few resamples

    Code
      tune_race_anova(f_wflow, rs, grid = grid_mod, control = control_race(
        verbose_elim = TRUE))
    Condition
      Error in `tune_race_anova()`:
      ! The number of resamples (2) needs to be more than the number of burn-in resamples (3) set by the control function `control_race()`.

---

    Code
      tune_race_win_loss(f_wflow, rs, grid = grid_mod, control = control_race(
        verbose_elim = TRUE))
    Condition
      Error in `tune_race_win_loss()`:
      ! The number of resamples (2) needs to be more than the number of burn-in resamples (3) set by the control function `control_race()`.

