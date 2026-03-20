# control_race bad arg passing

    Code
      control_race(verbose = "TRUE")
    Condition
      Error in `control_race()`:
      ! `verbose` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      control_race(verbose = rep(TRUE, 2))
    Condition
      Error in `control_race()`:
      ! `verbose` must be `TRUE` or `FALSE`, not a logical vector.

---

    Code
      control_race(verbose_elim = "TRUE")
    Condition
      Error in `control_race()`:
      ! `verbose_elim` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      control_race(verbose_elim = rep(TRUE, 2))
    Condition
      Error in `control_race()`:
      ! `verbose_elim` must be `TRUE` or `FALSE`, not a logical vector.

---

    Code
      control_race(save_pred = "TRUE")
    Condition
      Error in `control_race()`:
      ! `save_pred` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      control_race(save_pred = rep(TRUE, 2))
    Condition
      Error in `control_race()`:
      ! `save_pred` must be `TRUE` or `FALSE`, not a logical vector.

---

    Code
      control_race(save_workflow = "TRUE")
    Condition
      Error in `control_race()`:
      ! `save_workflow` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      control_race(save_workflow = rep(TRUE, 2))
    Condition
      Error in `control_race()`:
      ! `save_workflow` must be `TRUE` or `FALSE`, not a logical vector.

---

    Code
      control_race(burn_in = "yes")
    Condition
      Error in `control_race()`:
      ! `burn_in` must be a whole number, not the string "yes".

---

    Code
      control_race(burn_in = 0:1)
    Condition
      Error in `control_race()`:
      ! `burn_in` must be a whole number, not an integer vector.

---

    Code
      control_race(burn_in = 1)
    Condition
      Error in `control_race()`:
      ! `burn_in` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      control_race(num_ties = "yes")
    Condition
      Error in `control_race()`:
      ! `num_ties` must be a whole number, not the string "yes".

---

    Code
      control_race(num_ties = 0:1)
    Condition
      Error in `control_race()`:
      ! `num_ties` must be a whole number, not an integer vector.

---

    Code
      control_race(alpha = 0:1)
    Condition
      Error in `control_race()`:
      ! `alpha` must be a number, not an integer vector.

---

    Code
      control_race(alpha = "huge")
    Condition
      Error in `control_race()`:
      ! `alpha` must be a number, not the string "huge".

---

    Code
      control_race(alpha = 1)
    Condition
      Error in `control_race()`:
      ! `alpha` should be on (0, 1).

---

    Code
      control_race(pkg = 0:1)
    Condition
      Error in `control_race()`:
      ! `pkgs` must be a character vector or `NULL`, not an integer vector.

---

    Code
      control_race(extract = 0:1)
    Condition
      Error in `control_race()`:
      ! `extract` must be a function or `NULL`, not an integer vector.

# casting control_race to control_grid

    Code
      parsnip::condense_control(control_race(), control_grid())
    Message
      Grid/resamples control object
        `verbose`: FALSE
        `allow_par`: TRUE
        `extract`: NULL
        `save_pred`: FALSE
        `pkgs`: NULL
        `save_workflow`: FALSE
        `event_level`: "first"
        `parallel_over`: "everything"
        `backend_options`: NULL
        `workflow_size`: 100

