# control_sim_anneal bad arg passing

    Code
      control_sim_anneal(verbose = "TRUE")
    Condition
      Error in `control_sim_anneal()`:
      ! `verbose` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      control_sim_anneal(verbose = rep(TRUE, 2))
    Condition
      Error in `control_sim_anneal()`:
      ! `verbose` must be `TRUE` or `FALSE`, not a logical vector.

---

    Code
      control_sim_anneal(save_pred = "TRUE")
    Condition
      Error in `control_sim_anneal()`:
      ! `save_pred` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      control_sim_anneal(save_pred = rep(TRUE, 2))
    Condition
      Error in `control_sim_anneal()`:
      ! `save_pred` must be `TRUE` or `FALSE`, not a logical vector.

---

    Code
      control_sim_anneal(save_workflow = "TRUE")
    Condition
      Error in `control_sim_anneal()`:
      ! `save_workflow` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    Code
      control_sim_anneal(save_workflow = rep(TRUE, 2))
    Condition
      Error in `control_sim_anneal()`:
      ! `save_workflow` must be `TRUE` or `FALSE`, not a logical vector.

---

    Code
      control_sim_anneal(no_improve = "yes")
    Condition
      Error in `control_sim_anneal()`:
      ! `no_improve` must be a whole number, not the string "yes".

---

    Code
      control_sim_anneal(no_improve = 0:1)
    Condition
      Error in `control_sim_anneal()`:
      ! `no_improve` must be a whole number, not an integer vector.

---

    Code
      control_sim_anneal(no_improve = 1)
    Condition
      Error in `control_sim_anneal()`:
      ! `no_improve` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      control_sim_anneal(restart = "yes")
    Condition
      Error in `control_sim_anneal()`:
      ! `restart` must be a whole number, not the string "yes".

---

    Code
      control_sim_anneal(restart = 0:1)
    Condition
      Error in `control_sim_anneal()`:
      ! `restart` must be a whole number, not an integer vector.

---

    Code
      control_sim_anneal(restart = 1)
    Condition
      Error in `control_sim_anneal()`:
      ! `restart` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      control_sim_anneal(no_improve = 2, restart = 6)
    Message
      ! Parameter restart is scheduled after 6 poor iterations but the search will stop after 2.
    Output
      Simulated annealing control object

---

    Code
      control_sim_anneal(radius = "huge")
    Condition
      Error in `control_sim_anneal()`:
      ! Argument `radius` should be two numeric values.

---

    Code
      control_sim_anneal(flip = 0:1)
    Condition
      Error in `control_sim_anneal()`:
      ! `flip` must be a number, not an integer vector.

---

    Code
      control_sim_anneal(flip = "huge")
    Condition
      Error in `control_sim_anneal()`:
      ! `flip` must be a number, not the string "huge".

---

    Code
      control_sim_anneal(cooling_coef = 0:1)
    Condition
      Error in `control_sim_anneal()`:
      ! `cooling_coef` must be a number, not an integer vector.

---

    Code
      control_sim_anneal(cooling_coef = "huge")
    Condition
      Error in `control_sim_anneal()`:
      ! `cooling_coef` must be a number, not the string "huge".

---

    Code
      control_sim_anneal(pkg = 0:1)
    Condition
      Error in `control_sim_anneal()`:
      ! `pkgs` must be a character vector or `NULL`, not an integer vector.

---

    Code
      control_sim_anneal(extract = 0:1)
    Condition
      Error in `control_sim_anneal()`:
      ! `extract` must be a function or `NULL`, not an integer vector.

# casting control_sim_anneal to control_grid

    Code
      parsnip::condense_control(control_sim_anneal(), control_grid())
    Message
      Grid/resamples control object
        `verbose`: FALSE
        `allow_par`: TRUE
        `extract`: NULL
        `save_pred`: FALSE
        `pkgs`: NULL
        `save_workflow`: FALSE
        `event_level`: "first"
        `parallel_over`: NULL
        `backend_options`: NULL
        `workflow_size`: 100

