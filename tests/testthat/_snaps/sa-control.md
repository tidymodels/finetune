# control_sim_anneal bad arg passing

    `verbose` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    `verbose` must be `TRUE` or `FALSE`, not a logical vector.

---

    `save_pred` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    `save_pred` must be `TRUE` or `FALSE`, not a logical vector.

---

    `save_workflow` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    `save_workflow` must be `TRUE` or `FALSE`, not a logical vector.

---

    `no_improve` must be a whole number, not the string "yes".

---

    `no_improve` must be a whole number, not an integer vector.

---

    `no_improve` must be a whole number larger than or equal to 2, not the number 1.

---

    `restart` must be a whole number, not the string "yes".

---

    `restart` must be a whole number, not an integer vector.

---

    `restart` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      control_sim_anneal(no_improve = 2, restart = 6)
    Message
      ! Parameter restart is scheduled after 6 poor iterations but the search will stop after 2.
    Output
      Simulated annealing control object

---

    Argument `radius` should be two numeric values.

---

    `flip` must be a number, not an integer vector.

---

    `flip` must be a number, not the string "huge".

---

    `cooling_coef` must be a number, not an integer vector.

---

    `cooling_coef` must be a number, not the string "huge".

---

    `pkgs` must be a character vector or `NULL`, not an integer vector.

---

    `extract` must be a function or `NULL`, not an integer vector.

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

