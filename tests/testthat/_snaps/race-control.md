# control_race bad arg passing

    `verbose` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    `verbose` must be `TRUE` or `FALSE`, not a logical vector.

---

    `verbose_elim` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    `verbose_elim` must be `TRUE` or `FALSE`, not a logical vector.

---

    `save_pred` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    `save_pred` must be `TRUE` or `FALSE`, not a logical vector.

---

    `save_workflow` must be `TRUE` or `FALSE`, not the string "TRUE".

---

    `save_workflow` must be `TRUE` or `FALSE`, not a logical vector.

---

    `burn_in` must be a whole number, not the string "yes".

---

    `burn_in` must be a whole number, not an integer vector.

---

    `burn_in` must be a whole number larger than or equal to 2, not the number 1.

---

    `num_ties` must be a whole number, not the string "yes".

---

    `num_ties` must be a whole number, not an integer vector.

---

    `alpha` must be a number, not an integer vector.

---

    `alpha` must be a number, not the string "huge".

---

    `alpha` should be on (0, 1).

---

    `pkgs` must be a character vector or `NULL`, not an integer vector.

---

    `extract` must be a function or `NULL`, not an integer vector.

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

