# control_sim_anneal bad arg passing

    Argument 'verbose' should be a single logical value in `control_sim_anneal()`

---

    Argument 'verbose' should be a single logical value in `control_sim_anneal()`

---

    Argument 'save_pred' should be a single logical value in `control_sim_anneal()`

---

    Argument 'save_pred' should be a single logical value in `control_sim_anneal()`

---

    Argument 'save_workflow' should be a single logical value in `control_sim_anneal()`

---

    Argument 'save_workflow' should be a single logical value in `control_sim_anneal()`

---

    Argument 'no_improve' should be a single numeric or integer value in `control_sim_anneal()`

---

    Argument 'no_improve' should be a single numeric or integer value in `control_sim_anneal()`

---

    `no_improve` should be > 1.

---

    Argument 'restart' should be a single numeric or integer value in `control_sim_anneal()`

---

    Argument 'restart' should be a single numeric or integer value in `control_sim_anneal()`

---

    `restart` should be > 1.

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

    Argument 'flip' should be a single numeric value in `control_sim_anneal()`

---

    Argument 'flip' should be a single numeric value in `control_sim_anneal()`

---

    Argument 'cooling_coef' should be a single numeric value in `control_sim_anneal()`

---

    Argument 'cooling_coef' should be a single numeric value in `control_sim_anneal()`

---

    Argument 'pkgs' should be a character or NULL in `control_sim_anneal()`

---

    Argument 'extract' should be a function or NULL in `control_sim_anneal()`

# casting control_sim_anneal to control_grid

    Code
      parsnip::condense_control(control_sim_anneal(), control_grid())
    Output
      grid/resamples control object

