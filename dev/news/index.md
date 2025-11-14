# Changelog

## finetune (development version)

- A bug was fixed where `NULL` results generated during simulated
  annealing would cause errors when logging.

## finetune 1.2.1

CRAN release: 2025-05-20

- Maintenance release required by CRAN.

- Transition from the magrittr pipe to the base R pipe.

## finetune 1.2.0

CRAN release: 2024-03-21

### New Features

- finetune now fully supports models in the “censored regression” mode.
  These models can be fit, tuned, and evaluated like the regression and
  classification modes.
  [tidymodels.org](https://www.tidymodels.org/learn/#category=survival%20analysis)
  has more information and tutorials on how to work with survival
  analysis models.

- Improved error message from
  [`tune_sim_anneal()`](https://finetune.tidymodels.org/dev/reference/tune_sim_anneal.md)
  when values in the supplied `param_info` do not encompass all values
  evaluated in the `initial` grid. This most often happens when a user
  mistakenly supplies different parameter sets to the function that
  generated the initial results and
  [`tune_sim_anneal()`](https://finetune.tidymodels.org/dev/reference/tune_sim_anneal.md).

- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods for racing objects will now use integers in x-axis breaks
  ([\#75](https://github.com/tidymodels/finetune/issues/75)).

- Enabling the `verbose_elim` control option for
  [`tune_race_anova()`](https://finetune.tidymodels.org/dev/reference/tune_race_anova.md)
  will now additionally introduce a message confirming that the function
  is evaluating against the burn-in resamples.

- Updates based on the new version of tune, primarily for survival
  analysis models
  ([\#104](https://github.com/tidymodels/finetune/issues/104)).

### Bug Fixes

- Fixed bug where
  [`tune_sim_anneal()`](https://finetune.tidymodels.org/dev/reference/tune_sim_anneal.md)
  would fail when supplied parameters needing finalization. The function
  will now finalize needed parameter ranges internally
  ([\#39](https://github.com/tidymodels/finetune/issues/39)).

- Fixed bug where packages specified in `control_race(pkgs)` were not
  actually loaded in
  [`tune_race_anova()`](https://finetune.tidymodels.org/dev/reference/tune_race_anova.md)
  ([\#74](https://github.com/tidymodels/finetune/issues/74)).

### Breaking Change

- Ellipses (…) are now used consistently in the package to require
  optional arguments to be named.
  [`collect_predictions()`](https://finetune.tidymodels.org/dev/reference/collect_predictions.md),
  [`collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html)
  and
  [`show_best()`](https://tune.tidymodels.org/reference/show_best.html)
  methods previously had ellipses at the end of the function signature
  that have been moved to follow the last argument without a default
  value. Optional arguments previously passed by position will now error
  informatively prompting them to be named
  ([\#105](https://github.com/tidymodels/finetune/issues/105)).

## finetune 1.1.0

CRAN release: 2023-04-19

- Various minor changes to keep up with developments in the tune and
  dplyr packages
  ([\#60](https://github.com/tidymodels/finetune/issues/60))
  ([\#62](https://github.com/tidymodels/finetune/issues/62))
  ([\#67](https://github.com/tidymodels/finetune/issues/67))
  ([\#68](https://github.com/tidymodels/finetune/issues/68)).

- Corrects `.config` output with `save_pred = TRUE` in
  [`tune_sim_anneal()`](https://finetune.tidymodels.org/dev/reference/tune_sim_anneal.md).
  The function previously outputted a constant `Model1_Preprocessor1` in
  the `.predictions` slot, and now provides `.config` values that align
  with those in `.metrics`
  ([\#57](https://github.com/tidymodels/finetune/issues/57)).

- An `eval_time` attribute was added to tune objects produced by
  finetune.

## finetune 1.0.1

CRAN release: 2022-10-12

- For racing:

- [`collect_metrics()`](https://tune.tidymodels.org/reference/collect_predictions.html)
  and
  [`collect_predictions()`](https://finetune.tidymodels.org/dev/reference/collect_predictions.md)
  have a `'complete'` argument that only returns results for model
  configurations that were fully resampled.

- [`select_best()`](https://tune.tidymodels.org/reference/show_best.html)
  and
  [`show_best()`](https://tune.tidymodels.org/reference/show_best.html)
  now only show results for model configurations that were fully
  resampled.

- [`tune_race_anova()`](https://finetune.tidymodels.org/dev/reference/tune_race_anova.md),
  [`tune_race_win_loss()`](https://finetune.tidymodels.org/dev/reference/tune_race_win_loss.md),
  and
  [`tune_sim_anneal()`](https://finetune.tidymodels.org/dev/reference/tune_sim_anneal.md)
  no longer error if `control` argument isn’t a the corresponding
  `control_*()` object. Will work as long as the object passed to
  `control` includes the same elements as the required `control_*()`
  object.

- The
  [`control_sim_anneal()`](https://finetune.tidymodels.org/dev/reference/control_sim_anneal.md)
  got a new argument `verbose_iter` that is used to control the
  verbosity of the iterative calculations. This change means that the
  `verbose` argument is being passed to
  [`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
  to control its verbosity.

## finetune 1.0.0

CRAN release: 2022-09-05

- An informative error is given when there are not enough resamples for
  racing ([\#33](https://github.com/tidymodels/finetune/issues/33)).

- [`tune_sim_anneal()`](https://finetune.tidymodels.org/dev/reference/tune_sim_anneal.md)
  was not passing all arguments to
  [`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
  ([\#40](https://github.com/tidymodels/finetune/issues/40)).

## finetune 0.2.0

CRAN release: 2022-03-24

- Maintenance release for CRAN requirements.

- Use
  [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  instead of
  [`parameters()`](https://dials.tidymodels.org/reference/parameters.html)
  to get parameter sets.

- Removed some pillar-related S3 methods that currently live in tune.

## finetune 0.1.1

CRAN release: 2022-02-21

- [`tune_sim_anneal()`](https://finetune.tidymodels.org/dev/reference/tune_sim_anneal.md)
  only overwrites tuning parameter information when they originally
  contain unknowns.

## finetune 0.1.0

CRAN release: 2021-07-21

- A check was added to make sure that `lme4` or `BradleyTerry2` are
  installed ([\#8](https://github.com/tidymodels/finetune/issues/8))

- Added `pillar` methods for formatting `tune` objects in list columns.

- Fixed bug in `random_integer_neighbor_calc()` to keep values inside
  range ([\#10](https://github.com/tidymodels/finetune/issues/10))

- [`tune_sim_anneal()`](https://finetune.tidymodels.org/dev/reference/tune_sim_anneal.md)
  now retains a finalized parameter set and replaces any existing
  parameter set that was not finalized
  ([\#14](https://github.com/tidymodels/finetune/issues/14))

- A bug in win/loss racing was fixed for cases when one tuning parameter
  had results that were so bad that it broke the Bradley-Terry model
  ([\#7](https://github.com/tidymodels/finetune/issues/7))

## finetune 0.0.1

CRAN release: 2020-11-20

- First CRAN release
