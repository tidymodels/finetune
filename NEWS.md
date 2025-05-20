# finetune (development version)

# finetune 1.2.1

* Maintenance release required by CRAN.

* Transition from the magrittr pipe to the base R pipe.

# finetune 1.2.0

## New Features

* finetune now fully supports models in the "censored regression" mode. These models can be fit, tuned, and evaluated like the regression and classification modes. [tidymodels.org](https://www.tidymodels.org/learn/#category=survival%20analysis) has more information and tutorials on how to work with survival analysis models.

* Improved error message from `tune_sim_anneal()` when values in the supplied `param_info` do not encompass all values evaluated in the `initial` grid. This most often happens when a user mistakenly supplies different parameter sets to the function that generated the initial results and `tune_sim_anneal()`.

* `autoplot()` methods for racing objects will now use integers in x-axis breaks (#75).

* Enabling the `verbose_elim` control option for `tune_race_anova()` will now additionally introduce a message confirming that the function is evaluating against the burn-in resamples.

* Updates based on the new version of tune, primarily for survival analysis models (#104). 

## Bug Fixes

* Fixed bug where `tune_sim_anneal()` would fail when supplied parameters needing finalization. The function will now finalize needed parameter ranges internally (#39).

* Fixed bug where packages specified in `control_race(pkgs)` were not actually loaded in `tune_race_anova()` (#74).

## Breaking Change

* Ellipses (...) are now used consistently in the package to require optional arguments to be named. `collect_predictions()`, `collect_metrics()` and `show_best()` methods previously had ellipses at the end of the function signature  that have been moved to follow the last argument without a default value. Optional arguments previously passed by position will now error informatively prompting them to be named (#105).

# finetune 1.1.0

* Various minor changes to keep up with developments in the tune and dplyr packages (#60) (#62) (#67) (#68). 

* Corrects `.config` output with `save_pred = TRUE` in `tune_sim_anneal()`. The function previously outputted a constant `Model1_Preprocessor1` in the `.predictions` slot, and now provides `.config` values that align with those in `.metrics` (#57).

* An `eval_time` attribute was added to tune objects produced by finetune. 

# finetune 1.0.1

* For racing: 
 - `collect_metrics()` and `collect_predictions()` have a `'complete'` argument that only returns results for model configurations that were fully resampled. 
 - `select_best()` and `show_best()` now only show results for model configurations that were fully resampled. 

* `tune_race_anova()`, `tune_race_win_loss()`, and `tune_sim_anneal()` no longer error if `control` argument isn't a the corresponding `control_*()` object. Will work as long as the object passed to `control` includes the same elements as the required `control_*()` object.

* The `control_sim_anneal()` got a new argument `verbose_iter` that is used to control the verbosity of the iterative calculations. This change means that the `verbose` argument is being passed to `tune_grid()` to control its verbosity.


# finetune 1.0.0

* An informative error is given when there are not enough resamples for racing (#33).

* `tune_sim_anneal()` was not passing all arguments to `tune_grid()` (#40). 

# finetune 0.2.0

* Maintenance release for CRAN requirements. 

* Use `extract_parameter_set_dials()` instead of `parameters()` to get parameter sets. 

* Removed some pillar-related S3 methods that currently live in tune. 

# finetune 0.1.1

* `tune_sim_anneal()` only overwrites tuning parameter information when they originally contain unknowns.

# finetune 0.1.0

* A check was added to make sure that `lme4` or `BradleyTerry2` are installed (#8)

* Added `pillar` methods for formatting `tune` objects in list columns. 

* Fixed bug in `random_integer_neighbor_calc()` to keep values inside range (#10)

* `tune_sim_anneal()` now retains a finalized parameter set and replaces any existing parameter set that was not finalized (#14)

* A bug in win/loss racing was fixed for cases when one tuning parameter had results that were so bad that it broke the Bradley-Terry model (#7)

# finetune 0.0.1

* First CRAN release
