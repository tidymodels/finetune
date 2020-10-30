context("win/loss racing")

## -----------------------------------------------------------------------------

library(parsnip)
library(rsample)
library(dplyr)
library(lme4)
library(yardstick)
library(workflows)
library(recipes)

## -----------------------------------------------------------------------------

set.seed(2332)
folds <- vfold_cv(mtcars, v = 5, repeats = 2)
fold_att <- attributes(folds)
spec <- decision_tree(cost_complexity = tune(), min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")
wflow <- workflow() %>% add_model(spec) %>% add_formula(mpg ~ .)
grid <- expand.grid(cost_complexity = c(0.001, 0.01), min_n = c(2:5))
rec <- recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors())


## -----------------------------------------------------------------------------

test_that('top-level win/loss filter interfaces', {
  skip_on_cran()
  expect_error(
      expect_warning({
        set.seed(129)
        wl_mod <- spec %>% tune_race_win_loss(mpg ~ ., folds, grid = grid)
      },
      "non-integer counts in a binomial glm"
      ),
    regexp = NA
  )
  expect_true(inherits(wl_mod, "tune_race"))
  expect_true(inherits(wl_mod, "tune_results"))
  expect_true(tibble::is_tibble((wl_mod)))

  expect_silent(
    expect_error(
      expect_warning({
        set.seed(129)
        wl_wlfow <-
          wflow %>%
          tune_race_win_loss(folds, grid = grid,
                             control = control_race(verbose_elim = FALSE, save_pred = TRUE))
      },
      "non-integer counts in a binomial glm"
      ),
      regexp = NA
    )
  )
  expect_true(inherits(wl_wlfow, "tune_race"))
  expect_true(inherits(wl_wlfow, "tune_results"))
  expect_true(tibble::is_tibble((wl_wlfow)))
  expect_true(sum(names(wl_wlfow) == ".predictions") == 1)

  get_mod <- function(x) pull_workflow_fit(x)
  expect_message(
    expect_error(
      expect_warning({
        set.seed(129)
        wl_rec <-
          spec %>%
          tune_race_win_loss(rec, folds, grid = 5,
                             control = control_race(verbose_elim = FALSE, extract = get_mod))
      },
      "non-integer counts in a binomial glm"
      ),
      regexp = NA
    ),
    "correlation computation is required"
  )
  expect_true(inherits(wl_rec, "tune_race"))
  expect_true(inherits(wl_rec, "tune_results"))
  expect_true(tibble::is_tibble((wl_rec)))
  expect_true(sum(names(wl_rec) == ".extracts") == 1)

})
