## -----------------------------------------------------------------------------

test_that("anova filtering and logging", {
  # Skip for < 4.0 due to random number differences
  skip_if(getRversion() < "4.0.0")
  skip_if_not_installed("Matrix", "1.6-2")
  skip_if_not_installed("lme4", "1.1-35.1")

  set.seed(2332)
  folds <- vfold_cv(mtcars, v = 5, repeats = 2)
  fold_att <- attributes(folds)
  spec <-
    decision_tree(cost_complexity = tune(), min_n = tune()) |>
    set_engine("rpart") |>
    set_mode("regression")
  wflow <- workflow() |>
    add_model(spec) |>
    add_formula(mpg ~ .)
  grid <- expand.grid(cost_complexity = c(0.001, 0.01), min_n = c(2:5))

  ## -----------------------------------------------------------------------------

  grid_res <-
    spec |> tune_grid(mpg ~ ., folds, grid = grid, metrics = metric_set(rmse))
  # Pull out rmse values, format them to emulate the racing tests then
  # use lme4 package to create the model results for removing configurations.

  alpha <- 0.0381

  rmse_means <- collect_metrics(grid_res)
  configs <- rmse_means$.config[order(rmse_means$mean)]
  rmse_vals <- collect_metrics(grid_res, summarize = FALSE)
  rmse_configs <- rmse_vals
  rmse_configs$.config <- factor(rmse_configs$.config, levels = configs)
  rmse_configs <- rmse_configs[, c("id", "id2", ".estimate", ".config")]
  rmse_mod <- lmer(.estimate ~ .config + (1 | id2 / id), data = rmse_configs)
  rmse_summary <- summary(rmse_mod)$coef
  rmse_res <- tibble::as_tibble(rmse_summary)
  rmse_res$.config <- gsub("\\.config", "", rownames(rmse_summary))
  rmse_res$.config <- gsub(
    "(Intercept)",
    configs[1],
    rmse_res$.config,
    fixed = TRUE
  )
  rmse_ci <- confint(rmse_mod, level = 1 - alpha, method = "Wald", quiet = TRUE)
  rmse_ci <- rmse_ci[grepl("config", rownames(rmse_ci)), ]

  # ------------------------------------------------------------------------------
  # anova results

  anova_res <- finetune:::fit_anova(grid_res, rmse_configs, alpha = alpha)
  expect_equal(anova_res$estimate, rmse_res$Estimate[-1])
  expect_equal(anova_res$lower, unname(rmse_ci[, 1]))
  expect_equal(anova_res$upper, unname(rmse_ci[, 2]))
  expect_equal(anova_res$.config, configs[-1])

  # ------------------------------------------------------------------------------
  # top-level anova filter interfaces

  expect_snapshot({
    set.seed(129)
    anova_mod <- spec |> tune_race_anova(mpg ~ ., folds, grid = grid)
  })
  expect_true(inherits(anova_mod, "tune_race"))
  expect_true(inherits(anova_mod, "tune_results"))
  expect_true(tibble::is_tibble((anova_mod)))

  expect_silent({
    set.seed(129)
    anova_wlfow <-
      wflow |>
      tune_race_anova(
        folds,
        grid = grid,
        control = control_race(verbose_elim = FALSE, save_pred = TRUE)
      )
  })
  expect_true(inherits(anova_wlfow, "tune_race"))
  expect_true(inherits(anova_wlfow, "tune_results"))
  expect_true(tibble::is_tibble((anova_wlfow)))
  expect_true(sum(names(anova_wlfow) == ".predictions") == 1)

  ## -----------------------------------------------------------------------------
  ## anova formula

  for (i in 2:nrow(folds)) {
    f <- finetune:::lmer_formula(folds |> slice(1:i), fold_att)
    if (i < 7) {
      expect_equal(f, .estimate ~ .config + (1 | .all_id), ignore_attr = TRUE)
    } else {
      expect_equal(f, .estimate ~ .config + (1 | id2 / id), ignore_attr = TRUE)
    }
  }
  # This one takes a while to run:
  expect_equal(environment(f), rlang::base_env())

  car_bt <- bootstraps(mtcars, times = 5)
  car_att <- attributes(car_bt)

  for (i in 2:nrow(car_bt)) {
    f <- finetune:::lmer_formula(car_bt |> slice(1:i), car_att)
    expect_equal(f, .estimate ~ .config + (1 | id), ignore_attr = TRUE)
  }
  expect_equal(environment(f), rlang::base_env())

  res <- finetune:::refactor_by_mean(rmse_vals, maximize = FALSE)
  expect_equal(res, rmse_configs)

  # ------------------------------------------------------------------------------

  # Ue the built-in `ames_grid_search` object to test the object structure andU
  # printing

  param <- .get_tune_parameter_names(ames_grid_search)
  ames_grid_res <- collect_metrics(ames_grid_search)
  ames_grid_res <- ames_grid_res[ames_grid_res$.metric == "rmse", ]

  anova_res <- finetune:::test_parameters_gls(ames_grid_search)
  expect_equal(
    names(anova_res),
    # fmt: skip
    c(
      ".config", "lower", "upper", "estimate", "pass", "K", "weight_func",
      "dist_power", "lon", "lat"
    )
  )
  expect_equal(nrow(anova_res), nrow(ames_grid_res))
  expect_equal(anova_res$lower <= 0, anova_res$pass)
  expect_equal(
    anova_res |> dplyr::select(!!!param, .config) |> arrange(.config),
    ames_grid_res |> dplyr::select(!!!param, .config) |> arrange(.config)
  )

  expect_snapshot(
    finetune:::log_racing(
      control_race(verbose_elim = TRUE),
      anova_res,
      ames_grid_search$splits,
      10,
      "rmse"
    )
  )
  expect_snapshot(
    finetune:::log_racing(
      control_race(verbose_elim = TRUE),
      anova_res,
      ames_grid_search$splits,
      10,
      "rmse"
    )
  )
  expect_snapshot(
    finetune:::log_racing(
      control_race(verbose_elim = TRUE),
      anova_res,
      ames_grid_search$splits,
      10,
      "rmse"
    )
  )
})
