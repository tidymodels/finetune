context("anova racing")

## -----------------------------------------------------------------------------

library(parsnip)
library(rsample)
library(dplyr)
library(lme4)
library(yardstick)

## -----------------------------------------------------------------------------

set.seed(2332)
folds <- vfold_cv(mtcars, v = 5, repeats = 2)
spec <- decision_tree(cost_complexity = tune(), min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")
grid <- expand.grid(cost_complexity = c(0.001, 0.01), min_n = c(2:5))
grid_res <-
  spec %>% tune_grid(mpg ~ ., folds, grid = grid, metrics = metric_set(rmse))
rmse_means <- collect_metrics(grid_res)
configs <- rmse_means$.config[order(rmse_means$mean)]
rmse_vals <- collect_metrics(grid_res, summarize = FALSE)
rmse_configs <- rmse_vals
rmse_configs$.config <- factor(rmse_configs$.config, levels = configs)
rmse_configs <- rmse_configs[, c("id", "id2", ".estimate", ".config")]
rmse_mod <- lmer(.estimate ~ .config + (1| id2/id), data = rmse_configs)
rmse_summary <- summary(rmse_mod)$coef
rmse_res <- tibble::as_tibble(rmse_summary)
rmse_res$.config <- gsub("\\.config", "", rownames(rmse_summary))
rmse_res$.config <- gsub("(Intercept)", configs[1], rmse_res$.config , fixed = TRUE)
rmse_ci <- confint(rmse_mod, level = 1 - 0.0381, method = "Wald", quiet = TRUE)
rmse_ci <- rmse_ci[grepl("config", rownames(rmse_ci)),]

## -----------------------------------------------------------------------------

test_that('anova filtering and logging', {
  param <- .get_tune_parameter_names(ames_grid_search)
  grid_res <- collect_metrics(ames_grid_search)
  grid_res <- grid_res[grid_res$.metric == "rmse",]

  anova_res <- finetune:::test_parameters_gls(ames_grid_search)
  expect_equal(
    names(anova_res),
    c(".config", "lower", "upper", "estimate", "pass", "K", "weight_func",
      "dist_power", "lon", "lat")
  )
  expect_equal(nrow(anova_res), nrow(grid_res))
  expect_equal(anova_res$lower <= 0, anova_res$pass)
  expect_equal(
    anova_res %>% dplyr::select(!!!param, .config) %>% arrange(.config),
    grid_res %>% dplyr::select(!!!param, .config) %>% arrange(.config)
  )

  expect_message(
    finetune:::log_racing(control_race(), anova_res, ames_grid_search$splits, 10, "rmse"),
    "Fold10"
  )
  expect_message(
    finetune:::log_racing(control_race(), anova_res, ames_grid_search$splits, 10, "rmse"),
    "7 eliminated"
  )
  expect_message(
    finetune:::log_racing(control_race(), anova_res, ames_grid_search$splits, 10, "rmse"),
    "3 candidates remain"
  )
})

## -----------------------------------------------------------------------------

test_that('anova refactoring', {
  res <- finetune:::refactor_by_mean(rmse_vals, maximize = FALSE)
  expect_equal(res, rmse_configs)
})


test_that('anova results', {
  anova_res <- finetune:::fit_anova(grid_res, rmse_configs, alpha  = 0.0381)
  expect_equal(anova_res$estimate, rmse_res$Estimate[-1])
  expect_equal(anova_res$lower, unname(rmse_ci[,1]))
  expect_equal(anova_res$upper, unname(rmse_ci[,2]))
  expect_equal(anova_res$.config, configs[-1])
})
