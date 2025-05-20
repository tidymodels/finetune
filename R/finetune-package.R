#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# ------------------------------------------------------------------------------

#' @importFrom stats qt runif coef confint lm reorder rnorm setNames dist
#' @importFrom utils globalVariables
#' @importFrom rlang syms caller_env
#' @importFrom dplyr distinct
#' @importFrom utils globalVariables
#' @import tune
NULL

# ------------------------------------------------------------------------------

# fmt: skip
utils::globalVariables(
  c(
    ".config", ".estimate", ".iter", ".metric", ".parent", "B", "Estimate",
    "Std. Error", "lower", "metric_1", "metric_2", "n", "no_improve", "p1",
    "p2", "pair", "pass", "player", "player_1", "player_2", "std_err", "upper",
    "value", "wins", "wins_1", "wins_2", ".metrics", ".order", "id", "new",
    "orig", "stage", "symb", "id2", ".rand", ".eval_time"
  )
)
