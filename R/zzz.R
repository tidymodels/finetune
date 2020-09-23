#' @importFrom stats qt runif coef confint lm reorder rnorm setNames
#' @importFrom utils globalVariables
#' @importFrom rlang syms
#' @importFrom tibble tibble
#' @importFrom dplyr %>% distinct
#' @importFrom utils globalVariables
#' @import tune
NULL

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    ".config", ".estimate", ".iter", ".metric", ".parent", "B", "Estimate",
    "Std. Error", "lower", "metric_1", "metric_2", "n", "no_improve", "p1",
    "p2", "pair", "pass", "player", "player_1", "player_2", "std_err", "upper",
    "value", "wins", "wins_1", "wins_2", ".metrics", ".order", "id", "new",
    "orig", "stage", "symb", "id2"
  )
)


