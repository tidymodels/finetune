library(testthat)
library(finetune)

# CRAN wants packages to be able to be check without the Suggests dependencies
if (rlang::is_installed(c("modeldata", "lme4"))) {
  test_check("finetune")
}
