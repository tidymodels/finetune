
suppressPackageStartupMessages(library(finetune))

# CRAN wants packages to be able to be check without the Suggests dependencies
if (rlang::is_installed(c("modeldata", "lme4", "testthat"))) {
  suppressPackageStartupMessages(library(testthat))
  test_check("finetune")
}
