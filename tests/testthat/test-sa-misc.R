library(finetune)
library(tune)
library(dplyr)

## -----------------------------------------------------------------------------

load(file.path(test_path(), "sa_cart_test_objects.RData"))

## -----------------------------------------------------------------------------

cart_param <- tune::.get_tune_parameters(cart_search)
cart_metrics <- tune::.get_tune_metrics(cart_search)
cart_outcomes <- tune::.get_tune_outcome_names(cart_search)
cart_rset_info <- attributes(cart_search)$rset_info

## -----------------------------------------------------------------------------

test_that('tune_sim_anneal attributes', {

  # placeholder
 expect_true(TRUE)

})
