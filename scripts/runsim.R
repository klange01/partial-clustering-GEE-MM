## Performance of GEEs vs mixed models for analysing partially clustered data
## Kylie Lange (kylie.lange@adelaide.edu.au)

# Simulation of partially clustered data
# Independent and paired data
# Paired clusters are generated randomly with a specified probability
# Continuous outcome

# Treatment group allocation (2 groups):
# Cluster randomisation
# Individual randomisation
# Balanced (opposite) treatment randomisation

# Outcome model:
# Fixed effects: overall mean, treatment
# Random effects: cluster, residual error

# Each dataset analysed with:
# linear mixed effects model
# GEE (independence or exchangeable)

rm(list = ls())

library(tidyverse)
library(rsimsum)
library(lme4)
library(lmerTest)
library(geepack)

source("R/partial_clus_functions.R")
source("R/scenarios.R")

# For each randomisation method and design effect, create simulated datasets that are compatible for use by rsimum,
# then analyse each dataset with mixed models and GEEs.
# Datasets, random seeds, and model results are saved to file.
# Datasets may need to be removed and memory released at intervals throughout. Eg:
# rm(list = ls(pattern = "^data"))
# gc()


###  CLUSTER RAND, INDEPENDENCE DE

## non-null scenarios
data_ClusInd = simulate(scenarios_ClusInd, 5000)
saveRDS(data_ClusInd, file="/simdata/data_cluster_ind.rds")

tm = proc.time()
model_results_ClusInd = modify_depth(data_ClusInd, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
model_results_ClusInd = bind_rows(map(model_results_ClusInd, bind_rows))
saveRDS(model_results_ClusInd, file="/simdata/model_results_ClusInd.rds")

## null scenarios
null_data_ClusInd = simulate(scenarios_null_ClusInd, 5000)
saveRDS(null_data_ClusInd, file="/simdata/null_data_cluster_ind.rds")

tm = proc.time()
null_model_results_ClusInd = modify_depth(null_data_ClusInd, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
null_model_results_ClusInd = bind_rows(map(null_model_results_ClusInd, bind_rows))
null_model_results_ClusInd = id_null_scenarios(null_model_results_ClusInd)
saveRDS(null_model_results_ClusInd, file="/simdata/null_model_results_ClusInd.rds")


###  CLUSTER RAND, EXCHANGEABLE DE

## non-null scenarios
data_ClusExch = simulate(scenarios_ClusExch, 5000)
saveRDS(data_ClusExch, file="/simdata/data_cluster_exch.rds")

tm = proc.time()
model_results_ClusExch = modify_depth(data_ClusExch, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
model_results_ClusExch = bind_rows(map(model_results_ClusExch, bind_rows))
saveRDS(model_results_ClusExch, file="/simdata/model_results_ClusExch.rds")

## null scenarios
null_data_ClusExch = simulate(scenarios_null_ClusExch, 5000)
saveRDS(null_data_ClusExch, file="/simdata/null_data_cluster_exch.rds")

tm = proc.time()
null_model_results_ClusExch = modify_depth(null_data_ClusExch, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
null_model_results_ClusExch = bind_rows(map(null_model_results_ClusExch, bind_rows))
null_model_results_ClusExch = id_null_scenarios(null_model_results_ClusExch)
saveRDS(null_model_results_ClusExch, file="/simdata/null_model_results_ClusExch.rds")


###  INDIVIDUAL RAND, INDEPENDENCE DE

## non-null scenarios
data_IndInd = simulate(scenarios_IndInd, 5000)
saveRDS(data_IndInd, file="/simdata/data_indiv_ind.rds")

tm = proc.time()
model_results_IndInd = modify_depth(data_IndInd, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
model_results_IndInd = bind_rows(map(model_results_IndInd, bind_rows))
saveRDS(model_results_IndInd, file="/simdata/model_results_IndInd.rds")

## null scenarios
null_data_IndInd = simulate(scenarios_null_IndInd, 5000)
saveRDS(null_data_IndInd, file="/simdata/null_data_indiv_ind.rds")

tm = proc.time()
null_model_results_IndInd = modify_depth(null_data_IndInd, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
null_model_results_IndInd = bind_rows(map(null_model_results_IndInd, bind_rows))
null_model_results_IndInd = id_null_scenarios(null_model_results_IndInd)
saveRDS(null_model_results_IndInd, file="/simdata/null_model_results_IndInd.rds")


###  INDIVIDUAL RAND, EXCHANGEABLE DE

## non-null scenarios
data_IndExch = simulate(scenarios_IndExch, 5000)
saveRDS(data_IndExch, file="/simdata/data_indiv_exch.rds")

tm = proc.time()
model_results_IndExch = modify_depth(data_IndExch, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
model_results_IndExch = bind_rows(map(model_results_IndExch, bind_rows))
saveRDS(model_results_IndExch, file="/simdata/model_results_IndExch.rds")

## null scenarios
null_data_IndExch = simulate(scenarios_null_IndExch, 5000)
saveRDS(null_data_IndExch, file="/simdata/null_data_indiv_exch.rds")

tm = proc.time()
null_model_results_IndExch = modify_depth(null_data_IndExch, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
null_model_results_IndExch = bind_rows(map(null_model_results_IndExch, bind_rows))
null_model_results_IndExch = id_null_scenarios(null_model_results_IndExch)
saveRDS(null_model_results_IndExch, file="/simdata/null_model_results_IndExch.rds")


###  BALANCED RAND, INDEPENDENCE DE

## non-null scenarios
data_BalInd = simulate(scenarios_BalInd, 5000)
saveRDS(data_BalInd, file="/simdata/data_opp_ind.rds")

tm = proc.time()
model_results_BalInd = modify_depth(data_BalInd, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
model_results_BalInd = bind_rows(map(model_results_BalInd, bind_rows))
saveRDS(model_results_BalInd, file="/simdata/model_results_BalInd.rds")

## null scenarios
null_data_BalInd = simulate(scenarios_null_BalInd, 5000)
saveRDS(null_data_BalInd, file="/simdata/null_data_opp_ind.rds")

tm = proc.time()
null_model_results_BalInd = modify_depth(null_data_BalInd, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
null_model_results_BalInd = bind_rows(map(null_model_results_BalInd, bind_rows))
null_model_results_BalInd = id_null_scenarios(null_model_results_BalInd)
saveRDS(null_model_results_BalInd, file="/simdata/null_model_results_BalInd.rds")


###  BALANCED RAND, EXCHANGEABLE DE

## non-null scenarios
data_BalExch = simulate(scenarios_BalExch, 5000)
saveRDS(data_BalExch, file="/simdata/data_opp_exch.rds")

tm = proc.time()
model_results_BalExch = modify_depth(data_BalExch, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
model_results_BalExch = bind_rows(map(model_results_BalExch, bind_rows))
saveRDS(model_results_BalExch, file="/simdata/model_results_BalExch.rds")

## null scenarios
null_data_BalExch = simulate(scenarios_null_BalExch, 5000)
saveRDS(null_data_BalExch, file="/simdata/null_data_opp_exch.rds")

tm = proc.time()
null_model_results_BalExch = modify_depth(null_data_BalExch, .depth=2, ~fit_models(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
null_model_results_BalExch = bind_rows(map(null_model_results_BalExch, bind_rows))
null_model_results_BalExch = id_null_scenarios(null_model_results_BalExch)
saveRDS(null_model_results_BalExch, file="/simdata/null_model_results_BalExch.rds")
