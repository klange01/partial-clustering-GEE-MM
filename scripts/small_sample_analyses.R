## Performance of GEEs vs mixed models for analysing partially clustered data
## Kylie Lange (kylie.lange@adelaide.edu.au)

# SMALL SAMPLE BIAS CORRECTIONS
# Sub-study to assess performance of analysis methods when small sample corrections are applied

# Considers a subset of scenarios:
# ES = 0.8 & ppair = 0.2, 0.4, 0.7 & ICC = 0.1, 0.5, 0.9
# Each dataset analysed with:
# linear mixed effects model with Kenward-Roger degrees of freedom
# GEE with the Mancl-DeRouen small sample bias correction

library(tidyverse)
library(rsimsum)
library(lme4)
library(lmerTest)
library(pbkrtest)
library(geepack)
library(geesmv)
library(matrixcalc)

source("R/small_sample_functions.R")
source("R/partial_clus_functions.R")
source("R/scenarios.R")

###  CLUSTER RAND, INDEPENDENCE DE

# non-null scenarios
data_B8 = readRDS("/simdata/data_B.rds")[[3]]
data_C8 = readRDS("/simdata/data_C.rds")[[3]]
data_D8 = readRDS("/simdata/data_D.rds")[[3]]
data_F8 = readRDS("/simdata/data_F.rds")[[3]]
data_G8 = readRDS("/simdata/data_G.rds")[[3]]
data_H8 = readRDS("/simdata/data_H.rds")[[3]]
data_J8 = readRDS("/simdata/data_J.rds")[[3]]
data_K8 = readRDS("/simdata/data_K.rds")[[3]]
data_L8 = readRDS("/simdata/data_L.rds")[[3]]
small_data_ClusInd = list(data_B8, data_C8, data_D8, data_F8, data_G8, data_H8, data_J8, data_K8, data_L8)

tm = proc.time()
small_results_ClusInd = modify_depth(small_data_ClusInd, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_results_ClusInd = bind_rows(map(small_results_ClusInd, bind_rows))
saveRDS(small_results_ClusInd, file="/simdata/small_results_ClusInd.rds")

# null scenarios
null_data_B8 = readRDS("/simdata/null_data_B.rds")[[3]]
null_data_C8 = readRDS("/simdata/null_data_C.rds")[[3]]
null_data_D8 = readRDS("/simdata/null_data_D.rds")[[3]]
null_data_F8 = readRDS("/simdata/null_data_F.rds")[[3]]
null_data_G8 = readRDS("/simdata/null_data_G.rds")[[3]]
null_data_H8 = readRDS("/simdata/null_data_H.rds")[[3]]
null_data_J8 = readRDS("/simdata/null_data_J.rds")[[3]]
null_data_K8 = readRDS("/simdata/null_data_K.rds")[[3]]
null_data_L8 = readRDS("/simdata/null_data_L.rds")[[3]]
small_null_data_ClusInd = list(null_data_B8, null_data_C8, null_data_D8, null_data_F8,
                               null_data_G8, null_data_H8, null_data_J8, null_data_K8, null_data_L8)

tm = proc.time()
small_null_results_ClusInd = modify_depth(small_null_data_ClusInd, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_null_results_ClusInd = bind_rows(map(small_null_results_ClusInd, bind_rows))
small_null_results_ClusInd = id_null_scenarios(small_null_results_ClusInd)
saveRDS(small_null_results_ClusInd, file="/simdata/small_null_results_ClusInd.rds")


###  CLUSTER RAND, EXCHANGEABLE DE

# non-null scenarios
data_N8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_N.rds")[[3]]
data_O8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_O.rds")[[3]]
data_P8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_P.rds")[[3]]
data_R8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_R.rds")[[3]]
data_S8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_S.rds")[[3]]
data_T8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_T.rds")[[3]]
data_V8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_V.rds")[[3]]
data_W8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_W.rds")[[3]]
data_X8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_X.rds")[[3]]
small_data_ClusExch = list(data_N8, data_O8, data_P8, data_R8, data_S8, data_T8, data_V8, data_W8, data_X8)

tm = proc.time()
small_results_ClusExch = modify_depth(small_data_ClusExch, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_results_ClusExch = bind_rows(map(small_results_ClusExch, bind_rows))
saveRDS(small_results_ClusExch, file="/simdata/small_results_ClusExch.rds")

# null scenarios
null_data_N8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_N.rds")[[3]]
null_data_O8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_O.rds")[[3]]
null_data_P8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_P.rds")[[3]]
null_data_R8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_R.rds")[[3]]
null_data_S8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_S.rds")[[3]]
null_data_T8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_T.rds")[[3]]
null_data_V8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_V.rds")[[3]]
null_data_W8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_W.rds")[[3]]
null_data_X8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_X.rds")[[3]]
small_null_data_ClusExch = list(null_data_N8, null_data_O8, null_data_P8, null_data_R8,
                                null_data_S8, null_data_T8, null_data_V8, null_data_W8, null_data_X8)

tm = proc.time()
small_null_results_ClusExch = modify_depth(small_null_data_ClusExch, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_null_results_ClusExch = bind_rows(map(small_null_results_ClusExch, bind_rows))
small_null_results_ClusExch = id_null_scenarios(small_null_results_ClusExch)
saveRDS(small_null_results_ClusExch, file="/simdata/small_null_results_ClusExch.rds")


###  INDIVIDUAL RAND, INDEPENDENCE DE

# non-null scenarios
data_Z8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_Z.rds")[[3]]
data_AA8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AA.rds")[[3]]
data_AB8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AB.rds")[[3]]
data_AD8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AD.rds")[[3]]
data_AE8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AE.rds")[[3]]
data_AF8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AF.rds")[[3]]
data_AH8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AH.rds")[[3]]
data_AI8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AI.rds")[[3]]
data_AJ8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AJ.rds")[[3]]
small_data_IndInd = list(data_Z8, data_AA8, data_AB8, data_AD8, data_AE8, data_AF8, data_AH8, data_AI8, data_AJ8)

tm = proc.time()
small_results_IndInd = modify_depth(small_data_IndInd, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_results_IndInd = bind_rows(map(small_results_IndInd, bind_rows))
saveRDS(small_results_IndInd, file="/simdata/small_results_IndInd.rds")

# null scenarios
null_data_Z8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_Z.rds")[[3]]
null_data_AA8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AA.rds")[[3]]
null_data_AB8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AB.rds")[[3]]
null_data_AD8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AD.rds")[[3]]
null_data_AE8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AE.rds")[[3]]
null_data_AF8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AF.rds")[[3]]
null_data_AH8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AH.rds")[[3]]
null_data_AI8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AI.rds")[[3]]
null_data_AJ8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AJ.rds")[[3]]
small_null_data_IndInd = list(null_data_Z8, null_data_AA8, null_data_AB8, null_data_AD8,
                              null_data_AE8, null_data_AF8, null_data_AH8, null_data_AI8, null_data_AJ8)

tm = proc.time()
small_null_results_IndInd = modify_depth(small_null_data_IndInd, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_null_results_IndInd = bind_rows(map(small_null_results_IndInd, bind_rows))
small_null_results_IndInd = id_null_scenarios(small_null_results_IndInd)
saveRDS(small_null_results_IndInd, file="/simdata/small_null_results_IndInd.rds")


###  INDIVIDUAL RAND, EXCHANGEABLE DE

# non-null scenarios
data_AL8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AL.rds")[[3]]
data_AM8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AM.rds")[[3]]
data_AN8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AN.rds")[[3]]
data_AP8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AP.rds")[[3]]
data_AQ8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AQ.rds")[[3]]
data_AR8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AR.rds")[[3]]
data_AT8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AT.rds")[[3]]
data_AU8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AU.rds")[[3]]
data_AV8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AV.rds")[[3]]
small_data_IndExch = list(data_AL8, data_AM8, data_AN8, data_AP8, data_AQ8, data_AR8, data_AT8, data_AU8, data_AV8)

tm = proc.time()
small_results_IndExch = modify_depth(small_data_IndExch, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_results_IndExch = bind_rows(map(small_results_IndExch, bind_rows))
saveRDS(small_results_IndExch, file="/simdata/small_results_IndExch.rds")

# null scenarios
null_data_AL8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AL.rds")[[3]]
null_data_AM8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AM.rds")[[3]]
null_data_AN8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AN.rds")[[3]]
null_data_AP8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AP.rds")[[3]]
null_data_AQ8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AQ.rds")[[3]]
null_data_AR8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AR.rds")[[3]]
null_data_AT8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AT.rds")[[3]]
null_data_AU8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AU.rds")[[3]]
null_data_AV8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AV.rds")[[3]]
small_null_data_IndExch = list(null_data_AL8, null_data_AM8, null_data_AN8, null_data_AP8,
                               null_data_AQ8, null_data_AR8, null_data_AT8, null_data_AU8, null_data_AV8)

tm = proc.time()
small_null_results_IndExch = modify_depth(small_null_data_IndExch, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_null_results_IndExch = bind_rows(map(small_null_results_IndExch, bind_rows))
small_null_results_IndExch = id_null_scenarios(small_null_results_IndExch)
saveRDS(small_null_results_IndExch, file="/simdata/small_null_results_IndExch.rds")


###  BALANCED RAND, INDEPENDENCE DE

# non-null scenarios
data_AX8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AX.rds")[[3]]
data_AY8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AY.rds")[[3]]
data_AZ8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_AZ.rds")[[3]]
data_BB8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BB.rds")[[3]]
data_BC8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BC.rds")[[3]]
data_BD8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BD.rds")[[3]]
data_BF8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BF.rds")[[3]]
data_BG8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BG.rds")[[3]]
data_BH8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BH.rds")[[3]]
small_data_BalInd = list(data_AX8, data_AY8, data_AZ8, data_BB8, data_BC8, data_BD8, data_BF8, data_BG8, data_BH8)

tm = proc.time()
small_results_BalInd = modify_depth(small_data_BalInd, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_results_BalInd = bind_rows(map(small_results_BalInd, bind_rows))
saveRDS(small_results_BalInd, file="/simdata/small_results_BalInd.rds")

# null scenarios
null_data_AX8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AX.rds")[[3]]
null_data_AY8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AY.rds")[[3]]
null_data_AZ8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_AZ.rds")[[3]]
null_data_BB8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BB.rds")[[3]]
null_data_BC8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BC.rds")[[3]]
null_data_BD8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BD.rds")[[3]]
null_data_BF8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BF.rds")[[3]]
null_data_BG8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BG.rds")[[3]]
null_data_BH8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BH.rds")[[3]]
small_null_data_BalInd = list(null_data_AX8, null_data_AY8, null_data_AZ8, null_data_BB8,
                              null_data_BC8, null_data_BD8, null_data_BF8, null_data_BG8, null_data_BH8)

tm = proc.time()
small_null_results_BalInd = modify_depth(small_null_data_BalInd, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_null_results_BalInd = bind_rows(map(small_null_results_BalInd, bind_rows))
small_null_results_BalInd = id_null_scenarios(small_null_results_BalInd)
saveRDS(small_null_results_BalInd, file="/simdata/small_null_results_BalInd.rds")


###  BALANCED RAND, EXCHANGEABLE DE

# non-null scenarios
data_BJ8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BJ.rds")[[3]]
data_BK8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BK.rds")[[3]]
data_BL8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BL.rds")[[3]]
data_BN8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BN.rds")[[3]]
data_BO8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BO.rds")[[3]]
data_BP8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BP.rds")[[3]]
data_BR8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BR.rds")[[3]]
data_BS8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BS.rds")[[3]]
data_BT8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/data_BT.rds")[[3]]
small_data_BalExch = list(data_BJ8, data_BK8, data_BL8, data_BN8, data_BO8, data_BP8, data_BR8, data_BS8, data_BT8)

tm = proc.time()
small_results_BalExch = modify_depth(small_data_BalExch, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_results_BalExch = bind_rows(map(small_results_BalExch, bind_rows))
saveRDS(small_results_BalExch, file="/simdata/small_results_BalExch.rds")

# null scenarios
null_data_BJ8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BJ.rds")[[3]]
null_data_BK8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BK.rds")[[3]]
null_data_BL8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BL.rds")[[3]]
null_data_BN8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BN.rds")[[3]]
null_data_BO8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BO.rds")[[3]]
null_data_BP8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BP.rds")[[3]]
null_data_BR8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BR.rds")[[3]]
null_data_BS8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BS.rds")[[3]]
null_data_BT8 = readRDS("D:/git/partial-clustering-simulations/output/simdata/null_data_BT.rds")[[3]]
small_null_data_BalExch = list(null_data_BJ8, null_data_BK8, null_data_BL8, null_data_BN8,
                               null_data_BO8, null_data_BP8, null_data_BR8, null_data_BS8, null_data_BT8)

tm = proc.time()
small_null_results_BalExch = modify_depth(small_null_data_BalExch, .depth=2, ~fit_models_smallsample(.x, .x$rand_method[1], .x$gee_method[1]))
etm = proc.time() - tm
etm
small_null_results_BalExch = bind_rows(map(small_null_results_BalExch, bind_rows))
small_null_results_BalExch = id_null_scenarios(small_null_results_BalExch)
saveRDS(small_null_results_BalExch, file="/simdata/small_null_results_BalExch.rds")
