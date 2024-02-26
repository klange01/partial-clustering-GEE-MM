## Performance of GEEs vs mixed models for analysing partially clustered data
## Kylie Lange (kylie.lange@adelaide.edu.au)

## Process the model analysis results ready for analysis
## Create the simum objects that contain the performance measure outcomes

# thresholds for defining non-convergence of GEEs (via rsimsum::dropbig())
max = 10
semax = 100

## non-zero effect size scenarios

# set non-converged model results to NA so they are excluded from further analysis
model_results_ClusInd  = set_geeglm_nonconverged(model_results_ClusInd,  mod="geeglm", max, semax)
model_results_ClusExch = set_geeglm_nonconverged(model_results_ClusExch, mod="geeglm", max, semax)
model_results_IndInd   = set_geeglm_nonconverged(model_results_IndInd,   mod="geeglm", max, semax)
model_results_IndExch  = set_geeglm_nonconverged(model_results_IndExch,  mod="geeglm", max, semax)
model_results_BalInd   = set_geeglm_nonconverged(model_results_BalInd,   mod="geeglm", max, semax)
model_results_BalExch  = set_geeglm_nonconverged(model_results_BalExch,  mod="geeglm", max, semax)
model_results_ClusInd  = set_mixed_nonconverged(model_results_ClusInd)
model_results_ClusExch = set_mixed_nonconverged(model_results_ClusExch)
model_results_IndInd   = set_mixed_nonconverged(model_results_IndInd)
model_results_IndExch  = set_mixed_nonconverged(model_results_IndExch)
model_results_BalInd   = set_mixed_nonconverged(model_results_BalInd)
model_results_BalExch  = set_mixed_nonconverged(model_results_BalExch)

all_results = bind_rows(model_results_ClusInd, model_results_ClusExch,
                        model_results_IndInd, model_results_IndExch,
                        model_results_BalInd, model_results_BalExch)
all_results = all_results %>% 
  mutate(scenario =
           case_when(rand_method == "cluster" & gee_method == "ind"  ~ "cluster_ind",
                     rand_method == "cluster" & gee_method == "exch" ~ "cluster_exch",
                     rand_method == "ind"     & gee_method == "ind"  ~ "indiv_ind",
                     rand_method == "ind"     & gee_method == "exch" ~ "indiv_exch",
                     rand_method == "opp"     & gee_method == "ind"  ~ "opp_ind",
                     rand_method == "opp"     & gee_method == "exch" ~ "opp_exch"))

# data for manuscript tables
tables_results = all_results

# data for rsimsum and nested loop plots
all_GeeMix = all_results %>%  
  mutate(trueb1_v2 = trueb1) %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = trueb1)
GeeMix_ClusInd = all_GeeMix %>% 
  filter(scenario == "cluster_ind")
GeeMix_ClusExch = all_GeeMix %>% 
  filter(scenario == "cluster_exch")
GeeMix_IndInd = all_GeeMix %>% 
  filter(scenario == "indiv_ind")
GeeMix_IndExch = all_GeeMix %>% 
  filter(scenario == "indiv_exch")
GeeMix_BalInd = all_GeeMix %>% 
  filter(scenario == "opp_ind")
GeeMix_BalExch = all_GeeMix %>% 
  filter(scenario == "opp_exch")

## zero effect size scenarios

# set non-converged model results to NA so they are excluded from further analysis
null_model_results_ClusInd  = set_geeglm_nonconverged(null_model_results_ClusInd,  mod="geeglm", max, semax)
null_model_results_ClusExch = set_geeglm_nonconverged(null_model_results_ClusExch, mod="geeglm", max, semax)
null_model_results_IndInd   = set_geeglm_nonconverged(null_model_results_IndInd,   mod="geeglm", max, semax)
null_model_results_IndExch  = set_geeglm_nonconverged(null_model_results_IndExch,  mod="geeglm", max, semax)
null_model_results_BalInd   = set_geeglm_nonconverged(null_model_results_BalInd,   mod="geeglm", max, semax)
null_model_results_BalExch  = set_geeglm_nonconverged(null_model_results_BalExch,  mod="geeglm", max, semax)
null_model_results_ClusInd  = set_mixed_nonconverged(null_model_results_ClusInd)
null_model_results_ClusExch = set_mixed_nonconverged(null_model_results_ClusExch)
null_model_results_IndInd   = set_mixed_nonconverged(null_model_results_IndInd)
null_model_results_IndExch  = set_mixed_nonconverged(null_model_results_IndExch)
null_model_results_BalInd   = set_mixed_nonconverged(null_model_results_BalInd)
null_model_results_BalExch  = set_mixed_nonconverged(null_model_results_BalExch)

all_null_results = bind_rows(null_model_results_ClusInd, null_model_results_ClusExch,
                             null_model_results_IndInd, null_model_results_IndExch,
                             null_model_results_BalInd, null_model_results_BalExch)
all_null_results = all_null_results %>% 
  mutate(scenario =
           case_when(rand_method == "cluster" & gee_method == "ind"  ~ "cluster_ind",
                     rand_method == "cluster" & gee_method == "exch" ~ "cluster_exch",
                     rand_method == "ind"     & gee_method == "ind"  ~ "indiv_ind",
                     rand_method == "ind"     & gee_method == "exch" ~ "indiv_exch",
                     rand_method == "opp"     & gee_method == "ind"  ~ "opp_ind",
                     rand_method == "opp"     & gee_method == "exch" ~ "opp_exch"))

# data for rsimsum and nested loop plots
all_null_GeeMix = all_null_results %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = n_es)
null_GeeMix_ClusInd = all_null_GeeMix %>% 
  filter(scenario == "cluster_ind")
null_GeeMix_ClusExch = all_null_GeeMix %>% 
  filter(scenario == "cluster_exch")
null_GeeMix_IndInd = all_null_GeeMix %>% 
  filter(scenario == "indiv_ind")
null_GeeMix_IndExch = all_null_GeeMix %>% 
  filter(scenario == "indiv_exch")
null_GeeMix_BalInd = all_null_GeeMix %>% 
  filter(scenario == "opp_ind")
null_GeeMix_BalExch = all_null_GeeMix %>% 
  filter(scenario == "opp_exch")

## use simsum() to compute the performance measures for each scenario

s1 = simsum(data = GeeMix_ClusInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = FALSE,
            methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
s2 = simsum(data = GeeMix_ClusExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = FALSE,
            methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
s3 = simsum(data = GeeMix_IndInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = FALSE,
            methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
s4 = simsum(data = GeeMix_IndExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = FALSE,
            methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
s5 = simsum(data = GeeMix_BalInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = FALSE,
            methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
s6 = simsum(data = GeeMix_BalExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = FALSE,
            methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ns1 = simsum(data = null_GeeMix_ClusInd, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ns2 = simsum(data = null_GeeMix_ClusExch, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ns3 = simsum(data = null_GeeMix_IndInd, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ns4 = simsum(data = null_GeeMix_IndExch, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ns5 = simsum(data = null_GeeMix_BalInd, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ns6 = simsum(data = null_GeeMix_BalExch, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
