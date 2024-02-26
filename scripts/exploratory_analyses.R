## Performance of GEEs vs mixed models for analysing partially clustered data
## Kylie Lange (kylie.lange@adelaide.edu.au)

###################################
#####  REALISTIC SCENARIOS   ######
###################################
# Data for the subset of scenarios where partially clustered trials are most likely to be utilised

# non-null scenarios

# set 1: ES=0.2 for individual and balanced randomisation
real1_results = all_results %>%
  filter((trueb1 == 0.2) & (rand_method == "ind" | rand_method == "opp"))
real1_GeeMix = real1_results %>% 
  mutate(trueb1_v2 = trueb1) %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = trueb1)
real1_GeeMix_IndInd = real1_GeeMix %>% 
  filter(scenario == "indiv_ind")
real1_GeeMix_IndExch = real1_GeeMix %>% 
  filter(scenario == "indiv_exch")
real1_GeeMix_BalInd = real1_GeeMix %>% 
  filter(scenario == "opp_ind")
real1_GeeMix_BalExch = real1_GeeMix %>% 
  filter(scenario == "opp_exch")

# set 2: ES=0.5 excluding ppair=0.015, for individual and balanced randomisation
real2_results = all_results %>%
  filter(trueb1 == 0.5 & ppair != 0.015 & (rand_method == "ind" | rand_method == "opp"))
real2_GeeMix = real2_results %>% 
  mutate(trueb1_v2 = trueb1) %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = trueb1)
real2_GeeMix_IndInd = real2_GeeMix %>% 
  filter(scenario == "indiv_ind")
real2_GeeMix_IndExch = real2_GeeMix %>% 
  filter(scenario == "indiv_exch")
real2_GeeMix_BalInd = real2_GeeMix %>% 
  filter(scenario == "opp_ind")
real2_GeeMix_BalExch = real2_GeeMix %>% 
  filter(scenario == "opp_exch")

# null scenarios

# set 1: ES=0.2 for individual and balanced randomisation
real1_null_results = all_null_results %>%
  filter((n_es == 0.2) & (rand_method == "ind" | rand_method == "opp"))
real1_null_GeeMix = real1_null_results %>% 
  mutate(trueb1_v2 = trueb1) %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = n_es)
real1_null_GeeMix_IndInd = real1_null_GeeMix %>% 
  filter(scenario == "indiv_ind")
real1_null_GeeMix_IndExch = real1_null_GeeMix %>% 
  filter(scenario == "indiv_exch")
real1_null_GeeMix_BalInd = real1_null_GeeMix %>% 
  filter(scenario == "opp_ind")
real1_null_GeeMix_BalExch = real1_null_GeeMix %>% 
  filter(scenario == "opp_exch")

# set 2: ES=0.5 excluding ppair=0.015, for individual and balanced randomisation
real2_null_results = all_null_results %>%
  filter(n_es == 0.5 & ppair != 0.015 & (rand_method == "ind" | rand_method == "opp"))
real2_null_GeeMix = real2_null_results %>% 
  mutate(trueb1_v2 = trueb1) %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = n_es)
real2_null_GeeMix_IndInd = real2_null_GeeMix %>% 
  filter(scenario == "indiv_ind")
real2_null_GeeMix_IndExch = real2_null_GeeMix %>% 
  filter(scenario == "indiv_exch")
real2_null_GeeMix_BalInd = real2_null_GeeMix %>% 
  filter(scenario == "opp_ind")
real2_null_GeeMix_BalExch = real2_null_GeeMix %>% 
  filter(scenario == "opp_exch")

# use simsum() to compute the performance measures for each scenario
# set 1
rs31 = simsum(data = real1_GeeMix_IndInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
rs41 = simsum(data = real1_GeeMix_IndExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
rs51 = simsum(data = real1_GeeMix_BalInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
rs61 = simsum(data = real1_GeeMix_BalExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nrs31 = simsum(data = real1_null_GeeMix_IndInd, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = TRUE,
               methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nrs41 = simsum(data = real1_null_GeeMix_IndExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
               methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nrs51 = simsum(data = real1_null_GeeMix_BalInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
               methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nrs61 = simsum(data = real1_null_GeeMix_BalExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
               methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
# set 2
rs32 = simsum(data = real2_GeeMix_IndInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
rs42 = simsum(data = real2_GeeMix_IndExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
rs52 = simsum(data = real2_GeeMix_BalInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
rs62 = simsum(data = real2_GeeMix_BalExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nrs32 = simsum(data = real2_null_GeeMix_IndInd, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = TRUE,
               methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nrs42 = simsum(data = real2_null_GeeMix_IndExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
               methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nrs52 = simsum(data = real2_null_GeeMix_BalInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
               methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nrs62 = simsum(data = real2_null_GeeMix_BalExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = TRUE,
               methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))


###################################
#####  GEE NON-CONVERGENCE   ######
###################################
# lowering the threshold for non-convergence to assess the impact on GEE results

# thresholds for defining non-convergence via rsimsum::dropbig()
max = 5
semax = 50

conv_results_ClusInd  = set_geeglm_nonconverged(model_results_ClusInd,  mod="geeglm", max, semax)
conv_results_ClusExch = set_geeglm_nonconverged(model_results_ClusExch, mod="geeglm", max, semax)
conv_results_IndInd   = set_geeglm_nonconverged(model_results_IndInd,   mod="geeglm", max, semax)
conv_results_IndExch  = set_geeglm_nonconverged(model_results_IndExch,  mod="geeglm", max, semax)
conv_results_BalInd   = set_geeglm_nonconverged(model_results_BalInd,   mod="geeglm", max, semax)
conv_results_BalExch  = set_geeglm_nonconverged(model_results_BalExch,  mod="geeglm", max, semax)
conv_results_all = bind_rows(conv_results_ClusInd, conv_results_ClusExch,
                             conv_results_IndInd, conv_results_IndExch,
                             conv_results_BalInd, conv_results_BalExch)
conv_results_all = conv_results_all %>% filter(model == "geeglm") %>% 
  mutate(trueb1_v2 = trueb1) %>% 
  mutate(scenario =
           case_when(rand_method == "cluster" & gee_method == "ind"  ~ "cluster_ind",
                     rand_method == "cluster" & gee_method == "exch" ~ "cluster_exch",
                     rand_method == "ind"     & gee_method == "ind"  ~ "indiv_ind",
                     rand_method == "ind"     & gee_method == "exch" ~ "indiv_exch",
                     rand_method == "opp"     & gee_method == "ind"  ~ "opp_ind",
                     rand_method == "opp"     & gee_method == "exch" ~ "opp_exch"))
# convergence
conv_summary = conv_results_all %>% group_by(scenario, ICC, ppair, trueb1) %>% 
  summarise(pc_nonconv = mean(.dropbig) * 100,
            num_conv   = sum(!.dropbig))
# performance measures
conv_simsum = simsum(data = conv_results_all, estvarname = "b1", true = "trueb1_v2", se = "se_b1",
                     dropbig = FALSE, methodvar = "scenario", by=c("trueb1", "ppair", "ICC"))
conv_perf = tidy(conv_simsum)

############################################
#####  NON-POSITIVE DEFINITE MODELS   ######
############################################
# Exclude the results of non-positive definite exchangeable GEE results
# set npd results to NA

max = 10
semax = 100

# non-null scenarios
pd_results_ClusExch = set_geeglm_nonconverged(model_results_ClusExch, mod="geeglm", max, semax)
pd_results_IndExch  = set_geeglm_nonconverged(model_results_IndExch,  mod="geeglm", max, semax)
pd_results_BalExch  = set_geeglm_nonconverged(model_results_BalExch,  mod="geeglm", max, semax)
pd_results_ClusExch = pd_results_ClusExch %>% 
  mutate(geeglmNPD = case_when(model=="geeglm" ~ (abs(alpha) > 1)))
pd_results_IndExch = pd_results_IndExch %>% 
  mutate(geeglmNPD = case_when(model=="geeglm" ~ (abs(alpha) > 1)))
pd_results_BalExch = pd_results_BalExch %>% 
  mutate(geeglmNPD = case_when(model=="geeglm" ~ (abs(alpha) > 1)))
pd_exchDE = bind_rows(pd_results_ClusExch, pd_results_IndExch, pd_results_BalExch) %>% 
  filter(model == "geeglm") %>% 
  mutate(across(c(b0, se_b0, b1, se_b1, p_b1, alpha, alpha_se), ~ifelse((geeglmNPD==TRUE), NA, .))) %>% 
  mutate(trueb1_v2 = trueb1) %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = trueb1)

# null scenarios
pd_null_results_ClusExch = set_geeglm_nonconverged(null_model_results_ClusExch, mod="geeglm", max, semax)
pd_null_results_IndExch  = set_geeglm_nonconverged(null_model_results_IndExch,  mod="geeglm", max, semax)
pd_null_results_BalExch  = set_geeglm_nonconverged(null_model_results_BalExch,  mod="geeglm", max, semax)
pd_null_results_ClusExch = pd_null_results_ClusExch %>% 
  mutate(geeglmNPD = case_when(model=="geeglm" ~ (abs(alpha) > 1)))
pd_null_results_IndExch = pd_null_results_IndExch %>% 
  mutate(geeglmNPD = case_when(model=="geeglm" ~ (abs(alpha) > 1)))
pd_null_results_BalExch = pd_null_results_BalExch %>% 
  mutate(geeglmNPD = case_when(model=="geeglm" ~ (abs(alpha) > 1)))
pd_null_exchDE = bind_rows(pd_null_results_ClusExch, pd_null_results_IndExch, pd_null_results_BalExch) %>%
  filter(model == "geeglm") %>% 
  mutate(across(c(b0, se_b0, b1, se_b1, p_b1, alpha, alpha_se), ~ifelse((geeglmNPD==TRUE), NA, .))) %>% 
  mutate(trueb1_v2 = trueb1) %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = n_es)

# use simsum() to compute the performance measures for each scenario
pde = simsum(data = pd_exchDE, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = FALSE,
             methodvar = "rand_method", by=c("Probability of a pair", "ICC", "Effect size"))
npde = simsum(data = pd_null_exchDE, estvarname = "b1", true = "trueb1_v2", se = "se_b1", dropbig = FALSE,
              methodvar = "rand_method", by=c("Probability of a pair", "ICC", "Effect size"))
