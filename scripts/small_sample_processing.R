## Performance of GEEs vs mixed models for analysing partially clustered data
## Kylie Lange (kylie.lange@adelaide.edu.au)

# SMALL SAMPLE BIAS CORRECTIONS
# Sub-study to assess performance of analysis methods when small sample corrections are applied

## Process the model analysis results ready for analysis
## Create the simum objects that contain the performance measure outcomes

# thresholds for defining non-convergence (via rsimsum::dropbig())
max = 10
semax = 100

## non-zero effect size scenarios

# set non-converged model results to NA so they are excluded from further analysis
small_results_ClusInd  = set_geeglm_nonconverged_smallsample(small_results_ClusInd,  mod="geeglm", max, semax)
small_results_ClusExch = set_geeglm_nonconverged_smallsample(small_results_ClusExch, mod="geeglm", max, semax)
small_results_IndInd   = set_geeglm_nonconverged_smallsample(small_results_IndInd,   mod="geeglm", max, semax)
small_results_IndExch  = set_geeglm_nonconverged_smallsample(small_results_IndExch,  mod="geeglm", max, semax)
small_results_BalInd   = set_geeglm_nonconverged_smallsample(small_results_BalInd,   mod="geeglm", max, semax)
small_results_BalExch  = set_geeglm_nonconverged_smallsample(small_results_BalExch,  mod="geeglm", max, semax)
small_results_ClusInd  = set_geeglm_nonconverged_smallsample(small_results_ClusInd,  mod="gee-md", max, semax)
small_results_ClusExch = set_geeglm_nonconverged_smallsample(small_results_ClusExch, mod="gee-md", max, semax)
small_results_IndInd   = set_geeglm_nonconverged_smallsample(small_results_IndInd,   mod="gee-md", max, semax)
small_results_IndExch  = set_geeglm_nonconverged_smallsample(small_results_IndExch,  mod="gee-md", max, semax)
small_results_BalInd   = set_geeglm_nonconverged_smallsample(small_results_BalInd,   mod="gee-md", max, semax)
small_results_BalExch  = set_geeglm_nonconverged_smallsample(small_results_BalExch,  mod="gee-md", max, semax)
small_results_ClusInd  = set_mixed_nonconverged(small_results_ClusInd)
small_results_ClusExch = set_mixed_nonconverged(small_results_ClusExch)
small_results_IndInd   = set_mixed_nonconverged(small_results_IndInd)
small_results_IndExch  = set_mixed_nonconverged(small_results_IndExch)
small_results_BalInd   = set_mixed_nonconverged(small_results_BalInd)
small_results_BalExch  = set_mixed_nonconverged(small_results_BalExch)

all_small = bind_rows(small_results_ClusInd, small_results_ClusExch,
                      small_results_IndInd, small_results_IndExch,
                      small_results_BalInd, small_results_BalExch)
all_small = all_small %>% 
  mutate(scenario =
           case_when(rand_method == "cluster" & gee_method == "ind"  ~ "cluster_ind",
                     rand_method == "cluster" & gee_method == "exch" ~ "cluster_exch",
                     rand_method == "ind"     & gee_method == "ind"  ~ "indiv_ind",
                     rand_method == "ind"     & gee_method == "exch" ~ "indiv_exch",
                     rand_method == "opp"     & gee_method == "ind"  ~ "opp_ind",
                     rand_method == "opp"     & gee_method == "exch" ~ "opp_exch"))
all_adj = all_small %>% 
  filter(model == "mixed-kr" | model == "gee-md") %>% 
  mutate(trueb1_v2 = trueb1) %>%
  rename("Probability of a pair" = ppair,
         "Effect size" = trueb1)
small_ClusInd = all_adj %>% 
  filter(scenario == "cluster_ind")
small_ClusExch = all_adj %>% 
  filter(scenario == "cluster_exch")
small_IndInd = all_adj %>% 
  filter(scenario == "indiv_ind")
small_IndExch = all_adj %>% 
  filter(scenario == "indiv_exch")
small_BalInd = all_adj %>% 
  filter(scenario == "opp_ind")
small_BalExch = all_adj %>% 
  filter(scenario == "opp_exch")

# manual calculation of power
# rsimsum does not support rep-specific degrees of freedom for t-distn hypothesis test (in the power_df param), which is needed for K-R adjustment.
# so significance is determined using the adjusted CIs, then calculate power and put results into a dataframe compatible with rsimsum
power_results = all_small %>% 
  mutate(b1_sig = (((ci_lower < 0) & (ci_upper < 0)) | ((ci_lower > 0) & (ci_upper > 0)))) %>% 
  group_by(scenario, model, ICC, ppair, trueb1) %>% 
  summarise(nsim  = sum(!is.na(b1)),
            power = mean(b1_sig, na.rm = TRUE),
            mcse  = sqrt(power*(1-power)/nsim),
            .groups = 'drop') %>% 
  pivot_longer(cols = c('nsim', 'power'),
               names_to = 'stat',
               values_to = 'est') %>% 
  mutate(mcse = ifelse(stat=="nsim", NA, mcse),
         across(c(scenario, model, trueb1, ppair, ICC), as.factor)) %>% 
  select(scenario, stat, est, mcse, model, trueb1, ppair, ICC) %>% 
  mutate(trueb1_v2 = trueb1)
power_results_adj = power_results %>% 
  filter(model == "mixed-kr" | model == "gee-md") %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = trueb1)

## zero effect size scenarios

# set non-converged model results to NA so they are excluded from further analysis
small_null_results_ClusInd  = set_geeglm_nonconverged(small_null_results_ClusInd,  "geeglm", max, semax)
small_null_results_ClusExch = set_geeglm_nonconverged(small_null_results_ClusExch, "geeglm", max, semax)
small_null_results_IndInd   = set_geeglm_nonconverged(small_null_results_IndInd,   "geeglm", max, semax)
small_null_results_IndExch  = set_geeglm_nonconverged(small_null_results_IndExch,  "geeglm", max, semax)
small_null_results_BalInd   = set_geeglm_nonconverged(small_null_results_BalInd,   "geeglm", max, semax)
small_null_results_BalExch  = set_geeglm_nonconverged(small_null_results_BalExch,  "geeglm", max, semax)
small_null_results_ClusInd  = set_geeglm_nonconverged(small_null_results_ClusInd,  "gee-md", max, semax)
small_null_results_ClusExch = set_geeglm_nonconverged(small_null_results_ClusExch, "gee-md", max, semax)
small_null_results_IndInd   = set_geeglm_nonconverged(small_null_results_IndInd,   "gee-md", max, semax)
small_null_results_IndExch  = set_geeglm_nonconverged(small_null_results_IndExch,  "gee-md", max, semax)
small_null_results_BalInd   = set_geeglm_nonconverged(small_null_results_BalInd,   "gee-md", max, semax)
small_null_results_BalExch  = set_geeglm_nonconverged(small_null_results_BalExch,  "gee-md", max, semax)
small_null_results_ClusInd  = set_mixed_nonconverged(small_null_results_ClusInd)
small_null_results_ClusExch = set_mixed_nonconverged(small_null_results_ClusExch)
small_null_results_IndInd   = set_mixed_nonconverged(small_null_results_IndInd)
small_null_results_IndExch  = set_mixed_nonconverged(small_null_results_IndExch)
small_null_results_BalInd   = set_mixed_nonconverged(small_null_results_BalInd)
small_null_results_BalExch  = set_mixed_nonconverged(small_null_results_BalExch)

all_small_null = bind_rows(small_null_results_ClusInd, small_null_results_ClusExch,
                           small_null_results_IndInd, small_null_results_IndExch,
                           small_null_results_BalInd, small_null_results_BalExch)
all_small_null = all_small_null %>%
  mutate(scenario =
           case_when(rand_method == "cluster" & gee_method == "ind"  ~ "cluster_ind",
                     rand_method == "cluster" & gee_method == "exch" ~ "cluster_exch",
                     rand_method == "ind"   & gee_method == "ind"  ~ "indiv_ind",
                     rand_method == "ind"   & gee_method == "exch" ~ "indiv_exch",
                     rand_method == "opp"     & gee_method == "ind"  ~ "opp_ind",
                     rand_method == "opp"     & gee_method == "exch" ~ "opp_exch"))
all_null_adj = all_small_null %>% 
  filter(model == "mixed-kr" | model == "gee-md") %>%
  rename("Probability of a pair" = ppair,
         "Effect size" = n_es)
small_null_ClusInd = all_null_adj %>% 
  filter(scenario == "cluster_ind")
small_null_ClusExch = all_null_adj %>% 
  filter(scenario == "cluster_exch")
small_null_IndInd = all_null_adj %>% 
  filter(scenario == "indiv_ind")
small_null_IndExch = all_null_adj %>% 
  filter(scenario == "indiv_exch")
small_null_BalInd = all_null_adj %>% 
  filter(scenario == "opp_ind")
small_null_BalExch = all_null_adj %>% 
  filter(scenario == "opp_exch")

# manual calculation of type I error
# rsimsum does not support rep-specific degrees of freedom for t-distn hypothesis test (in the power_df param), which is needed for K-R adjustment.
# so significance is determined using the adjusted CIs, then calculate power and put results into a dataframe compatible with rsimsum
typeIerr_results = all_small_null %>% 
  mutate(b1_sig = (((ci_lower < 0) & (ci_upper < 0)) | ((ci_lower > 0) & (ci_upper > 0)))) %>% 
  group_by(scenario, model, ICC, ppair, trueb1, n_es) %>% 
  summarise(nsim  = sum(!is.na(b1)),
            power = mean(b1_sig, na.rm = TRUE),
            mcse  = sqrt(power*(1-power)/nsim),
            .groups = 'drop') %>% 
  pivot_longer(cols = c('nsim', 'power'),
               names_to = 'stat',
               values_to = 'est') %>% 
  mutate(mcse = ifelse(stat=="nsim", NA, mcse),
         across(c(scenario, model, trueb1, n_es, ppair, ICC), as.factor)) %>% 
  select(scenario, stat, est, mcse, model, n_es, trueb1, ppair, ICC)
typeIerr_results_adj = typeIerr_results %>% 
  filter(model == "mixed-kr" | model == "gee-md") %>% 
  rename("Probability of a pair" = ppair,
         "Effect size" = n_es)

## use simsum() to compute the performance measures for each scenario
ss1 = simsum(data = small_ClusInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1",
             ci.limits = c("ci_lower", "ci_upper"), dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ss2 = simsum(data = small_ClusExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1",
             ci.limits = c("ci_lower", "ci_upper"), dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ss3 = simsum(data = small_IndInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1",
             ci.limits = c("ci_lower", "ci_upper"), dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ss4 = simsum(data = small_IndExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1",
             ci.limits = c("ci_lower", "ci_upper"), dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ss5 = simsum(data = small_BalInd, estvarname = "b1", true = "trueb1_v2", se = "se_b1",
             ci.limits = c("ci_lower", "ci_upper"), dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
ss6 = simsum(data = small_BalExch, estvarname = "b1", true = "trueb1_v2", se = "se_b1",
             ci.limits = c("ci_lower", "ci_upper"), dropbig = FALSE,
             methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nss1 = simsum(data = small_null_ClusInd, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nss2 = simsum(data = small_null_ClusExch, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nss3 = simsum(data = small_null_IndInd, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nss4 = simsum(data = small_null_IndExch, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nss5 = simsum(data = small_null_BalInd, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))
nss6 = simsum(data = small_null_BalExch, estvarname = "b1", true = "trueb1", se = "se_b1", dropbig = FALSE,
              methodvar = "model", by=c("Probability of a pair", "ICC", "Effect size"))

# replace simsum power values with the manually created results
power_results1 = power_results_adj %>% filter(scenario == "cluster_ind") %>% select(-c(scenario, trueb1_v2))
sp1 = ss1
sp1$summ = as.data.frame(power_results1)
power_results2 = power_results_adj %>% filter(scenario == "cluster_exch") %>% select(-c(scenario, trueb1_v2))
sp2 = ss2
sp2$summ = as.data.frame(power_results2)
power_results3 = power_results_adj %>% filter(scenario == "indiv_ind") %>% select(-c(scenario, trueb1_v2))
sp3 = ss3
sp3$summ = as.data.frame(power_results3)
power_results4 = power_results_adj %>% filter(scenario == "indiv_exch") %>% select(-c(scenario, trueb1_v2))
sp4 = ss4
sp4$summ = as.data.frame(power_results4)
power_results5 = power_results_adj %>% filter(scenario == "opp_ind") %>% select(-c(scenario, trueb1_v2))
sp5 = ss5
sp5$summ = as.data.frame(power_results5)
power_results6 = power_results_adj %>% filter(scenario == "opp_exch") %>% select(-c(scenario, trueb1_v2))
sp6 = ss6
sp6$summ = as.data.frame(power_results6)
typeIerr_results1 = typeIerr_results_adj %>% filter(scenario == "cluster_ind") %>% select(-c(scenario, trueb1))
st1 = nss1
st1$summ = as.data.frame(typeIerr_results1)
typeIerr_results2 = typeIerr_results_adj %>% filter(scenario == "cluster_exch") %>% select(-c(scenario, trueb1))
st2 = nss2
st2$summ = as.data.frame(typeIerr_results2)
typeIerr_results3 = typeIerr_results_adj %>% filter(scenario == "indiv_ind") %>% select(-c(scenario, trueb1))
st3 = nss3
st3$summ = as.data.frame(typeIerr_results3)
typeIerr_results4 = typeIerr_results_adj %>% filter(scenario == "indiv_exch") %>% select(-c(scenario, trueb1))
st4 = nss4
st4$summ = as.data.frame(typeIerr_results4)
typeIerr_results5 = typeIerr_results_adj %>% filter(scenario == "opp_ind") %>% select(-c(scenario, trueb1))
st5 = nss5
st5$summ = as.data.frame(typeIerr_results5)
typeIerr_results6 = typeIerr_results_adj %>% filter(scenario == "opp_exch") %>% select(-c(scenario, trueb1))
st6 = nss6
st6$summ = as.data.frame(typeIerr_results6)
