## Performance of GEEs vs mixed models for analysing partially clustered data
## Kylie Lange (kylie.lange@adelaide.edu.au)

##### GENERATING DATA #####

# Generate a random starting seed based on system time
generate_seed = function() {
  my_seed = as.integer(Sys.time()) %% 100000
  return(my_seed)
}

# Generate a singel simulated dataset
# Inputs: simulation parameters
# Output: a dataframe
generate_data = function(nobs, ppair, b0, b1, ICC) {
  sd_alpha = sqrt(ICC)
  sd_eps = sqrt(1 - ICC)
  cluster_id = as.factor(1:nobs)
  ctype = rbinom(cluster_id, 1, ppair)
  clusN = ctype + 1
  alpha_j = rnorm(cluster_id, 0, sd_alpha)
  cluster_id = rep(cluster_id, times = clusN)
  alpha_j = rep(alpha_j, times = clusN)
  csize = rep(clusN, times = clusN)
  ind_id = as.factor(1:length(cluster_id))
  eps_ij = rnorm(length(cluster_id), 0, sd_eps)
  selected_ids = data.frame(cluster_id, ind_id) %>%                     # create a tmp dataframe of the ids and select obs from required number of clusters
    filter(as.numeric(cluster_id) <= as.numeric(cluster_id)[nobs])
  selected_obs = length(selected_ids$ind_id)                            # check the resulting number of observations
  if (selected_obs == nobs) {                                           # Only progress to generating the simulated dataset if has required sample size
    dat = data.frame(cluster_id, csize, alpha_j, ind_id, eps_ij) %>%
      filter(as.numeric(cluster_id) <= as.numeric(cluster_id)[nobs]) %>%
      group_by(cluster_id) %>%
      mutate(withinclus_id = row_number(cluster_id)) %>%
      ungroup()
    actualobs = length(dat$ind_id)
    actualclus = length(unique(dat$cluster_id))
    actualclusN = clusN[1:actualclus]  
    tmt_ind = sample(rep(c(0, 1), length = actualobs))
    tmt_clus = rep(sample(rep(c(0,1), length = actualclus)), times = actualclusN)
    dat = dat %>% add_column(tmt_opp = NA)
    dat$tmt_opp[dat$withinclus_id == 1] = sample(rep(c(0,1), length = sum(dat$withinclus_id == 1)))
    dat$tmt_opp[dat$withinclus_id == 2 & lag(dat$tmt_opp) == 0] = 1
    dat$tmt_opp[dat$withinclus_id == 2 & lag(dat$tmt_opp) == 1] = 0
    resp_ind  = b0 + b1*tmt_ind + dat$alpha_j + dat$eps_ij
    resp_clus = b0 + b1*tmt_clus + dat$alpha_j + dat$eps_ij
    resp_opp  = b0 + b1*dat$tmt_opp + dat$alpha_j + dat$eps_ij
    dat = data.frame(dat, tmt_ind, tmt_clus, resp_ind, resp_clus, resp_opp, nobs=nobs, ppair=ppair, ICC=ICC, b1=b1)
    return(dat)
  }
}

# Generate multiple simulated datasets
# Inputs: simulation parameters for a single scenario
# Outputs: a list of datasets created by generate_data() and a matrix of random seeds for each dataset
generate_multiple_datasets = function(sims, nobs, ppair, b0, b1, ICC) {
  nsims = 0
  simulated_data = list()
  states = matrix(ncol=626, nrow=sims)
  while (nsims < sims) {
    tmp_seed = .Random.seed
    simulated_data[[length(simulated_data) + 1]] = generate_data(nobs=nobs, ppair=ppair, b0=b0, b1=b1, ICC=ICC)
    if (!is_empty(simulated_data)) {
      if (is.null(simulated_data[[length(simulated_data)]])) {      # if generate_data() created a dataset of invalid length,
        simulated_data = compact(simulated_data)                    # remove the NULL object and do not keep the random seed
      }
      else {
        states[length(simulated_data), ] = tmp_seed
      }
    }
    nsims = length(simulated_data)
  }
  simulated_data = Map(cbind, simulated_data, index=seq_along(simulated_data))
  list(data=simulated_data, states=states)
}

# Run simulation for a set of multiple scenarios
# Inputs: a list of simulation parameters for multiple scenarios
# Outputs: a list of datasets and random seeds for each scenario
run_simulation = function(scenario, nsims) {
  n  = scenario$sample_sizes
  b1 = scenario$b1
  sim    = list()
  data   = list()
  states = list()
  for (i in 1:length(n)) {
    sim[[i]]    = generate_multiple_datasets(nsims, ICC=scenario$ICC, ppair=scenario$ppair, b0=scenario$b0, nobs=n[i], b1=b1[i])
    data[[i]]   = sim[[i]]$data
    data[[i]]   = Map(cbind, data[[i]], rand_method=scenario$rand_method)
    data[[i]]   = Map(cbind, data[[i]], gee_method=scenario$gee_method)
    states[[i]] = sim[[i]]$states
  }
  list(data=data, states=states)
}

# Run simulation for given scenarios and save the resulting data files
# Inputs: a list of simulation parameters for multiple scenarios and the number of simulated datsets to be created per scenario
# Outputs: a list of datasets; individual datasets and random seeds saved to file
simulate = function(scenarios, nsims){

  # setup and random seed
  rand = scenarios[[1]]$rand_method
  de   = scenarios[[1]]$gee_method
  s = format(generate_seed(), scientific=FALSE)
  set.seed(s)
  saveRDS(s, file=paste0("simdata/seed_", rand, "_",  de, ".rds"))

  # generate simulated datasets
  tm = proc.time()
  result = map(scenarios, run_simulation, nsims=nsims)
  etm = proc.time() - tm
  print(etm)
  
  # save datasets to file and append all datasets for analysis
  data_all = list()
  for (i in 1:length(result)) {
    data   = result[[i]]$data
    states = result[[i]]$states
    name   = scenarios[[i]]$name
    saveRDS(data, file=paste0("simdata/data_", name, ".rds"))
    saveRDS(states, file=paste0("simdata/states_", name, ".rds"))
    data_all = c(data_all, data)
  }
  return(data_all)
}


###### MODEL FITTING ######

# Fit models for partially clustered data to a dataset
# Inputs: a dataset as produced by generate_data()
# Output: a dataframe of simulation parameters and model estimates, one row per analysis (rsimsum compatible)
# Analyses: geepack, lmer
fit_models = function(dat, my_rand, my_gee) {
  # logging
  print(dat[1,c("ICC", "ppair", "nobs", "index")]) 
  print(Sys.time())
  # setup
  if (my_rand == "ind")     {dat = mutate(dat, tmt = tmt_ind,  resp = resp_ind)}
  if (my_rand == "cluster") {dat = mutate(dat, tmt = tmt_clus, resp = resp_clus)}
  if (my_rand == "opp")     {dat = mutate(dat, tmt = tmt_opp,  resp = resp_opp)}
  if (my_gee == "ind")      {gee_method = "independence"}
  if (my_gee == "exch")     {gee_method = "exchangeable"}
  numpairs = (nrow(dat) - n_distinct(dat$cluster_id))
  # sim params
  d = data.frame(dataset = dat$index[1],
                 nobs    = dat$nobs[1],
                 numpairs = numpairs,
                 ppair   = dat$ppair[1],
                 ICC     = dat$ICC[1],
                 trueb1  = dat$b1[1],
                 rand_method = my_rand,
                 gee_method  = my_gee,
                 stringsAsFactors = FALSE, row.names = NULL)

  # gee via geepack::geeglm
  m1 = geeglm(resp ~ tmt, id=cluster_id, family=gaussian, corstr=gee_method, data=dat)
  s1 = summary(m1)
  d1 = data.frame(model = "geeglm",
                  b0    = s1$coefficients["(Intercept)", "Estimate"],
                  se_b0 = s1$coefficients["(Intercept)", "Std.err"],
                  b1    = s1$coefficients["tmt", "Estimate"],
                  se_b1 = s1$coefficients["tmt", "Std.err"],
                  p_b1  = s1$coefficients["tmt", "Pr(>|W|)"],
                  stringsAsFactors = FALSE, row.names = NULL)
  d1 = bind_cols(d, d1)
  if (my_gee == "exch") {
    a1 = data.frame(alpha = s1$corr["alpha", "Estimate"],
                    alpha_se = s1$corr["alpha", "Std.err"],
                    stringsAsFactors = FALSE, row.names = NULL)
    d1 = bind_cols(d1, a1)
  }
  
  # mixed model via lme4::lmer
  if (numpairs > 0) {
    mfix = FALSE
    m2 = lmer(resp ~ tmt + (1 | cluster_id), data=dat)
    s2 = summary(m2)
    d2 = data.frame(model = "mixed",
                    b0    = s2$coefficients["(Intercept)", "Estimate"],
                    se_b0 = s2$coefficients["(Intercept)", "Std. Error"],
                    b1    = s2$coefficients["tmt", "Estimate"],
                    se_b1 = s2$coefficients["tmt", "Std. Error"],
                    sd_alpha = as.data.frame(VarCorr(m2))[1, "sdcor"],
                    sd_resid = as.data.frame(VarCorr(m2))[2, "sdcor"],
                    p_b1 = s2$coefficients["tmt", "Pr(>|t|)"],
                    stringsAsFactors = FALSE, row.names = NULL)
    d2 = bind_cols(d, d2, as.data.frame(check_mixed_converged(m2), col.names=c('mconv', 'mnsng', 'mmsg')), as.data.frame(mfix))
  }
  else if (numpairs == 0) {  # if there are no pairs, fit a fixed effects model
    mfix = TRUE
    m2 = lm(resp ~ tmt, data=dat)
    s2 = summary(m2)
    d2 = data.frame(model = "mixed",
                    b0    = s2$coefficients["(Intercept)", "Estimate"],
                    se_b0 = s2$coefficients["(Intercept)", "Std. Error"],
                    b1    = s2$coefficients["tmt", "Estimate"],
                    se_b1 = s2$coefficients["tmt", "Std. Error"],
                    sd_resid = sd(s2$residuals),
                    p_b1  = s2$coefficients["tmt", "Pr(>|t|)"],
                    stringsAsFactors = FALSE, row.names = NULL)
    d2 = bind_cols(d, d2, as.data.frame(mfix))
  }
  
  # merge into one dataframe with one row per model
  result = bind_rows(d1, d2)
  return(result)
}

# Fit models for partially clustered data to a dataset, using small sample corrections
# Inputs: a dataset as produced by generate_data()
# Output: a dataframe of simulation parameters and model estimates, one row per analysis (rsimsum compatible)
# Analyses: unadjusted geepack, lmer; Kenward-Roger df for mixed model (pbkrtest), Mancl-DeRouen correction for GEE (geesmv)
fit_models_smallsample = function(dat, my_rand, my_gee) {
  # logging
  print(dat[1,c("ICC", "ppair", "nobs", "index")])
  print(Sys.time()) 
  # setup
  if (my_rand == "ind")     {dat = mutate(dat, tmt = tmt_ind,  resp = resp_ind)}
  if (my_rand == "cluster") {dat = mutate(dat, tmt = tmt_clus, resp = resp_clus)}
  if (my_rand == "opp")     {dat = mutate(dat, tmt = tmt_opp,  resp = resp_opp)}
  if (my_gee == "ind")      {gee_method = "independence"}
  if (my_gee == "exch")     {gee_method = "exchangeable"}
  numpairs = (nrow(dat) - n_distinct(dat$cluster_id))
  # sim params
  d = data.frame(dataset = dat$index[1],
                 nobs    = dat$nobs[1],
                 numpairs = numpairs,
                 ppair   = dat$ppair[1],
                 ICC     = dat$ICC[1],
                 trueb1  = dat$b1[1],
                 rand_method = my_rand,
                 gee_method  = my_gee,
                 stringsAsFactors = FALSE, row.names = NULL)
  
  # mixed model via lme4::lmer
  if (numpairs > 0) {
    mfix = FALSE
    m1 = lmer(resp ~ tmt + (1 | cluster_id), data=dat)
    s1 = summary(m1)
    d1 = data.frame(model = "mixed",
                    b0    = s1$coefficients["(Intercept)", "Estimate"],
                    se_b0 = s1$coefficients["(Intercept)", "Std. Error"],
                    b1    = s1$coefficients["tmt", "Estimate"],
                    se_b1 = s1$coefficients["tmt", "Std. Error"],
                    sd_alpha = as.data.frame(s1$varcor)[1, "sdcor"],
                    sd_resid = as.data.frame(s1$varcor)[2, "sdcor"],
                    p_b1 = s1$coefficients["tmt", "Pr(>|t|)"],
                    ci_lower = confint(m1, method="Wald")["tmt", "2.5 %"],
                    ci_upper = confint(m1, method="Wald")["tmt", "97.5 %"],
                    stringsAsFactors = FALSE, row.names = NULL)
    d1 = bind_cols(d, d1, as.data.frame(check_mixed_converged(m1), col.names=c('mconv', 'mnsng', 'mmsg')), as.data.frame(mfix))
  }
  else if (numpairs == 0) {  # if there are no pairs, fit a fixed effects model
    mfix = TRUE
    m1 = lm(resp ~ tmt, data=dat)
    s1 = summary(m1)
    d1 = data.frame(model = "mixed",
                    b0    = s1$coefficients["(Intercept)", "Estimate"],
                    se_b0 = s1$coefficients["(Intercept)", "Std. Error"],
                    b1    = s1$coefficients["tmt", "Estimate"],
                    se_b1 = s1$coefficients["tmt", "Std. Error"],
                    sd_resid = sd(s1$residuals),
                    p_b1  = s1$coefficients["tmt", "Pr(>|t|)"],
                    ci_lower = confint(m1, method="Wald")["tmt", "2.5 %"],
                    ci_upper = confint(m1, method="Wald")["tmt", "97.5 %"],
                    stringsAsFactors = FALSE, row.names = NULL)
    d1 = bind_cols(d, d1, as.data.frame(mfix))
  }
  
  # mixed model with Kenward-Roger corrected se and denom df via pbkrtest package
  if (numpairs > 0) {
    mfix = FALSE
    # to catch vcovAdj errors in small number of cases where mixed model runs but is possibly non-converged
    v1 = tryCatch(
      vcovAdj(m1),
      error = function(e) e$message
    )
    if (class(v1) != "dgeMatrix") {  # if vcovAdj cannot be computed, use the unadjusted model
      d2 = d1 %>% 
        mutate(model = "mixed-kr") %>% 
        mutate(vcoverr = TRUE)
    } else {
      # KR calculations
      kr_se  = sqrt(vcovAdj(m1)['tmt', 'tmt'])
      L      = rep(0, length(fixef(m1)))
      L[which(names(fixef(m1))=='tmt')] = 1
      kr_ddf  = Lb_ddf(L, vcov(m1), vcovAdj(m1))
      kr_t    = qt(0.975, kr_ddf)
      kr_lowerCI = s1$coefficients["tmt", "Estimate"] - kr_t * kr_se
      kr_upperCI = s1$coefficients["tmt", "Estimate"] + kr_t * kr_se    
      d2 = data.frame(model = "mixed-kr",
                      b0    = s1$coefficients["(Intercept)", "Estimate"],
                      se_b0 = s1$coefficients["(Intercept)", "Std. Error"],
                      b1    = s1$coefficients["tmt", "Estimate"],
                      se_b1 = kr_se,
                      sd_alpha = as.data.frame(s1$varcor)[1, "sdcor"],
                      sd_resid = as.data.frame(s1$varcor)[2, "sdcor"],
                      ci_lower = kr_lowerCI,
                      ci_upper = kr_upperCI,
                      adj_ddf  = kr_ddf,
                      vcoverr = FALSE,
                      stringsAsFactors = FALSE, row.names = NULL)
      d2 = bind_cols(d, d2, as.data.frame(check_mixed_converged(m1), col.names=c('mconv', 'mnsng', 'mmsg')), as.data.frame(mfix))
    }
  }
  else if (numpairs == 0) {  # if there are no pairs, use the same fixed effects model as in the unadjusted case. KR adjustment is not applicable.
    d2 = d1 %>% 
      mutate(model = "mixed-kr")
  }
  
  # gee via geepack::geeglm
  m3 = geeglm(resp ~ tmt, id=cluster_id, family=gaussian, corstr=gee_method, data=dat)
  s3 = summary(m3)
  d3 = data.frame(model = "geeglm",
                  b0    = s3$coefficients["(Intercept)", "Estimate"],
                  se_b0 = s3$coefficients["(Intercept)", "Std.err"],
                  b1    = s3$coefficients["tmt", "Estimate"],
                  se_b1 = s3$coefficients["tmt", "Std.err"],
                  ci_lower = s3$coefficients["tmt", "Estimate"] - 1.96*s3$coefficients["tmt", "Std.err"],
                  ci_upper = s3$coefficients["tmt", "Estimate"] + 1.96*s3$coefficients["tmt", "Std.err"],
                  p_b1  = s3$coefficients["tmt", "Pr(>|W|)"],
                  stringsAsFactors = FALSE, row.names = NULL)
  d3 = bind_cols(d, d3)
  if (my_gee == "exch") {
    a3 = data.frame(alpha = s3$corr["alpha", "Estimate"],
                    alpha_se = s3$corr["alpha", "Std.err"],
                    stringsAsFactors = FALSE, row.names = NULL)
    d3 = bind_cols(d3, a3)
  }
  
  # gee with Mancl & DeRouen correction via (modified) geesmv package
  # and N-p df for t statistic in CIs
  m4 = GEE.var.md2(resp ~ tmt, id="cluster_id", family=gaussian, corstr=gee_method, data=dat)
  md_se  = sqrt(m4$cov.beta['tmt'])
  md_ddf = nrow(dat) - 2
  md_t    = qt(0.975, md_ddf)
  md_lowerCI = s3$coefficients["tmt", "Estimate"] - md_t * md_se
  md_upperCI = s3$coefficients["tmt", "Estimate"] + md_t * md_se
  
  d4 = data.frame(model = "gee-md",
                  b0    = s3$coefficients["(Intercept)", "Estimate"],
                  se_b0 = s3$coefficients["(Intercept)", "Std.err"],
                  b1    = s3$coefficients["tmt", "Estimate"],         # point estimate from geeglm
                  se_b1 = md_se,
                  ci_lower = md_lowerCI,                              # CI calculated using b1 from geeglm
                  ci_upper = md_upperCI,
                  adj_ddf  = md_ddf,
                  stringsAsFactors = FALSE, row.names = NULL)
  d4 = bind_cols(d, d4)
  if (my_gee == "exch") {
    a4 = data.frame(alpha = s3$corr["alpha", "Estimate"],
                    alpha_se = s3$corr["alpha", "Std.err"],
                    stringsAsFactors = FALSE, row.names = NULL)
    d4 = bind_cols(d4, a4)
  }

  # merge into one dataframe with one row per model
  result = bind_rows(d1, d2, d3, d4)
  return(result)
}

# Check for convergence of mixed model
# Inputs: a lmer model object
# Outputs: a list of logical indicators for whether the model returned a non-convergence message or boundary/singular fit message, and content of the message
# warnings in the lmer model are captured in the messages attribute
# example warnings:
# "boundary (singular) fit: see help('isSingular')"
# "Model failed to converge"
# "Model is nearly unidentifiable"
check_mixed_converged = function(m) {
  msg = summary(m)$optinfo$conv$lme4$messages
  if (is.null(msg)) {
    conv = TRUE
    nsng = TRUE
    msg  = ""
  }
  else {
    msg = paste(msg, collapse=';;')
    conv = !grepl('failed to converge|unidentifiable|Hessian is numerically singular', msg)
    nsng = !grepl('boundary \\(singular\\) fit', msg)
  }
  list(conv=conv, nsng=nsng, msg=msg)
}


###### PROCESSING RESULTS ######

# Check for convergence of geepack::geeglm model and set results to NA
# Uses the dropbig() function of rsimsum to identify models with large estimates
# Inputs: a dataframe of model results; thresholds to use for the point estimates (max) and SE (semax)
# Outputs: the input dataframe with new .dropbig column
set_geeglm_nonconverged = function(res, mod, max=10, semax=100) {
  # separate out the geeglm results
  res_gee = res %>% 
    filter(model == mod)
  res_other = res %>% 
    filter(model != mod)
  # identify nonconverged results
  res_gee_conv = dropbig(data = res_gee, estvarname = "b1", se = "se_b1", by = c("trueb1", "ppair", "ICC"),
                         max = max, semax = semax)
  # if dataset is from independence GEEs: set nonconverged model results to NA
  if (head(res_gee_conv, 1)$gee_method == "ind") {
    res_gee_conv = res_gee_conv %>% 
      mutate(across(c(b0, se_b0, b1, se_b1, p_b1), ~ifelse((.dropbig==TRUE), NA, .)))
  }
  # if dataset is from exchangeable GEEs: set nonconverged model results to NA and identify non-positive definite results
  if (head(res_gee_conv, 1)$gee_method == "exch") {
    res_gee_conv = res_gee_conv %>% 
      mutate(across(c(b0, se_b0, b1, se_b1, p_b1, alpha, alpha_se), ~ifelse((.dropbig==TRUE), NA, .))) %>% 
      mutate(geeglmNPD = (abs(alpha) > 1))
  }
  # stick back together with the original dataframe
  res = bind_rows(res_other, res_gee_conv) %>% 
    arrange(ICC, ppair, trueb1, dataset, model)
  res
}

# Check for convergence of lmer model and set results to NA
# Inputs: a dataframe of model results
# Outputs: the input dataframe with results of non-converged mixed models set to NA
set_mixed_nonconverged = function(res) {
  # separate out the mixed results
  res_mix = res %>% 
    filter(model == "mixed")
  res_other = res %>% 
    filter(model != "mixed")
  # set nonconverged model results to NA
  res_mix_conv = res_mix %>%
    mutate(mconvall = ifelse(is.na(mconv), TRUE, mconv)) %>% 
    mutate(mnsngall = ifelse(is.na(mnsng), TRUE, mnsng)) %>% 
    mutate(across(c(b0, se_b0, b1, se_b1, p_b1), ~ifelse(mconvall==FALSE, NA, .)))
  # stick back together with the original dataframe
  res = bind_rows(res_other, res_mix_conv) %>% 
    arrange(ICC, ppair, trueb1, dataset, model)
  res
}

# Check for convergence of GEE model with Mancl-DeRouen correction and set results to NA
# Uses the dropbig() function of rsimsum to identify models with large estimates
# Inputs: a dataframe of model results; thresholds to use for the point estimates (max) and SE (semax)
# Outputs: the input dataframe with new .dropbig column
set_geeglm_nonconverged_smallsample = function(res, mod, max=10, semax=100) {
  # separate out the GEE results
  res_gee = res %>% 
    filter(model == mod)
  res_other = res %>% 
    filter(model != mod)
  # identify nonconverged results
  res_gee_conv = dropbig(data = res_gee, estvarname = "b1", se = "se_b1", by = c("trueb1", "ppair", "ICC"),
                         max = max, semax = semax)
  # if dataset is independence GEEs: set nonconverged model results to NA
  if (head(res_gee_conv, 1)$gee_method == "ind") {
    res_gee_conv = res_gee_conv %>% 
      mutate(across(c(b0, se_b0, b1, se_b1, p_b1, ci_lower, ci_upper), ~ifelse((.dropbig==TRUE), NA, .)))
  }
  # if dataset is exchangeable GEEs: set nonconverged model results to NA and identify non-positive definite results
  if (head(res_gee_conv, 1)$gee_method == "exch") {
    res_gee_conv = res_gee_conv %>% 
      mutate(across(c(b0, se_b0, b1, se_b1, p_b1, ci_lower, ci_upper, alpha, alpha_se), ~ifelse((.dropbig==TRUE), NA, .))) %>% 
      mutate(geeglmNPD = (abs(alpha) > 1))
  }
  # stick back together with the original dataframe
  res = bind_rows(res_other, res_gee_conv) %>% 
    arrange(ICC, ppair, trueb1, dataset, model)
  res
}

# Check for convergence of mixed model with Kenward-Roger adjusted dfs and set results to NA
# Inputs: a dataframe of model results as formatted for rsimsum
# Outputs: the input dataframe with mixed results set to NA
set_mixed_nonconverged_smallsample = function(res) {
  # separate out the mixed results
  res_mix = res %>% 
    filter(model == "mixed")
  res_other = res %>% 
    filter(model != "mixed")
  # set nonconverged model results to NA
  res_mix_conv = res_mix %>%
    mutate(mconvall = ifelse(is.na(mconv), TRUE, mconv)) %>% 
    mutate(mnsngall = ifelse(is.na(mnsng), TRUE, mnsng)) %>% 
    mutate(across(c(b0, se_b0, b1, se_b1, p_b1, sd_alpha, sd_resid, ci_lower, ci_upper), ~ifelse(mconvall==FALSE, NA, .)))
  # stick back together with the original dataframe
  res = bind_rows(res_other, res_mix_conv) %>% 
    arrange(ICC, ppair, trueb1, dataset, model)
  res
}
