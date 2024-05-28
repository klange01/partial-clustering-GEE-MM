## Performance of GEEs vs mixed models for analysing partially clustered data
## Kylie Lange (kylie.lange@adelaide.edu.au)

## Manuscript tables

library(tidyverse)
library(officer)
library(flextable)

## GEE non-positive definite rates (Supp Table 4)
# includes all converged exchangeable GEEs

npd = tables_results %>% 
  filter(model == "geeglm" & gee_method == "exch" & .dropbig == FALSE) %>% 
  group_by(rand_method, trueb1, ICC, ppair) %>%
  summarise(pc_npd = mean(geeglmNPD) * 100) %>% 
  spread(rand_method, pc_npd)

suppT4 = flextable(data = npd) %>%
  set_caption("Supplementary Table 4: Percentage of simulated datasets where the GEE with exchangeable working correlation structure produced non-positive definite results") %>% 
  set_header_labels(values = list(trueb1 = "ES", ppair = "P(pair)", cluster = "Cluster", ind = "Individual", opp = "Balanced")) %>% 
  add_header_row(colwidths = c(3,3), values = c("", "Randomisation method")) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 3) %>% 
  hline(i = seq(4, 36, by=4)) %>% 
  colformat_double(j = c("cluster", "ind", "opp"), digits=1) %>% 
  compose(j="cluster", i = ~(cluster < 0.1 & cluster > 0), value = as_paragraph("<0.1")) %>% 
  compose(j="ind", i = ~(ind < 0.1 & ind > 0), value = as_paragraph("<0.1")) %>%
  compose(j="opp", i = ~(opp < 0.1 & opp > 0), value = as_paragraph("<0.1")) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("Non-positive results identified by an estimated correlation coefficient <-1 or >1. The reported results exclude datasets where the model did not converge.") %>% 
  add_footer_lines("ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Mixed model non-convergence rates (Supp Table 5)
# includes all mixed models

mconv = tables_results %>% 
  filter(model == "mixed") %>% 
  group_by(rand_method, gee_method, trueb1, ICC, ppair) %>%
  summarise(pc_nonconv = (1 - mean(mconvall)) * 100) %>% 
  pivot_wider(id_cols=c(trueb1, ICC, ppair), names_from = c(rand_method, gee_method), values_from = pc_nonconv) %>% 
  relocate(cluster_exch, .after=cluster_ind) %>%
  relocate(ind_exch, .after=ind_ind) %>%
  relocate(opp_exch, .after=opp_ind)

suppT5 = flextable(data = mconv) %>%
  set_caption("Supplementary Table 5: Percentage of simulated datasets where the mixed effects model failed to converge") %>%
  add_header_row(colwidths = c(3, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(3, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(trueb1 = "ES", ppair = "P(pair)", cluster_ind = "GEE-ind DE", cluster_exch = "GEE-exch DE",
                                  ind_ind = "GEE-ind DE", ind_exch = "GEE-exch DE", opp_ind = "GEE-ind DE", opp_exch = "GEE-exch DE")) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 3) %>% 
  hline(i = seq(4, 36, by=4)) %>% 
  colformat_double(j = c("cluster_ind", "cluster_exch", "ind_ind", "ind_exch", "opp_ind", "opp_exch"), digits=1) %>% 
  compose(j="cluster_ind", i = ~(cluster_ind < 0.1 & cluster_ind > 0), value = as_paragraph("<0.1")) %>% 
  compose(j="cluster_exch", i = ~(cluster_exch < 0.1 & cluster_exch > 0), value = as_paragraph("<0.1")) %>%
  compose(j="ind_ind", i = ~(ind_ind < 0.1 & ind_ind > 0), value = as_paragraph("<0.1")) %>%
  compose(j="ind_exch", i = ~(ind_exch < 0.1 & ind_exch > 0), value = as_paragraph("<0.1")) %>%
  compose(j="opp_ind", i = ~(opp_ind < 0.1 & opp_ind > 0), value = as_paragraph("<0.1")) %>%
  compose(j="opp_exch", i = ~(opp_exch < 0.1 & opp_exch > 0), value = as_paragraph("<0.1")) %>% 
  line_spacing(space=0.5) %>%
  add_footer_lines("Non-convergence identified by warning messages printed out by ‘lmer’. The reported results include all simulated datasets, including those with no pairs where non-convergence was not possible due to the use of linear regression") %>% 
  add_footer_lines("GEE-ind = generalised estimating equation with independence working correlation structure, GEE-exch = generalised estimating equation with exchangeable working correlation structure, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Mixed model singular rates (Supp Table 6)
# includes all converged mixed models

sing = tables_results %>% 
  filter(model == "mixed" & mconvall == TRUE) %>% 
  group_by(rand_method, gee_method, trueb1, ICC, ppair) %>%
  summarise(pc_singular = (1 - mean(mnsngall)) * 100) %>% 
  pivot_wider(id_cols=c(trueb1, ICC, ppair), names_from = c(rand_method, gee_method), values_from = pc_singular) %>% 
  relocate(cluster_exch, .after=cluster_ind) %>%
  relocate(ind_exch, .after=ind_ind) %>%
  relocate(opp_exch, .after=opp_ind)

suppT6 = flextable(data = sing) %>%
  set_caption("Supplementary Table 6: Percentage of simulated datasets where the mixed effects model produced singular results") %>%
  add_header_row(colwidths = c(3, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(3, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(trueb1 = "ES", ppair = "P(pair)", cluster_ind = "GEE-ind DE", cluster_exch = "GEE-exch DE",
                                  ind_ind = "GEE-ind DE", ind_exch = "GEE-exch DE", opp_ind = "GEE-ind DE", opp_exch = "GEE-exch DE")) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 3) %>% 
  hline(i = seq(4, 36, by=4)) %>% 
  colformat_double(j = c("cluster_ind", "cluster_exch", "ind_ind", "ind_exch", "opp_ind", "opp_exch"), digits=1) %>% 
  compose(j="cluster_ind", i = ~(cluster_ind < 0.1 & cluster_ind > 0), value = as_paragraph("<0.1")) %>% 
  compose(j="cluster_exch", i = ~(cluster_exch < 0.1 & cluster_exch > 0), value = as_paragraph("<0.1")) %>%
  compose(j="ind_ind", i = ~(ind_ind < 0.1 & ind_ind > 0), value = as_paragraph("<0.1")) %>%
  compose(j="ind_exch", i = ~(ind_exch < 0.1 & ind_exch > 0), value = as_paragraph("<0.1")) %>%
  compose(j="opp_ind", i = ~(opp_ind < 0.1 & opp_ind > 0), value = as_paragraph("<0.1")) %>%
  compose(j="opp_exch", i = ~(opp_exch < 0.1 & opp_exch > 0), value = as_paragraph("<0.1")) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("Singular results identified by an estimated variance of 0 for the random cluster effect. The reported results exclude datasets where the model did not converge and include datasets with no pairs where singularity was not possible due to the use of linear regression") %>% 
  add_footer_lines("GEE-ind = generalised estimating equation with independence working correlation structure, GEE-exch = generalised estimating equation with exchangeable working correlation structure, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Power (Supp Table 7)
# data shown in Figure 1

power1 = pd1 %>%
  mutate(rand_method = "cluster",
         gee_method  = "ind")
power2 = pd2 %>%
  mutate(rand_method = "cluster",
         gee_method  = "exch")
power3 = pd3 %>%
  mutate(rand_method = "ind",
         gee_method  = "ind")
power4 = pd4 %>%
  mutate(rand_method = "ind",
         gee_method  = "exch")
power5 = pd5 %>%
  mutate(rand_method = "opp",
         gee_method  = "ind")
power6 = pd6 %>%
  mutate(rand_method = "opp",
         gee_method  = "exch")
power_ind = bind_rows(power1, power3, power5) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Independence")
power_exch = bind_rows(power2, power4, power6) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Exchangeable")
power_data = bind_rows(power_ind, power_exch) %>% 
  relocate(de, .before="Effect size")

suppT7 = flextable(data = power_data) %>%
  set_caption("Supplementary Table 7: Power of GEEs and mixed models by randomisation methods and GEE working correlation structure") %>% 
  add_header_row(colwidths = c(4, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(4, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(de = "Design effect", "Effect size" = "ES", "Probability of a pair" = "P(pair)", cluster_geeglm = "GEE", cluster_mixed = "Mixed model",
                                  ind_geeglm = "GEE", ind_mixed = "Mixed model", opp_geeglm = "GEE", opp_mixed = "Mixed model")) %>% 
  merge_v(j = ~ de) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 4) %>% 
  hline(i = seq(4, 72, by=4)) %>% 
  colformat_double(j = c("cluster_geeglm", "cluster_mixed", "ind_geeglm", "ind_mixed", "opp_geeglm", "opp_mixed"), digits=3) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("GEE = generalised estimating equation, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  add_footer_lines("The reported results for each method of analysis (GEE independence, GEE exchangeable and mixed effects model) exclude datasets where the analysis model did not converge.") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Coverage (Supp Table 8)
# data shown in Figure 2

cover1 = cd1 %>%
  mutate(rand_method = "cluster",
         gee_method  = "ind")
cover2 = cd2 %>%
  mutate(rand_method = "cluster",
         gee_method  = "exch")
cover3 = cd3 %>%
  mutate(rand_method = "ind",
         gee_method  = "ind")
cover4 = cd4 %>%
  mutate(rand_method = "ind",
         gee_method  = "exch")
cover5 = cd5 %>%
  mutate(rand_method = "opp",
         gee_method  = "ind")
cover6 = cd6 %>%
  mutate(rand_method = "opp",
         gee_method  = "exch")
cover_ind = bind_rows(cover1, cover3, cover5) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Independence")
cover_exch = bind_rows(cover2, cover4, cover6) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Exchangeable")
cover_data = bind_rows(cover_ind, cover_exch) %>% 
  relocate(de, .before="Effect size")

suppT8 = flextable(data = cover_data) %>%
  set_caption("Supplementary Table 8: Coverage of the 95% confidence interval from GEEs and mixed models by randomisation methods and GEE working correlation structure") %>% 
  add_header_row(colwidths = c(4, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(4, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(de = "Design effect", "Effect size" = "ES", "Probability of a pair" = "P(pair)", cluster_geeglm = "GEE", cluster_mixed = "Mixed model",
                                  ind_geeglm = "GEE", ind_mixed = "Mixed model", opp_geeglm = "GEE", opp_mixed = "Mixed model")) %>% 
  merge_v(j = ~ de) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 4) %>% 
  hline(i = seq(4, 72, by=4)) %>% 
  colformat_double(j = c("cluster_geeglm", "cluster_mixed", "ind_geeglm", "ind_mixed", "opp_geeglm", "opp_mixed"), digits=3) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("GEE = generalised estimating equation, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  add_footer_lines("The reported results for each method of analysis (GEE independence, GEE exchangeable and mixed effects model) exclude datasets where the analysis model did not converge.") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Coverage in small samples (Supp Table 9)
# data shown in Figure 3

smcover1 = scd1 %>%
  mutate(rand_method = "cluster",
         gee_method  = "ind")
smcover2 = scd2 %>%
  mutate(rand_method = "cluster",
         gee_method  = "exch")
smcover3 = scd3 %>%
  mutate(rand_method = "ind",
         gee_method  = "ind")
smcover4 = scd4 %>%
  mutate(rand_method = "ind",
         gee_method  = "exch")
smcover5 = scd5 %>%
  mutate(rand_method = "opp",
         gee_method  = "ind")
smcover6 = scd6 %>%
  mutate(rand_method = "opp",
         gee_method  = "exch")
smcover_ind = bind_rows(smcover1, smcover3, smcover5) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Independence")
smcover_exch = bind_rows(smcover2, smcover4, smcover6) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Exchangeable")
smcover_data = bind_rows(smcover_ind, smcover_exch) %>% 
  relocate(de, .before="Effect size")

suppT9 = flextable(data = smcover_data) %>%
  set_caption("Supplementary Table 9: Coverage of the 95% confidence interval from GEEs and mixed models with small sample corrections by randomisation methods and GEE working correlation structure for a subset of scenarios with small sample sizes") %>% 
  add_header_row(colwidths = c(4, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(4, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(de = "Design effect", "Effect size" = "ES", "Probability of a pair" = "P(pair)",
                                  "cluster_gee-md" = "GEE: MD", "cluster_mixed-kr" = "Mixed model: KR", "ind_gee-md" = "GEE: MD",
                                  "ind_mixed-kr" = "Mixed model: KR", "opp_gee-md" = "GEE: MD", "opp_mixed-kr" = "Mixed model: KR")) %>% 
  merge_v(j = ~ de) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 4) %>% 
  hline(i = seq(3, 18, by=3)) %>% 
  colformat_double(j = c("cluster_gee-md", "cluster_mixed-kr", "ind_gee-md", "ind_mixed-kr", "opp_gee-md", "opp_mixed-kr"), digits=3) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("GEE = generalised estimating equation, MD = Mancl-DeRouen correction, KR = Kenward-Roger correction, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  add_footer_lines("The reported results for each method of analysis (GEE independence, GEE exchangeable and mixed effects model) exclude datasets where the analysis model did not converge.") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Relative error (Supp Table 10)
# data shown in Supp Figure 1

relerror1 = rd1 %>%
  mutate(rand_method = "cluster",
         gee_method  = "ind")
relerror2 = rd2 %>%
  mutate(rand_method = "cluster",
         gee_method  = "exch")
relerror3 = rd3 %>%
  mutate(rand_method = "ind",
         gee_method  = "ind")
relerror4 = rd4 %>%
  mutate(rand_method = "ind",
         gee_method  = "exch")
relerror5 = rd5 %>%
  mutate(rand_method = "opp",
         gee_method  = "ind")
relerror6 = rd6 %>%
  mutate(rand_method = "opp",
         gee_method  = "exch")
relerror_ind = bind_rows(relerror1, relerror3, relerror5) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Independence")
relerror_exch = bind_rows(relerror2, relerror4, relerror6) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Exchangeable")
relerror_data = bind_rows(relerror_ind, relerror_exch) %>% 
  relocate(de, .before="Effect size")

suppT10 = flextable(data = relerror_data) %>%
  set_caption("Supplementary Table 10: Relative percent error in the average estimated SE for GEEs and mixed models by randomisation methods and GEE working correlation structure") %>% 
  add_header_row(colwidths = c(4, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(4, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(de = "Design effect", "Effect size" = "ES", "Probability of a pair" = "P(pair)", cluster_geeglm = "GEE", cluster_mixed = "Mixed model",
                                  ind_geeglm = "GEE", ind_mixed = "Mixed model", opp_geeglm = "GEE", opp_mixed = "Mixed model")) %>% 
  merge_v(j = ~ de) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 4) %>% 
  hline(i = seq(4, 72, by=4)) %>% 
  colformat_double(j = c("cluster_geeglm", "cluster_mixed", "ind_geeglm", "ind_mixed", "opp_geeglm", "opp_mixed"), digits=2) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("GEE = generalised estimating equation, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  add_footer_lines("The reported results for each method of analysis (GEE independence, GEE exchangeable and mixed effects model) exclude datasets where the analysis model did not converge.") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Type I error (Supp Table 11)
# data shown in Supp Figure 2

t1err1 = npd1 %>%
  mutate(rand_method = "cluster",
         gee_method  = "ind")
t1err2 = npd2 %>%
  mutate(rand_method = "cluster",
         gee_method  = "exch")
t1err3 = npd3 %>%
  mutate(rand_method = "ind",
         gee_method  = "ind")
t1err4 = npd4 %>%
  mutate(rand_method = "ind",
         gee_method  = "exch")
t1err5 = npd5 %>%
  mutate(rand_method = "opp",
         gee_method  = "ind")
t1err6 = npd6 %>%
  mutate(rand_method = "opp",
         gee_method  = "exch")
t1err_ind = bind_rows(t1err1, t1err3, t1err5) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Independence")
t1err_exch = bind_rows(t1err2, t1err4, t1err6) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Exchangeable")
t1err_data = bind_rows(t1err_ind, t1err_exch) %>% 
  relocate(de, .before="Effect size")

suppT11 = flextable(data = t1err_data) %>%
  set_caption("Supplementary Table 11: Type I error rates of GEEs and mixed models by randomisation methods and GEE working correlation structure") %>% 
  add_header_row(colwidths = c(4, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(4, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(de = "Design effect", "Effect size" = "ES", "Probability of a pair" = "P(pair)", cluster_geeglm = "GEE", cluster_mixed = "Mixed model",
                                  ind_geeglm = "GEE", ind_mixed = "Mixed model", opp_geeglm = "GEE", opp_mixed = "Mixed model")) %>% 
  merge_v(j = ~ de) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 4) %>% 
  hline(i = seq(4, 72, by=4)) %>% 
  colformat_double(j = c("cluster_geeglm", "cluster_mixed", "ind_geeglm", "ind_mixed", "opp_geeglm", "opp_mixed"), digits=3) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("GEE = generalised estimating equation, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  add_footer_lines("The reported results for each method of analysis (GEE independence, GEE exchangeable and mixed effects model) exclude datasets where the analysis model did not converge.") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Performance measures of exchangeable GEE, excluding non-positive definite results (Supp Table 12)
# data shown in Supp Figure 7


## Type I error rates in small samples (Supp Table 13)
# data shown in Supp Figure 8

smt1error1 = std1 %>%
  mutate(rand_method = "cluster",
         gee_method  = "ind")
smt1error2 = std2 %>%
  mutate(rand_method = "cluster",
         gee_method  = "exch")
smt1error3 = std3 %>%
  mutate(rand_method = "ind",
         gee_method  = "ind")
smt1error4 = std4 %>%
  mutate(rand_method = "ind",
         gee_method  = "exch")
smt1error5 = std5 %>%
  mutate(rand_method = "opp",
         gee_method  = "ind")
smt1error6 = std6 %>%
  mutate(rand_method = "opp",
         gee_method  = "exch")
smt1error_ind = bind_rows(smt1error1, smt1error3, smt1error5) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Independence")
smt1error_exch = bind_rows(smt1error2, smt1error4, smt1error6) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Exchangeable")
smt1error_data = bind_rows(smt1error_ind, smt1error_exch) %>% 
  relocate(de, .before="Effect size")

suppT13 = flextable(data = smt1error_data) %>%
  set_caption("Supplementary Table 13: Type I error rates of GEEs and mixed models with small sample corrections by randomisation methods and GEE working correlation structure for a subset of scenarios with small sample sizes") %>% 
  add_header_row(colwidths = c(4, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(4, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(de = "Design effect", "Effect size" = "ES", "Probability of a pair" = "P(pair)",
                                  "cluster_gee-md" = "GEE: MD", "cluster_mixed-kr" = "Mixed model: KR", "ind_gee-md" = "GEE: MD",
                                  "ind_mixed-kr" = "Mixed model: KR", "opp_gee-md" = "GEE: MD", "opp_mixed-kr" = "Mixed model: KR")) %>% 
  merge_v(j = ~ de) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 4) %>% 
  hline(i = seq(3, 18, by=3)) %>% 
  colformat_double(j = c("cluster_gee-md", "cluster_mixed-kr", "ind_gee-md", "ind_mixed-kr", "opp_gee-md", "opp_mixed-kr"), digits=3) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("GEE = generalised estimating equation, MD = Mancl-DeRouen correction, KR = Kenward-Roger correction, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  add_footer_lines("The reported results for each method of analysis (GEE independence, GEE exchangeable and mixed effects model) exclude datasets where the analysis model did not converge.") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Power in small samples (Supp Table 14)
# data shown in Supp Figure 9

smpower1 = spd1 %>%
  mutate(rand_method = "cluster",
         gee_method  = "ind")
smpower2 = spd2 %>%
  mutate(rand_method = "cluster",
         gee_method  = "exch")
smpower3 = spd3 %>%
  mutate(rand_method = "ind",
         gee_method  = "ind")
smpower4 = spd4 %>%
  mutate(rand_method = "ind",
         gee_method  = "exch")
smpower5 = spd5 %>%
  mutate(rand_method = "opp",
         gee_method  = "ind")
smpower6 = spd6 %>%
  mutate(rand_method = "opp",
         gee_method  = "exch")
smpower_ind = bind_rows(smpower1, smpower3, smpower5) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Independence")
smpower_exch = bind_rows(smpower2, smpower4, smpower6) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Exchangeable")
smpower_data = bind_rows(smpower_ind, smpower_exch) %>% 
  relocate(de, .before="Effect size")

suppT14 = flextable(data = smpower_data) %>%
  set_caption("Supplementary Table 14: Power of GEEs and mixed models with small sample corrections by randomisation methods and GEE working correlation structure for a subset of scenarios with small sample sizes") %>% 
  add_header_row(colwidths = c(4, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(4, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(de = "Design effect", "Effect size" = "ES", "Probability of a pair" = "P(pair)",
                                  "cluster_gee-md" = "GEE: MD", "cluster_mixed-kr" = "Mixed model: KR", "ind_gee-md" = "GEE: MD",
                                  "ind_mixed-kr" = "Mixed model: KR", "opp_gee-md" = "GEE: MD", "opp_mixed-kr" = "Mixed model: KR")) %>% 
  merge_v(j = ~ de) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 4) %>% 
  hline(i = seq(3, 18, by=3)) %>% 
  colformat_double(j = c("cluster_gee-md", "cluster_mixed-kr", "ind_gee-md", "ind_mixed-kr", "opp_gee-md", "opp_mixed-kr"), digits=3) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("GEE = generalised estimating equation, MD = Mancl-DeRouen correction, KR = Kenward-Roger correction, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  add_footer_lines("The reported results for each method of analysis (GEE independence, GEE exchangeable and mixed effects model) exclude datasets where the analysis model did not converge.") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)

## Relative error in small samples (Supp Table 15)
# data shown in Supp Figure 10

smrelerr1 = srd1 %>%
  mutate(rand_method = "cluster",
         gee_method  = "ind")
smrelerr2 = srd2 %>%
  mutate(rand_method = "cluster",
         gee_method  = "exch")
smrelerr3 = srd3 %>%
  mutate(rand_method = "ind",
         gee_method  = "ind")
smrelerr4 = srd4 %>%
  mutate(rand_method = "ind",
         gee_method  = "exch")
smrelerr5 = srd5 %>%
  mutate(rand_method = "opp",
         gee_method  = "ind")
smrelerr6 = srd6 %>%
  mutate(rand_method = "opp",
         gee_method  = "exch")
smrelerr_ind = bind_rows(smrelerr1, smrelerr3, smrelerr5) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Independence")
smrelerr_exch = bind_rows(smrelerr2, smrelerr4, smrelerr6) %>% 
  pivot_wider(id_cols=c("Effect size", ICC, "Probability of a pair"), names_from = c(rand_method, model), values_from = est) %>% 
  mutate(de = "Exchangeable")
smrelerr_data = bind_rows(smrelerr_ind, smrelerr_exch) %>% 
  relocate(de, .before="Effect size")

suppT15 = flextable(data = smrelerr_data) %>%
  set_caption("Supplementary Table 15: Relative percent error in the average estimated SE for GEEs and mixed models with small sample corrections by randomisation methods and GEE working correlation structure for a subset of scenarios with small sample sizes") %>% 
  add_header_row(colwidths = c(4, 2, 2, 2), values = c("", "Cluster", "Individual", "Balanced")) %>% 
  add_header_row(colwidths = c(4, 6), values = c("", "Randomisation method")) %>% 
  set_header_labels(values = list(de = "Design effect", "Effect size" = "ES", "Probability of a pair" = "P(pair)",
                                  "cluster_gee-md" = "GEE: MD", "cluster_mixed-kr" = "Mixed model: KR", "ind_gee-md" = "GEE: MD",
                                  "ind_mixed-kr" = "Mixed model: KR", "opp_gee-md" = "GEE: MD", "opp_mixed-kr" = "Mixed model: KR")) %>% 
  merge_v(j = ~ de) %>% 
  align(align = "center", part = "header") %>% 
  vline(j = 4) %>% 
  hline(i = seq(3, 18, by=3)) %>% 
  colformat_double(j = c("cluster_gee-md", "cluster_mixed-kr", "ind_gee-md", "ind_mixed-kr", "opp_gee-md", "opp_mixed-kr"), digits=2) %>% 
  line_spacing(space=0.5) %>% 
  add_footer_lines("GEE = generalised estimating equation, MD = Mancl-DeRouen correction, KR = Kenward-Roger correction, DE = design effect, ES = minimum clinically important effect size, ICC = intracluster correlation coefficient, P(pair) = probability of a paired cluster") %>% 
  add_footer_lines("The reported results for each method of analysis (GEE independence, GEE exchangeable and mixed effects model) exclude datasets where the analysis model did not converge.") %>% 
  fontsize(size=9) %>% 
  fontsize(part="header", size=10) %>% 
  fontsize(part="footer", size=9)



## Save as Word file

read_docx() %>% 
  body_add_flextable(suppT4) %>% 
  body_add_break() %>%  
  body_add_flextable(suppT5) %>% 
  body_add_break() %>% 
  body_add_flextable(suppT6) %>%
  body_add_break() %>% 
  body_add_flextable(suppT7) %>%
  body_add_break() %>% 
  body_add_flextable(suppT8) %>%
  body_add_break() %>% 
  body_add_flextable(suppT9) %>%
  body_add_break() %>% 
  body_add_flextable(suppT10) %>%
  body_add_break() %>% 
  body_add_flextable(suppT11) %>%
  body_add_break() %>% 
  body_add_flextable(suppT13) %>%
  body_add_break() %>% 
  body_add_flextable(suppT14) %>%
  body_add_break() %>% 
  body_add_flextable(suppT15) %>%
  print(target="output/tables.docx")
