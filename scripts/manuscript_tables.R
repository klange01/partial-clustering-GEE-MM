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

## Save as Word file

read_docx() %>% 
  body_add_flextable(suppT4) %>% 
  body_add_break() %>%  
  body_add_flextable(suppT5) %>% 
  body_add_break() %>% 
  body_add_flextable(suppT6) %>%
  print(target="output/tables.docx")
