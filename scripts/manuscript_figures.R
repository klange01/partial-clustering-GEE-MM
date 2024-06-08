## Performance of GEEs vs mixed models for analysing partially clustered data
## Kylie Lange (kylie.lange@adelaide.edu.au)

## Manuscript figures

library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(cowplot)
library(patchwork)
library(egg)

source("R/nlp_GEEvMEM.R")
source("R/nlp_GEEvMEMref.R")
source("R/nlp_byDE.R")
source("R/nlp_byDEref.R")

### Plot data
row1 = ggplot() + annotate(geom='text', x=1, y=1, label="Cluster", angle=90, size=3) + theme_void()
row2 = ggplot() + annotate(geom='text', x=1, y=1, label="Individual", angle=90, size=3) + theme_void()
row3 = ggplot() + annotate(geom='text', x=1, y=1, label="Balanced", angle=90, size=3) + theme_void()
col1 = ggplot() + annotate(geom='text', x=1, y=1, label="Independence DE", size=3) + theme_void()
col2 = ggplot() + annotate(geom='text', x=1, y=1, label="Exchangeable DE", size=3) + theme_void()

layout6 = '
#dddddeeeee
afffffggggg
afffffggggg
afffffggggg
afffffggggg
bhhhhhiiiii
bhhhhhiiiii
bhhhhhiiiii
bhhhhhiiiii
cjjjjjkkkkk
cjjjjjkkkkk
cjjjjjkkkkk
cjjjjjkkkkk'

layout4 = '
#dddddeeeee
afffffggggg
afffffggggg
afffffggggg
afffffggggg
bhhhhhiiiii
bhhhhhiiiii
bhhhhhiiiii
bhhhhhiiiii'

layoutpd = '
fffffggggg
fffffggggg
fffffggggg
fffffggggg
hhhhhiiiii
hhhhhiiiii
hhhhhiiiii
hhhhhiiiii'

top=FALSE

### Power (Fig 1)
# Cluster randomisation: GEE-indp vs mixed model(ind)
pd1 = tidy(s1, stats = "power")
pp1 = .nlp2(data = pd1, methodvar = s1$methodvar, by = s1$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
            pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.5, 1))
# Cluster randomisation: GEE-exch vs mixed model(exch)
pd2 = tidy(s2, stats = "power")
pp2 = .nlp2(data = pd2, methodvar = s2$methodvar, by = s2$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
            pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.5,1))
# Individual  randomisation: GEE-indp vs mixed model(ind)
pd3 = tidy(s3, stats = "power")
pp3 = .nlp2(data = pd3, methodvar = s3$methodvar, by = s3$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
            pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.5, 1))
# Individual  randomisation: GEE-exch vs mixed model(exch)
pd4 = tidy(s4, stats = "power")
pp4 = .nlp2(data = pd4, methodvar = s4$methodvar, by = s4$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
            pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.5,1))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
pd5 = tidy(s5, stats = "power")
pp5 = .nlp2(data = pd5, methodvar = s5$methodvar, by = s5$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
            pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.5, 1))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
pd6 = tidy(s6, stats = "power")
pp6 = .nlp2(data = pd6, methodvar = s6$methodvar, by = s6$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
            pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.5,1))
# multipanel plot
plotlist = list(a=row1, b=row2, c=row3, d=col1, e=col2, f=pp1, g=pp2, h=pp3, i=pp4, j=pp5, k=pp6)
wrap_plots(plotlist, design=layout6)
ggsave("output/power.tiff",height=7.5,width=9,unit="in",dpi=800)

### Coverage (Fig 2)
reflow_cov = 0.944
refhigh_cov = 0.956
# Cluster randomisation: GEE-indp vs mixed model(ind)
cd1 = tidy(s1, stats = "cover")
pc1 = .nlp2ref(data = cd1, methodvar = s1$methodvar, by = s1$by, stats = "cover", target = 0.95,
               reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.74,0.96))
# Cluster randomisation: GEE-exch vs mixed model(exch)
cd2 = tidy(s2, stats = "cover")
pc2 = .nlp2ref(data = cd2, methodvar = s2$methodvar, by = s2$by, stats = "cover", target = 0.95,
               reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.74,0.96))
# Individual  randomisation: GEE-indp vs mixed model(ind)
cd3 = tidy(s3, stats = "cover")
pc3 = .nlp2ref(data = cd3, methodvar = s3$methodvar, by = s3$by, stats = "cover", target = 0.95,
               reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.74,0.96))
# Individual  randomisation: GEE-exch vs mixed model(exch)
cd4 = tidy(s4, stats = "cover")
pc4 = .nlp2ref(data = cd4, methodvar = s4$methodvar, by = s4$by, stats = "cover", target = 0.95,
               reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.74,0.96))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
cd5 = tidy(s5, stats = "cover")
pc5 = .nlp2ref(data = cd5, methodvar = s5$methodvar, by = s5$by, stats = "cover", target = 0.95,
               reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.74,0.96))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
cd6 = tidy(s6, stats = "cover")
pc6 = .nlp2ref(data = cd6, methodvar = s6$methodvar, by = s6$by, stats = "cover", target = 0.95,
               reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.74,0.96))
# multipanel plot
plotlist = list(a=row1, b=row2, c=row3, d=col1, e=col2, f=pc1, g=pc2, h=pc3, i=pc4, j=pc5, k=pc6)
wrap_plots(plotlist, design=layout6)
ggsave("output/coverage.tiff",height=7.5,width=9,unit="in",dpi=800)

### Coverage for small samples (Fig 3)
# Cluster randomisation: GEE-indp vs mixed model(ind)
scd1 = tidy(ss1, stats = "cover")
sc1 = .nlp2ref(data = scd1, methodvar = ss1$methodvar, by = ss1$by, stats = "cover", target = 0.95,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_cov, refhigh = refhigh_cov,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.925,0.97))
# Cluster randomisation: GEE-exch vs mixed model(exch)
scd2 = tidy(ss2, stats = "cover")
sc2 = .nlp2ref(data = scd2, methodvar = ss2$methodvar, by = ss2$by, stats = "cover", target = 0.95,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_cov, refhigh = refhigh_cov,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.925,0.97))
# Individual  randomisation: GEE-indp vs mixed model(ind)
scd3 = tidy(ss3, stats = "cover")
sc3 = .nlp2ref(data = scd3, methodvar = ss3$methodvar, by = ss3$by, stats = "cover", target = 0.95,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_cov, refhigh = refhigh_cov,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.925,0.97))
# Individual  randomisation: GEE-exch vs mixed model(exch)
scd4 = tidy(ss4, stats = "cover")
sc4 = .nlp2ref(data = scd4, methodvar = ss4$methodvar, by = ss4$by, stats = "cover", target = 0.95,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_cov, refhigh = refhigh_cov,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.925,0.97))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
scd5 = tidy(ss5, stats = "cover")
sc5 = .nlp2ref(data = scd5, methodvar = ss5$methodvar, by = ss5$by, stats = "cover", target = 0.95,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_cov, refhigh = refhigh_cov,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.925,0.97))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
scd6 = tidy(ss6, stats = "cover")
sc6 = .nlp2ref(data = scd6, methodvar = ss6$methodvar, by = ss6$by, stats = "cover", target = 0.95,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_cov, refhigh = refhigh_cov,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.925,0.97))
# multipanel plot
plotlist = list(a=row1, b=row2, c=row3, d=col1, e=col2, f=sc1, g=sc2, h=sc3, i=sc4, j=sc5, k=sc6)
wrap_plots(plotlist, design=layout6)
ggsave("output/smallsample-coverage.tiff",height=7.5,width=9,unit="in",dpi=800)

### Relative error (Supp Fig 1)
# Cluster randomisation: GEE-indp vs mixed model(ind)
rd1 = tidy(s1, stats = "relerror")
pr1 = .nlp2(data = rd1, methodvar = s1$methodvar, by = s1$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
            ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-20,200))
# Cluster randomisation: GEE-exch vs mixed model(exch)
rd2 = tidy(s2, stats = "relerror")
pr2 = .nlp2(data = rd2, methodvar = s2$methodvar, by = s2$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
            ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-20,200))
# Individual  randomisation: GEE-indp vs mixed model(ind)
rd3 = tidy(s3, stats = "relerror")
pr3 = .nlp2(data = rd3, methodvar = s3$methodvar, by = s3$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
            ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-20,200))
# Individual  randomisation: GEE-exch vs mixed model(exch)
rd4 = tidy(s4, stats = "relerror")
pr4 = .nlp2(data = rd4, methodvar = s4$methodvar, by = s4$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
            ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-20,200))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
rd5 = tidy(s5, stats = "relerror")
pr5 = .nlp2(data = rd5, methodvar = s5$methodvar, by = s5$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
            ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-20,200))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
rd6 = tidy(s6, stats = "relerror")
pr6 = .nlp2(data = rd6, methodvar = s6$methodvar, by = s6$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
            ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-20,200))
# multipanel plot
plotlist = list(a=row1, b=row2, c=row3, d=col1, e=col2, f=pr1, g=pr2, h=pr3, i=pr4, j=pr5, k=pr6)
wrap_plots(plotlist, design=layout6)
ggsave("output/relative-error.tiff",height=7.5,width=9,unit="in",dpi=800)

### Type I error (Supp Fig 2)
reflow_type1 = 0.044
refhigh_type1 = 0.056
# Cluster randomisation: GEE-indp vs mixed model(ind)
npd1 = tidy(ns1, stats = "power")
tp1 = .nlp2ref(data = npd1, methodvar = ns1$methodvar, by = ns1$by, stats = "power", target = 0.05,
               reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
               ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.043,0.25)) 
# Cluster randomisation: GEE-exch vs mixed model(exch)
npd2 = tidy(ns2, stats = "power")
tp2 = .nlp2ref(data = npd2, methodvar = ns2$methodvar, by = ns2$by, stats = "power", target = 0.05,
               reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
               ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.043,0.25)) 
# Individual randomisation: GEE-indp vs mixed model(ind)
npd3 = tidy(ns3, stats = "power")
tp3 = .nlp2ref(data = npd3, methodvar = ns3$methodvar, by = ns3$by, stats = "power", target = 0.05,
               reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
               ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.043,0.25)) 
# Individual randomisation: GEE-exch vs mixed model(exch)
npd4 = tidy(ns4, stats = "power")
tp4 = .nlp2ref(data = npd4, methodvar = ns4$methodvar, by = ns4$by, stats = "power", target = 0.05,
               reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
               ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.043,0.25)) 
# Balanced randomisation: GEE-indp vs mixed model(ind)
npd5 = tidy(ns5, stats = "power")
tp5 = .nlp2ref(data = npd5, methodvar = ns5$methodvar, by = ns5$by, stats = "power", target = 0.05,
               reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
               ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.043,0.25)) 
# Balanced randomisation: GEE-exch vs mixed model(exch)
npd6 = tidy(ns6, stats = "power")
tp6 = .nlp2ref(data = npd6, methodvar = ns6$methodvar, by = ns6$by, stats = "power", target = 0.05,
               reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
               ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.043,0.25)) 
# multipanel plot
plotlist = list(a=row1, b=row2, c=row3, d=col1, e=col2, f=tp1, g=tp2, h=tp3, i=tp4, j=tp5, k=tp6)
wrap_plots(plotlist, design=layout6)
ggsave("output/typeI-error.tiff",height=7.5,width=9,unit="in",dpi=800)

### Power for realistic scenarios (Supp Fig 3)
# Set 1
# Individual  randomisation: GEE-indp vs mixed model(ind)
pr31 = tidy(rs31, stats = "power")
rp31 = .nlp2(data = pr31, methodvar = rs31$methodvar, by = rs31$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
             pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.45, 1))
# Individual  randomisation: GEE-exch vs mixed model(exch)
pr41 = tidy(rs41, stats = "power")
rp41 = .nlp2(data = pr41, methodvar = rs41$methodvar, by = rs41$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
             pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.45, 1))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
pr51 = tidy(rs51, stats = "power")
rp51 = .nlp2(data = pr51, methodvar = rs51$methodvar, by = rs51$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
             pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.45, 1))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
pr61 = tidy(rs61, stats = "power")
rp61 = .nlp2(data = pr61, methodvar = rs61$methodvar, by = rs61$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
             pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.45, 1))
# Set 2
# Individual  randomisation: GEE-indp vs mixed model(ind)
pr32 = tidy(rs32, stats = "power")
rp32 = .nlp2(data = pr32, methodvar = rs32$methodvar, by = rs32$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
             pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.45, 1))
# Individual  randomisation: GEE-exch vs mixed model(exch)
pr42 = tidy(rs42, stats = "power")
rp42 = .nlp2(data = pr42, methodvar = rs42$methodvar, by = rs42$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
             pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.45,1))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
pr52 = tidy(rs52, stats = "power")
rp52 = .nlp2(data = pr52, methodvar = rs52$methodvar, by = rs52$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
             pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.45, 1))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
pr62 = tidy(rs62, stats = "power")
rp62 = .nlp2(data = pr62, methodvar = rs62$methodvar, by = rs62$by, stats = "power", target = 0.8, top = top, linewidth=0.4, atextsize=6,
             pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"), ltitle="", pheight=c(6.5,3.5), y.lim=c(0.45,1))
# multipanel plot
p3 = wrap_plots(rp31, rp32)
p4 = wrap_plots(rp41, rp42)
p5 = wrap_plots(rp51, rp52)
p6 = wrap_plots(rp61, rp62)
plotlist = list(a=row2, b=row3, d=col1, e=col2, f=p3, g=p4, h=p5, i=p6)
wrap_plots(plotlist, design=layout4)
ggsave("output/realistic-scenarios-power.tiff",height=7.5,width=9,unit="in",dpi=800)

### Coverage for realistic scenarios (Supp Fig 4)
reflow_cov = 0.944
refhigh_cov = 0.956
# Set 1
# Individual  randomisation: GEE-indp vs mixed model(ind)
cr31 = tidy(rs31, stats = "cover")
rc31 = .nlp2ref(data = cr31, methodvar = rs31$methodvar, by = rs31$by, stats = "cover", target = 0.95,
                reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6, 
                pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
                ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.88,0.96))
# Individual  randomisation: GEE-exch vs mixed model(exch)
cr41 = tidy(rs41, stats = "cover")
rc41 = .nlp2ref(data = cr41, methodvar = rs41$methodvar, by = rs41$by, stats = "cover", target = 0.95,
                reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
                pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
                ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.88,0.96))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
cr51 = tidy(rs51, stats = "cover")
rc51 = .nlp2ref(data = cr51, methodvar = rs51$methodvar, by = rs51$by, stats = "cover", target = 0.95,
                reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
                pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
                ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.88,0.96))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
cr61 = tidy(rs61, stats = "cover")
rc61 = .nlp2ref(data = cr61, methodvar = rs61$methodvar, by = rs61$by, stats = "cover", target = 0.95,
                reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
                pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
                ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.88,0.96))
# Set 2
# Individual  randomisation: GEE-indp vs mixed model(ind)
cr32 = tidy(rs32, stats = "cover")
rc32 = .nlp2ref(data = cr32, methodvar = rs32$methodvar, by = rs32$by, stats = "cover", target = 0.95,
                reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
                pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
                ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.88,0.96))
# Individual  randomisation: GEE-exch vs mixed model(exch)
cr42 = tidy(rs42, stats = "cover")
rc42 = .nlp2ref(data = cr42, methodvar = rs42$methodvar, by = rs42$by, stats = "cover", target = 0.95,
                reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
                pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
                ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.88,0.96))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
cr52 = tidy(rs52, stats = "cover")
rc52 = .nlp2ref(data = cr52, methodvar = rs52$methodvar, by = rs52$by, stats = "cover", target = 0.95,
                reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
                pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
                ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.88,0.96))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
cr62 = tidy(rs62, stats = "cover")
rc62 = .nlp2ref(data = cr62, methodvar = rs62$methodvar, by = rs62$by, stats = "cover", target = 0.95,
                reflow = reflow_cov, refhigh = refhigh_cov, top = top, linewidth=0.4, atextsize=6,
                pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
                ylab="coverage", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.88,0.96))
# multipanel plot
c3 = wrap_plots(rc31, rc32)
c4 = wrap_plots(rc41, rc42)
c5 = wrap_plots(rc51, rc52)
c6 = wrap_plots(rc61, rc62)
plotlist = list(a=row2, b=row3, d=col1, e=col2, f=c3, g=c4, h=c5, i=c6)
wrap_plots(plotlist, design=layout4)
ggsave("output/realistic-scenarios-cover.tiff",height=7.5,width=9,unit="in",dpi=800)

### Relative error for realistic scenarios (Supp Fig 5)
# Set 1
# Individual  randomisation: GEE-indp vs mixed model(ind)
er31 = tidy(rs31, stats = "relerror")
rr31 = .nlp2(data = er31, methodvar = rs31$methodvar, by = rs31$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
             atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
             ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-10,200))
# Individual  randomisation: GEE-exch vs mixed model(exch)
er41 = tidy(rs41, stats = "relerror")
rr41 = .nlp2(data = er41, methodvar = rs41$methodvar, by = rs41$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
             atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
             ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-10,200))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
er51 = tidy(rs51, stats = "relerror")
rr51 = .nlp2(data = er51, methodvar = rs51$methodvar, by = rs51$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
             atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
             ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-10,200))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
er61 = tidy(rs61, stats = "relerror")
rr61 = .nlp2(data = er61, methodvar = rs61$methodvar, by = rs61$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
             atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
             ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-10,200))
# Set 2
# Individual  randomisation: GEE-indp vs mixed model(ind)
er32 = tidy(rs32, stats = "relerror")
rr32 = .nlp2(data = er32, methodvar = rs32$methodvar, by = rs32$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
             atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
             ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-10,200))
# Individual  randomisation: GEE-exch vs mixed model(exch)
er42 = tidy(rs42, stats = "relerror")
rr42 = .nlp2(data = er42, methodvar = rs42$methodvar, by = rs42$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
             atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
             ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-10,200))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
er52 = tidy(rs52, stats = "relerror")
rr52 = .nlp2(data = er52, methodvar = rs52$methodvar, by = rs52$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
             atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
             ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-10,200))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
er62 = tidy(rs62, stats = "relerror")
rr62 = .nlp2(data = er62, methodvar = rs62$methodvar, by = rs62$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
             atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
             ylab="relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-10,200))
# multipanel plot
r3 = wrap_plots(rr31, rr32)
r4 = wrap_plots(rr41, rr42)
r5 = wrap_plots(rr51, rr52)
r6 = wrap_plots(rr61, rr62)
plotlist = list(a=row2, b=row3, d=col1, e=col2, f=r3, g=r4, h=r5, i=r6)
wrap_plots(plotlist, design=layout4)
ggsave("output/realistic-scenarios-relerror.tiff",height=7.5,width=9,unit="in",dpi=800)

### Type I error for realistic scenarios (Supp Fig 6)
# Set 1
# Individual randomisation: GEE-indp vs mixed model(ind)
nr31 = tidy(nrs31, stats = "power")
tr31 = .nlp2ref(data = nr31, methodvar = nrs31$methodvar, by = nrs31$by, stats = "power", target = 0.05,
                reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
                pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
                ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.04,0.15)) 
# Individual randomisation: GEE-exch vs mixed model(exch)
nr41 = tidy(nrs41, stats = "power")
tr41 = .nlp2ref(data = nr41, methodvar = nrs41$methodvar, by = nrs41$by, stats = "power", target = 0.05,
                reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
                pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
                ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.04,0.15)) 
# Balanced randomisation: GEE-indp vs mixed model(ind)
nr51 = tidy(nrs51, stats = "power")
tr51 = .nlp2ref(data = nr51, methodvar = nrs51$methodvar, by = nrs51$by, stats = "power", target = 0.05,
                reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
                pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
                ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.04,0.15)) 
# Balanced randomisation: GEE-exch vs mixed model(exch)
nr61 = tidy(nrs61, stats = "power")
tr61 = .nlp2ref(data = nr61, methodvar = nrs61$methodvar, by = nrs61$by, stats = "power", target = 0.05,
                reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
                pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
                ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.04,0.15)) 
# Set 2
# Individual randomisation: GEE-indp vs mixed model(ind)
nr32 = tidy(nrs32, stats = "power")
tr32 = .nlp2ref(data = nr32, methodvar = nrs32$methodvar, by = nrs32$by, stats = "power", target = 0.05,
                reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
                pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
                ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.04,0.15)) 
# Individual randomisation: GEE-exch vs mixed model(exch)
nr42 = tidy(nrs42, stats = "power")
tr42 = .nlp2ref(data = nr42, methodvar = nrs42$methodvar, by = nrs42$by, stats = "power", target = 0.05,
                reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
                pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
                ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.04,0.15)) 
# Balanced randomisation: GEE-indp vs mixed model(ind)
nr52 = tidy(nrs52, stats = "power")
tr52 = .nlp2ref(data = nr52, methodvar = nrs52$methodvar, by = nrs52$by, stats = "power", target = 0.05,
                reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
                pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence", "Mixed model"),
                ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.04,0.15)) 
# Balanced randomisation: GEE-exch vs mixed model(exch)
nr62 = tidy(nrs62, stats = "power")
tr62 = .nlp2ref(data = nr62, methodvar = nrs62$methodvar, by = nrs62$by, stats = "power", target = 0.05,
                reflow = reflow_type1, refhigh = refhigh_type1, top = top, linewidth=0.4, atextsize=6,
                pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable", "Mixed model"),
                ylab="type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.04,0.15)) 
# multipanel plot
t3 = wrap_plots(tr31, tr32)
t4 = wrap_plots(tr41, tr42)
t5 = wrap_plots(tr51, tr52)
t6 = wrap_plots(tr61, tr62)
plotlist = list(a=row2, b=row3, d=col1, e=col2, f=t3, g=t4, h=t5, i=t6)
wrap_plots(plotlist, design=layout4)
ggsave("output/realistic-scenarios-typeIerror.tiff",height=7.5,width=9,unit="in",dpi=800)

### Performance measures of exchangeable GEE when exclude NPD results (Supp Fig 7)
# Relative error
pdr = tidy(pde, stats = "relerror")
pdrp = .nlp2de(data = pdr, methodvar = pde$methodvar, by = pde$by, stats = "relerror", target = 0,
              top = top, linewidth=0.4, atextsize=6, pcol=c("seagreen4", "gold3", "darkorchid1"),
              mlab=c("Cluster randomisation", "Individual randomisation", "Balanced randomisation"), ylab="relative error", ltitle="", pheight=c(6.5,3.5)) 
# Coverage
pdc = tidy(pde, stats = "cover")
pdcp = .nlp2deref(data = pdc, methodvar = pde$methodvar, by = pde$by, stats = "cover", target = 0.95,
                 reflow = reflow_cov, refhigh = refhigh_cov, top = top, ylab = "coverage",
                 linewidth=0.4, atextsize=6, pcol=c("seagreen4", "gold3", "darkorchid1"),
                 mlab=c("Cluster randomisation", "Individual randomisation", "Balanced randomisation"), ltitle="", pheight=c(6.5,3.5))
# Type I error
pdn = tidy(npde, stats = "power")
pdnp = .nlp2deref(data = pdn, methodvar = npde$methodvar, by = npde$by, stats = "power", target = 0.05,
                 reflow = reflow_type1, refhigh = refhigh_type1, top = top, ylab = "type I error",
                 linewidth=0.4, atextsize=6, pcol=c("seagreen4", "gold3", "darkorchid1"),
                 mlab=c("Cluster randomisation", "Individual randomisation", "Balanced randomisation"), ltitle="", pheight=c(6.5,3.5))
# Power
pdp = tidy(pde, stats = "power")
pdpp = .nlp2de(data = pdp, methodvar = pde$methodvar, by = pde$by, stats = "power", target = 0.8,
              top = top, linewidth=0.4, atextsize=6, pcol=c("seagreen4", "gold3", "darkorchid1"),
              mlab=c("Cluster randomisation", "Individual randomisation", "Balanced randomisation"), ltitle="", pheight=c(6.5,3.5)) 
# multipanel plot
plotlist = list(f=pdrp, g=pdcp, h=pdnp, i=pdpp)
wrap_plots(plotlist, design=layoutpd)
ggsave("output/positive-definite.tiff",height=7.5,width=9,unit="in",dpi=800)

### Type I error of small sample corrections (Supp Fig 8)
# Cluster randomisation: GEE-indp vs mixed model(ind)
std1 = tidy(st1, stats = "power")
stp1 = .nlp2ref(data = std1, methodvar = st1$methodvar, by = st1$by, stats = "power", target = 0.05,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_type1, refhigh = refhigh_type1,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab = "type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.023, 0.0683))
# Cluster randomisation: GEE-exch vs mixed model(exch)
std2 = tidy(st2, stats = "power")
stp2 = .nlp2ref(data = std2, methodvar = st2$methodvar, by = st2$by, stats = "power", target = 0.05,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_type1, refhigh = refhigh_type1,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab = "type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.023, 0.0683))
# Individual randomisation: GEE-indp vs mixed model(ind)
std3 = tidy(st3, stats = "power")
stp3 = .nlp2ref(data = std3, methodvar = st3$methodvar, by = st3$by, stats = "power", target = 0.05,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_type1, refhigh = refhigh_type1,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab = "type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.023, 0.0683))
# Individual randomisation: GEE-exch vs mixed model(exch)
std4 = tidy(st4, stats = "power")
stp4 = .nlp2ref(data = std4, methodvar = st4$methodvar, by = st4$by, stats = "power", target = 0.05,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_type1, refhigh = refhigh_type1,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab = "type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.023, 0.0683))
# Balanced randomisation: GEE-indp vs mixed model(ind)
std5 = tidy(st5, stats = "power")
stp5 = .nlp2ref(data = std5, methodvar = st5$methodvar, by = st5$by, stats = "power", target = 0.05,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_type1, refhigh = refhigh_type1,
               pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab = "type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.023, 0.0683))
# Balanced randomisation: GEE-exch vs mixed model(exch)
std6 = tidy(st6, stats = "power")
stp6 = .nlp2ref(data = std6, methodvar = st6$methodvar, by = st6$by, stats = "power", target = 0.05,
               top = top, linewidth=0.4, atextsize=6, reflow = reflow_type1, refhigh = refhigh_type1,
               pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
               ylab = "type I error", ltitle="", pheight=c(6.5,3.5), y.lim=c(0.023, 0.0683))
# multipanel plot
plotlist = list(a=row1, b=row2, c=row3, d=col1, e=col2, f=stp1, g=stp2, h=stp3, i=stp4, j=stp5, k=stp6)
wrap_plots(plotlist, design=layout6)
ggsave("output/smallsample-typeIerror.tiff",height=7.5,width=9,unit="in",dpi=800)

### Power of small sample corrections (Supp Fig 9)
# Cluster randomisation: GEE-indp vs mixed model(ind)
spd1 = tidy(sp1, stats = "power")
spp1 = .nlp2(data = spd1, methodvar = sp1$methodvar, by = sp1$by, stats = "power", target = 0.8, top = top, linewidth=0.4,
            atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ltitle="", pheight=c(6.5,3.5), y.lim=c(0.3, 1))
# Cluster randomisation: GEE-exch vs mixed model(exch)
spd2 = tidy(sp2, stats = "power")
spp2 = .nlp2(data = spd2, methodvar = sp2$methodvar, by = sp2$by, stats = "power", target = 0.8, top = top, linewidth=0.4,
            atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ltitle="", pheight=c(6.5,3.5), y.lim=c(0.3, 1))
# Individual  randomisation: GEE-indp vs mixed model(ind)
spd3 = tidy(sp3, stats = "power")
spp3 = .nlp2(data = spd3, methodvar = sp3$methodvar, by = sp3$by, stats = "power", target = 0.8, top = top, linewidth=0.4,
            atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ltitle="", pheight=c(6.5,3.5), y.lim=c(0.3, 1))
# Individual  randomisation: GEE-exch vs mixed model(exch)
spd4 = tidy(sp4, stats = "power")
spp4 = .nlp2(data = spd4, methodvar = sp4$methodvar, by = sp4$by, stats = "power", target = 0.8, top = top, linewidth=0.4,
            atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ltitle="", pheight=c(6.5,3.5), y.lim=c(0.3, 1))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
spd5 = tidy(sp5, stats = "power")
spp5 = .nlp2(data = spd5, methodvar = sp5$methodvar, by = sp5$by, stats = "power", target = 0.8, top = top, linewidth=0.4,
            atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ltitle="", pheight=c(6.5,3.5), y.lim=c(0.3, 1))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
spd6 = tidy(sp6, stats = "power")
spp6 = .nlp2(data = spd6, methodvar = sp6$methodvar, by = sp6$by, stats = "power", target = 0.8, top = top, linewidth=0.4,
            atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ltitle="", pheight=c(6.5,3.5), y.lim=c(0.3, 1))
# multipanel plot
plotlist = list(a=row1, b=row2, c=row3, d=col1, e=col2, f=spp1, g=spp2, h=spp3, i=spp4, j=spp5, k=spp6)
wrap_plots(plotlist, design=layout6)
ggsave("output/smallsample-power.tiff",height=7.5,width=9,unit="in",dpi=800)

### Relative error of small sample corrections (Supp Fig 10)
# Cluster randomisation: GEE-indp vs mixed model(ind)
srd1 = tidy(ss1, stats = "relerror")
srp1 = .nlp2(data = srd1, methodvar = ss1$methodvar, by = ss1$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ylab = "relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-3.5,300))
# Cluster randomisation: GEE-exch vs mixed model(exch)
srd2 = tidy(ss2, stats = "relerror")
srp2 = .nlp2(data = srd2, methodvar = ss2$methodvar, by = ss2$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ylab = "relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-3.5,300))
# Individual  randomisation: GEE-indp vs mixed model(ind)
srd3 = tidy(ss3, stats = "relerror")
srp3 = .nlp2(data = srd3, methodvar = ss3$methodvar, by = ss3$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ylab = "relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-3.5,300))
# Individual  randomisation: GEE-exch vs mixed model(exch)
srd4 = tidy(ss4, stats = "relerror")
srp4 = .nlp2(data = srd4, methodvar = ss4$methodvar, by = ss4$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ylab = "relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-3.5,300))
# Balanced  randomisation: GEE-indp vs mixed model(ind)
srd5 = tidy(ss5, stats = "relerror")
srp5 = .nlp2(data = srd5, methodvar = ss5$methodvar, by = ss5$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("deepskyblue3", "orangered"), mlab=c("GEE independence: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ylab = "relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-3.5,300))
# Balanced  randomisation: GEE-exch vs mixed model(exch)
srd6 = tidy(ss6, stats = "relerror")
srp6 = .nlp2(data = srd6, methodvar = ss6$methodvar, by = ss6$by, stats = "relerror", target = 0, top = top, linewidth=0.4,
            atextsize=6, pcol=c("chartreuse3", "orangered"), mlab=c("GEE exchangeable: Mancl-DeRouen", "Mixed: Kenward-Roger"),
            ylab = "relative error", ltitle="", pheight=c(6.5,3.5), y.lim=c(-3.5,300))
# multipanel plot
plotlist = list(a=row1, b=row2, c=row3, d=col1, e=col2, f=srp1, g=srp2, h=srp3, i=srp4, j=srp5, k=srp6)
wrap_plots(plotlist, design=layout6)
ggsave("output/smallsample-relerror.tiff",height=7.5,width=9,unit="in",dpi=800)
