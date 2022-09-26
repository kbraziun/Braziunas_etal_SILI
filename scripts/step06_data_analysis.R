#####
#
## data analysis
#
#####
  
# load libraries
library(openxlsx)
library(tidyverse)
library(sp)
library(raster)
library(sf)
library(car) # qqPlot
library(plotrix) # std error
library(glmmTMB) # zero-inflated models for simulations
library(DHARMa) # test model assumptions
library(pscl) # zero-inflated regression
library(lmtest) # coeftest
library(sandwich) # vcovCL
library(ggpubr) # plot statistics
library(corrplot) # correlation plots
library(leaps) # model selection
    
####
# 1. load data
####

# plot info
plotinfo.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                        sheet="Plot_info",
                        colNames=TRUE) %>%
  # add column for each unique reburn
  mutate(Fire_combo = paste0(Fire_1,"_",Fire_2)) %>%
  # add column for each unique plot pair
  mutate(Plot_pair = paste0(Fire_combo,"_",Site,"_",Plot)) %>%
  # change various columns to factors
  mutate_at(c("Fire_1","Fire_2","Fire_interval","Fire_combo","Plot_pair"),factor) %>%
  # select only needed columns
  dplyr::select(Plot_code,Fire_year_1,Fire_year_2:TSF,Fire_interval,Plot_pair)

# general plot measurements
plots.in <- read.csv("data/field_plots_2021/cleaned_data/general_plot_meas_estimated_unburned.csv") %>%
  # change various columns to factors
  mutate_at(c("Fire_interval"),factor) %>%
  # select only needed columns
  dplyr::select(Plot_code,Easting:Elev_m,Unburned_dist_est_m) %>%
  # rename unburned dist
  rename(Unburned_dist_m = Unburned_dist_est_m) %>%
  # tack on to plotinfo
  full_join(plotinfo.in, by="Plot_code") %>%
  # add sampling info
  mutate(Sample_year = 2021)

# tallies
counts.in <- read.csv("data/field_plots_2021/cleaned_data/tallies_postfire_cleaned_filled.csv", header=TRUE) %>%
  # drop count, not needed
  dplyr::select(-Count)

# schoennagel data
scho.in <- read.csv("data/field_plots_2021/cleaned_data/Schoennagel_etal2003_cleaned.csv", header=TRUE) %>%
  # add or rename columns to align with new data
  mutate(Plot_code = paste0(SiteName,"_",SiteID),
         Stem_type ="All",
         Fire_interval = ifelse(Trt==0,"Short","Long"),
         Sample_year = 2000) %>%
  rename(Aspect_deg=Aspect,
         Elev_m = Elevm,
         Unburned_dist_m = DisUnburnEdge,
         Plot_pair=SiteName) %>%
  pivot_longer(cols=c(PICO:POTR),names_to="Species",values_to="stems_ha") %>% 
  # select columns to align with plot + count data
  dplyr::select(c(Plot_pair,Plot_code,Easting,Northing,Aspect_deg,Slope_deg,Elev_m,Unburned_dist_m,Fire_year_1,Fire_year_2,FRI,TSF,Fire_interval,Stem_type,Species,stems_ha,Sample_year))

# climate data
terra.in <- read.csv("processed_data/climate/terraclim.csv")
terra.anom <- read.csv("processed_data/climate/terraclim_def_vpd_anom.csv")

# biomass and fuels
fuels.in <- read.csv("processed_data/biomass_fuels/biomass_fuels.csv")

####
# 2. general plot info
####

# helper function for degrees to radians 
deg2rad <- function(deg) {(deg * pi) / (180)}

### combine plot info for 2000 and 2021 field data
plots.all <- scho.in %>%
  # only 1 entry per plot
  group_by(Plot_code) %>%
  slice(1) %>%
  ungroup() %>%
  # drop tally data
  dplyr::select(-c(Stem_type:stems_ha)) %>%
  # combine with 2000 plots
  rbind(plots.in) %>%
  # transform aspect to ne-ness
  mutate(Aspect_NE = cos(deg2rad(45-Aspect_deg)) + 1) %>%
  # add short FRI for LI plots in Schoennagel data
  mutate(FRI_shortonly = ifelse(Fire_interval=="Long",NA,FRI))

plots.all %>%
  group_by(FRI,TSF) %>%
  slice(1)
# 16 unique reburns sampled in 2016 + 10 unique reburns from 2000 dataset (1 repeat) = 26 unique reburns (defined as short x long-interval combinations) sampled
# 66 total plots, 33 plot pairs
# some things only measured in subset, in the 2016 data, fuels are only in 21 pairs, shrubs in 20 pairs

### distance between plot pairs
plots.sf <- st_as_sf(plots.all, coords=c("Easting","Northing")) %>%
  st_set_crs(26912)

# SI plots
plots.short <- plots.sf %>%
  filter(Fire_interval=="Short",Sample_year==2021) %>%
  arrange(Plot_pair)

# LI plots
plots.long <- plots.sf %>%
  filter(Fire_interval=="Long",Sample_year==2021) %>%
  arrange(Plot_pair)

# distance within pairs
out <- st_distance(plots.short,plots.long,by_element = TRUE) %>% as.data.frame() 
summary(out) # mean 398 m, all but 1 pair within 1000 m of each other

# distance btwn pairs, based on SI plot locations
pair.dist <- st_distance(plots.short) %>% as.dist()  %>% as.vector() 
summary(pair.dist) # most plots far apart
pair.dist[pair.dist<500] # a few pairs <500 m apart

### look at breakdowns for FRI x TSF
fri_label <- data.frame(FRI_round = c(5,10,15,20,25),
                        FRI_label = c("05-09",
                                      "10-14",
                                      "15-19",
                                      "20-24",
                                      "25-29"))

tsf_label <- data.frame(TSF_round = c(0,5,10,15,20,25),
                        TSF_label = c("01-04",
                                      "05-09",
                                      "10-14",
                                      "15-19",
                                      "20-24",
                                      "25-29"))

fri_tsf <- plots.all %>%
  # only SI
  filter(!is.na(FRI_shortonly)) %>%
  # round to nearest five
  mutate(FRI_round = floor(FRI*2/10)*10/2) %>%
  mutate(TSF_round = floor(TSF*2/10)*10/2) %>%
  left_join(fri_label, by="FRI_round") %>%
  left_join(tsf_label, by="TSF_round") %>%
  group_by(FRI_label,TSF_label) %>%
  tally()

fri_tsf
fri_tsf %>%
  group_by(FRI_label) %>%
  summarise(n=sum(n)) # at least 3 plots were 5-year window; balanced across decadal windows
fri_tsf %>%
  group_by(TSF_label) %>%
  summarise(n=sum(n)) # majority of plots measured 5-15 yrs postfire

####
# 3. Q1: how do fire interval, climate, and other factors interact to affect post-fire forest recovery?
####

### prep count data
# add scho data to counts
counts.all <- scho.in %>%
  dplyr::select(c("Plot_code","Stem_type","Species","stems_ha")) %>%
  rbind(counts.in)

# add plot information to count data
counts.plot <- counts.all %>%
  left_join(plots.all, by=c("Plot_code"))

### h1a: we expect postfire seedling densities to be lower in short compared to long-interval fire

### first quick look at data
# by stem type, summed across all species (excludes dead)
counts.plot %>%
  # live stems only
  filter(Species !="D", Stem_type !="Snag") %>%
  group_by(Stem_type,Fire_interval,Plot_code) %>%
  summarise_if(is.numeric,sum) %>%
  ggplot(aes(x=Stem_type, y=stems_ha+1, color=factor(Fire_interval))) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw()
# interesting how close the median sapling counts are, maybe because in LI more trees. 

counts.plot %>%
  # live stems only
  filter(Species !="D", Stem_type !="Snag") %>%
  group_by(Stem_type,Fire_interval,Plot_code) %>%
  summarise_at("stems_ha",sum) %>%
  ungroup() %>%
  group_by(Stem_type,Fire_interval) %>%
  summarise_at("stems_ha",c("mean"=mean,"median"=median)) 
# medians much closer than means

# by species, summed across all stem types (includes dead and snags)
counts.plot %>%
  group_by(Species,Fire_interval,Plot_code) %>%
  summarise_if(is.numeric,sum) %>%
  ggplot(aes(x=Species, y=stems_ha+1, color=factor(Fire_interval))) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw() # median is 0 for most species other than pico

### compare live counts, all species; with and without data sampled in 2000
# make sure include or exclude stem type and species as needed
summary(as.factor(counts.plot$Stem_type)) # omit snag from live count
summary(as.factor(counts.plot$Species)) # omit D, RF, PF

counts.live <- counts.plot %>%
  # exclude snags and dead
  filter(Stem_type != "Snag",!Species %in% c("D","RF","PF")) %>% 
  # filter(Stem_type != "Seedling_lt_2") %>% # removing seedlings does not change results, retain seedlings
  group_by(Plot_pair,Plot_code,Fire_interval,Sample_year) %>%
  summarise_at("stems_ha",sum) 

# check distribution of data
counts.live %>%
  ggplot(aes(x=stems_ha)) +
  facet_wrap(~Fire_interval) +
  geom_histogram() +
  theme_bw() # not normal
summary(counts.live)

# summary statistics
counts.live %>%
  group_by(Fire_interval) %>%
  summarise(across(stems_ha, list(min=min, max=max, mean=mean,se=std.error,median=median)))

counts.live %>%
  group_by(Fire_interval,Sample_year) %>%
  summarise(across(stems_ha, list(min=min, max=max, mean=mean,se=std.error,median=median)))

# using paired wilcoxon signed rank test: count data, non-parametric
# 1-sided, because expect higher density of stems in long v. short

# prep pairs
counts.test <- counts.live %>%
  ungroup() %>%
  dplyr::select(-Plot_code) %>%
  pivot_wider(names_from="Fire_interval",values_from="stems_ha")

# all data
wilcox.test(counts.test$Long, counts.test$Short, 
            alternative="greater", paired=TRUE) # p < 0.001, not exact b/c ties
# 2021 data only
wilcox.test(counts.test[counts.test$Sample_year==2021,]$Long, counts.test[counts.test$Sample_year==2021,]$Short,  
            alternative="greater", paired=TRUE) # p < 0.01
# 2000 data only
wilcox.test(counts.test[counts.test$Sample_year==2000,]$Long, counts.test[counts.test$Sample_year==2000,]$Short,  
            alternative="greater", paired=TRUE) # p < 0.01

### by species
### first identify appropriate statistical approach

# live counts by species
counts.sp <- counts.plot %>%
  # can ck for consistency in each sample year
  # filter(Sample_year=="2021") %>%
  # exclude snags and dead
  filter(Stem_type != "Snag",!Species %in% c("D","RF","PF")) %>%
  # add P/A
  mutate(pres = ifelse(stems_ha>0,1,0)) %>%
  group_by(Plot_pair,Plot_code,Fire_interval,Sample_year,Species) %>%
  # sum across stem type, code for 1 if present
  summarise(stems_ha = sum(stems_ha), pres = max(pres)) 

# summary statistics
counts.sp %>%
  group_by(Species,Fire_interval) %>%
  summarise(across(stems_ha, list(min=min, max=max, mean=mean,se=std.error,median=median)))

# check distribution of data, pico only
counts.sp %>%
  filter(Species=="PICO") %>%
  ggplot(aes(x=stems_ha)) +
  facet_wrap(~Fire_interval) +
  geom_histogram() +
  theme_bw() # not normal
# PICO present in all plots, assess with wilcoxon signed rank test

# check distribution of data, other species
counts.sp %>%
  filter(Species!="PICO") %>%
  ggplot(aes(x=stems_ha)) +
  facet_wrap(Species~Fire_interval, scales="free") +
  geom_histogram() +
  theme_bw() # not normal

counts.sp %>%
  ungroup() %>%
  group_by(Species,Fire_interval) %>%
  summarise(mean=mean(stems_ha), se = std.error(stems_ha), min = min(stems_ha),max=max(stems_ha),var=var(stems_ha), prop_zeros=(33-sum(pres))/33, pres = 1-prop_zeros) # high proportion of 0s, high variance

# identify distribution to best represent data: try ZI poisson and binomial
# data must be integer, round to nearest
counts.mod <- counts.sp %>%
  mutate(stems_ha=round(stems_ha))

# fit model, use glmmTMB with dharma to assess model assumptions
spec.in <- "PSME" # ABLA, PIAL, PIEN, POTR, PSME

# fit <- glmmTMB(stems_ha ~ 1, data = counts.mod[counts.mod$Species==spec.in,], family="gaussian") # default
# fit <- glmmTMB(stems_ha ~ 1, data = counts.mod[counts.mod$Species==spec.in,], family="poisson",ziformula=~1)
fit <- glmmTMB(stems_ha ~ 1, data = counts.mod[counts.mod$Species==spec.in,], family="nbinom1",ziformula=~1)

# simulate data
sims <- simulate(fit, nsim=100)

sims %>%
  as_tibble() %>%
  gather() %>%
  ggplot() +
  geom_line(stat = "count", aes(x = value, group=key), alpha = 0.1, col="#d73027") +
  geom_density(stat="count", data = counts.mod[counts.mod$Species==spec.in,], aes(x = stems_ha), col = "black", size = 1) +
  theme_bw() +
  scale_y_log10() +
  labs(x = "Stems/ha",
       y = "Count")

# summary simulation statistics
simstats <- sims %>%
  pivot_longer(everything()) %>%
  mutate(zero = ifelse(value==0,1,0)) %>%
  group_by(name) %>%
  summarise(min = min(value),
            p01 = quantile(value,probs=0.01),
            p05 = quantile(value,probs=0.05),
            p25 = quantile(value, probs=0.25),
            p50 = quantile(value,probs=0.5),
            p75 = quantile(value,probs=0.75),
            p99 = quantile(value, probs=0.99),
            max = max(value),
            mean = mean(value),
            sd = sd(value),
            zeros=sum(zero)) %>%
  mutate(name="sims")

covstats <- counts.mod[counts.mod$Species==spec.in,] %>%
  ungroup() %>%
  dplyr::select(-c(Plot_pair:Species)) %>%
  mutate(zero = ifelse(pres==0,1,0)) %>%
  summarise(min = min(stems_ha),
            p01 = quantile(stems_ha,probs=0.01),
            p05 = quantile(stems_ha,probs=0.05),
            p25 = quantile(stems_ha, probs=0.25),
            p50 = quantile(stems_ha,probs=0.5),
            p75 = quantile(stems_ha,probs=0.75),
            p99 = quantile(stems_ha, probs=0.99),
            max = max(stems_ha),
            mean = mean(stems_ha),
            sd = sd(stems_ha),
            zeros=sum(zero)) %>%
  mutate(name="data")

covstats %>%
  rbind(simstats) %>%
  pivot_longer(-name, names_to="stats") %>%
  ggplot(aes(x=name,y=value, color=name)) +
  facet_wrap(~stats, scales="free") +
  # is actual value captured by at least 50% of the simulations?
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median) +
  scale_color_manual(values=c("black","red")) +
  theme_bw()

# dharma tests
resids <- simulateResiduals(fit, n=100)
testResiduals(resids)
testZeroInflation(resids)

# gaussian: poorly aligns with sim quantiles, 0s; fails some dharma cks, zero inflation test
# zi-poisson: poorly aligns with max, sd, some quantiles of sims; fails some dharma cks
# zi-neg bin: better aligns with quantile distributions, mean, sd of sims; passes all dharma cks

### test for long v. short differences by species

# pico: paired wilcoxon
counts.pico <- counts.sp %>%
  filter(Species=="PICO") %>%
  ungroup() %>%
  dplyr::select(-Plot_code) %>%
  pivot_wider(names_from="Fire_interval", values_from="stems_ha")
wilcox.test(counts.pico$Long,counts.pico$Short, 
            alternative="two.sided", paired=TRUE) # p < 0.001
# sampled in 2021: p < 0.01
# sampled in 2000: p < 0.001

# other species: zi neg binomial model
# generate statistics for matched data, zero-inflated models
# code modified from: https://stats.stackexchange.com/questions/499404/zero-inflated-poisson-models-for-matched-data

# use integer data
# functionalize
zinb.test <- function(spec.in) {
  # subset to species of interest
  counts.nb <- counts.mod %>%
    filter(Species==spec.in)
  
  # fit zi-neg binomial model
  fit.sp <- zeroinfl(stems_ha~Fire_interval, data=counts.nb, dist="negbin")
  
  # test matched data
  return(coeftest(fit.sp, vcov. = vcovCL, cluster=~Plot_pair))
}

zinb.test("ABLA")
zinb.test("PIAL")
zinb.test("PIEN")
zinb.test("POTR")
zinb.test("PSME")
# sampled in 2021: consistent with full dataset, density differences remain significant with some slight changes in p-values (most similar strength)
# sampled in 2000: insufficient data for stat test

### write out data
# full stem counts
counts.out <- counts.live %>%
  # add ID column and presence to full count
  mutate(Species="Total", pres=ifelse(stems_ha>0,1,0)) %>%
  # combine with species
  rbind(counts.sp) %>%
  # reorder
  dplyr::select(c(Plot_pair,Plot_code,Fire_interval,Sample_year,Species,stems_ha,pres)) %>%
  arrange(Plot_pair,Fire_interval,Species)

write.csv(counts.out, "analysis/q1_postfire_regen/postfire_regen_live_counts.csv",row.names=FALSE)

# summary statistics
counts.summ <- counts.out %>%
  ungroup() %>%
  group_by(Fire_interval,Species) %>%
  summarise(across(stems_ha, list(min=min,max=max,mean=mean,se=std.error,median=median)), pres_prop=mean(pres))

write.csv(counts.summ, "analysis/q1_postfire_regen/postfire_regen_summary_stats.csv",row.names=FALSE)

### h1b: we expect a synergistic interaction between shorter FRI and drier postfire climate to amplify differences in regen between SI and LI fire

### average plot characteristics for each pair
pair.clim <- plots.all %>%
  left_join(terra.in, by=c("Plot_code","Fire_year_2")) %>%
  left_join(terra.anom, by=c("Plot_code","Fire_year_2","ann_def_mm_norm","summer_vpd_kpa_norm")) %>%
  # remove unneeded columns
  dplyr::select(-c(Plot_code:Aspect_deg,Unburned_dist_m:Fire_year_1,FRI,Fire_interval,Sample_year)) %>%
  # average by plot pair
  group_by(Plot_pair) %>%
  summarise(FRI_shortonly = max(FRI_shortonly,na.rm=TRUE),
            across(c(Slope_deg:Aspect_NE,ann_def_mm_pf1:anom_summer_vpd_zscore),~mean(.)))

### for this analysis, focus on counts of live conifer regen only
counts.con <- counts.sp %>%
  filter(Species %in% c("ABLA","PIAL","PICO","PIEN","PSME")) %>%
  group_by(Plot_pair,Plot_code,Fire_interval) %>%
  summarise(conifer_stems_ha = sum(stems_ha)) 

# calculate difference
con.diff <- counts.con %>%
  ungroup() %>%
  dplyr::select(-Plot_code) %>%
  pivot_wider(values_from="conifer_stems_ha", names_from="Fire_interval") %>%
  # absolute and relative difference
  mutate(pair_diff = Long-Short,
         rel_diff = pair_diff/Long) %>%
  # join with avg site and climate
  left_join(pair.clim, by=c("Plot_pair"))
         
### visualize data, not normal
hist(con.diff$pair_diff) 
hist(con.diff$rel_diff) 

con.diff %>%
  ggplot(aes(y=pair_diff)) +
  geom_boxplot(fill="gray") +
  scale_y_continuous(labels=scales::comma) +
  ylab("Difference in conifer density (stems/ha)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) 

con.diff %>%
  ggplot(aes(y=rel_diff)) +
  geom_boxplot(fill="gray") +
  scale_y_continuous(labels=scales::comma) +
  ylab("Difference in conifer density (stems/ha)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) 

### which factors associated with amplified differences between SI and LI fire
# first: site characteristics, FRI, TSF
con.diff %>%
  # plot as cube root for better visualization
  mutate(pair_diff_cuberoot = sign(pair_diff)*abs(pair_diff)^(1/3)) %>% 
  pivot_longer(c(FRI_shortonly:Aspect_NE)) %>%
  ggplot(aes(y=pair_diff_cuberoot,x=value)) +
  facet_wrap(~name, scales="free_x") +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",") +
  geom_point() +
  theme_bw()

con.diff %>%
  pivot_longer(c(FRI_shortonly:Aspect_NE)) %>%
  ggplot(aes(y=rel_diff,x=value)) +
  facet_wrap(~name, scales="free_x") +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",", label.y=-2) +
  geom_point() +
  theme_bw()
# consistent: greater abs and rel differences weakly correlated with more NE (moister) aspects, moderately correlated with lower elevations; year of most recent fire and TSF only very weakly correlated
# not consistent: greater abs differences associated with longer short FRIs (but no corr with rel diff); greater rel diff weakly assoc with shallower slopes (but very weak corr with abs diff)

# second: climate predictors
# post-fire climate
con.diff %>%
  # plot as cube root for better visualization
  mutate(pair_diff_cuberoot = sign(pair_diff)*abs(pair_diff)^(1/3)) %>% 
  pivot_longer(c(ann_def_mm_pf1:grow_prec_mm_pf3)) %>%
  ggplot(aes(y=pair_diff_cuberoot,x=value)) +
  facet_wrap(~name, scales="free_x") +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",") +
  geom_point() +
  theme_bw()

# climate norms
con.diff %>%
  # plot as cube root for better visualization
  mutate(pair_diff_cuberoot = sign(pair_diff)*abs(pair_diff)^(1/3)) %>% 
  pivot_longer(c(ann_def_mm_norm:grow_prec_mm_norm)) %>%
  ggplot(aes(y=pair_diff_cuberoot,x=value)) +
  facet_wrap(~name, scales="free_x") +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",") +
  geom_point() +
  theme_bw()

# post-fire anomaly
con.diff %>%
  # plot as cube root for better visualization
  mutate(pair_diff_cuberoot = sign(pair_diff)*abs(pair_diff)^(1/3)) %>% 
  pivot_longer(c(anom_def_mm:anom_summer_vpd_zscore)) %>%
  ggplot(aes(y=pair_diff_cuberoot,x=value)) +
  facet_wrap(~name, scales="free_x") +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",") +
  geom_point() +
  theme_bw()
# strongest correlations with climate water def norm, then summer vpd, max temp, grow temp norms
# summer vpd anomaly increases differences; def anom only weakly associated

# relative difference
# post-fire climate
con.diff %>%
  pivot_longer(c(ann_def_mm_pf1:grow_prec_mm_pf3)) %>%
  ggplot(aes(y=rel_diff,x=value)) +
  facet_wrap(~name, scales="free_x") +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",") +
  geom_point() +
  theme_bw()

# climate norms
con.diff %>%
  pivot_longer(c(ann_def_mm_norm:grow_prec_mm_norm)) %>%
  ggplot(aes(y=rel_diff,x=value)) +
  facet_wrap(~name, scales="free_x") +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",") +
  geom_point() +
  theme_bw()

# post-fire anomaly
con.diff %>%
  pivot_longer(c(anom_def_mm:anom_summer_vpd_zscore)) %>%
  ggplot(aes(y=rel_diff,x=value)) +
  facet_wrap(~name, scales="free_x") +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",") +
  geom_point() +
  theme_bw()
# consisent with abs diff, except now assoc with inc diff is slightly inc with deficit anomaly v. summer vpd anomaly

### some SI plots increase in density relative to LI plots. where does this occur?
con.diff %>%
  mutate(diff_sign = ifelse(pair_diff<0,"neg","pos")) %>%
  group_by(diff_sign) %>%
  tally() # 5 plot pairs

con.diff %>%
  mutate(diff_sign = ifelse(pair_diff<0,"SI_higher","SI_lower")) %>%
  dplyr::select(c(diff_sign,pair_diff:Aspect_NE,ann_def_mm_norm,summer_vpd_kpa_norm,anom_def_zscore,anom_summer_vpd_zscore)) %>%
  pivot_longer(-diff_sign) %>%
  ggplot(aes(x=diff_sign,y=value, fill=diff_sign)) +
  facet_wrap(~name, scales="free") +
  stat_summary(position=position_dodge(width=0.25),geom="bar", fun.data="mean_cl_normal") +
  stat_summary(position=position_dodge(width=0.25),geom="errorbar", fun.data="mean_cl_normal",width=0.25) +
  theme_bw()
# too few increases to make concrete conclusions, but generally if regen was higher after SI v. LI in a pair, the magnitude of this difference was small relative to plots that decreased in density ("pair_diff"). CI also overlaps with 0.
# higher regen following SI v. LI fire only occurred when climate was cooler-wetter than normal (anom_def_zscore)

# write out
head(con.diff)
# subset with climate water def and summer vpd only
con.diff %>%
  dplyr::select(c(Plot_pair,pair_diff,rel_diff,ann_def_mm_norm,summer_vpd_kpa_norm,anom_def_zscore,anom_summer_vpd_zscore)) %>%
  write.csv("analysis/q1_postfire_regen/pair_conifer_diff_climate.csv",row.names=FALSE)

# full set
write.csv(con.diff, "analysis/q1_postfire_regen/pair_conifer_diff_full.csv",row.names=FALSE)

### h1c: we expect other factors to mediate the importance of SI and LI fire

### this section will not run, processed inputs are reloaded later
# dem and gye outline
dem <- raster("data/nr_gye/nr_dem_utm_30m.tif")
gye <- st_read("data/nr_gye/GYE_outline.shp", crs = crs(dem)) # same crs, but updating proj4string

### calculate other predictors: topographic indexes
dem.gye <- crop(dem,gye) %>% mask(gye)

# calculate topographic position index and join with plots
tpi.gye <- terrain(dem.gye, opt="tpi", neighbors=8)

plots.tpi <- plots.sf %>%
  st_transform(crs=crs(tpi.gye)) %>%
  # extract values to points
  mutate(tpi = extract(tpi.gye, .)) %>%
  as.data.frame() %>%
  dplyr::select(c(Plot_code,tpi))

# calculate heat load index
# mccune & keon
# need latitude
plots.latlon <- plots.sf %>%
  st_transform(crs=4326) %>%
  st_coordinates()

plots.hli <- plots.sf %>%
  as.data.frame() %>%
  cbind(plots.latlon)

plots.hli$Aspect_rad <- deg2rad(plots.hli$Aspect_deg)
plots.hli$Aspect_fold <- pi - abs(plots.hli$Aspect_rad - pi)
plots.hli$Slope_rad <- deg2rad(plots.hli$Slope_deg)
plots.hli$Lat_rad <- deg2rad(plots.hli$Y)
plots.hli$hli <- exp(-1.467 +
                     1.582 * cos(plots.hli$Lat_rad) * cos(plots.hli$Slope_rad) -
                     1.5 * cos(plots.hli$Aspect_fold) * sin(plots.hli$Slope_rad) * sin(plots.hli$Lat_rad) -
                     0.262 * sin(plots.hli$Lat_rad) * sin(plots.hli$Slope_rad) +
                     0.607 * sin(plots.hli$Aspect_fold) * sin(plots.hli$Slope_rad))

# join topo data
plots.dem <- plots.tpi %>%
  left_join(plots.hli, by="Plot_code") %>%
  dplyr::select(c(Plot_code,tpi,hli))

### join with conifer counts and plot data
head(counts.con)

con.all <- counts.con %>%
  # join with site data
  left_join(plots.all, by=c("Plot_pair","Plot_code","Fire_interval")) %>%
  left_join(plots.dem, by="Plot_code") %>%
  # join with climate data
  left_join(terra.anom, by=c("Plot_code","Fire_year_2")) %>%
  # classify elevation as proxy for high v. low serotiny
  # 2350 halfway between tinker and schoennagel, retains all pairs in same set
  mutate(Elev_high = ifelse(Elev_m<2350,0,1)) %>%
  # include only potential predictors
  dplyr::select(c(Plot_pair:conifer_stems_ha,TSF,Elev_high,Elev_m,Unburned_dist_m,Elev_m,tpi,hli,ann_def_mm_norm, anom_summer_vpd_zscore)) 

# write out
write.csv(con.all, "analysis/q1_postfire_regen/conifer_regen_predictors.csv",row.names=FALSE)
### end code that relies on external DEM

# reload predictors
con.all <- read.csv("analysis/q1_postfire_regen/conifer_regen_predictors.csv")

### identify and exclude highly correlated predictors
corr.mat <- con.all %>%
  ungroup() %>%
  dplyr::select(c(TSF:anom_summer_vpd_zscore)) %>% cor() 

corrplot(corr.mat, diag=FALSE, type="upper")

abs(corr.mat)
# retain: TSF, Elev_high, Unburned_dist_m, tpi, hli, ann_def_mm_norm, anom_summer_vpd_zscore
# exclude due to collinearity: Elev_m

### scale predictors except for categorical and zscore
con.scale <- con.all %>%
  as.data.frame() %>%
  mutate(across(c(TSF,Unburned_dist_m:ann_def_mm_norm), ~as.numeric(scale(.))))

### fit model and perform model selection
# first assess indep variables
full.mod <- lm(log10(conifer_stems_ha)~(TSF+Elev_high+Unburned_dist_m+tpi+hli+ann_def_mm_norm+anom_summer_vpd_zscore)*Fire_interval, data=con.scale)

# ck assumptions, suggest log transformation
plot(full.mod, 1)
qqPlot(full.mod)
plot(full.mod, 5)
boxCox(full.mod)

summary(full.mod)

# model selection, exhaustive with BIC
model.subsets <- regsubsets(log10(conifer_stems_ha)~(TSF+Elev_high+Unburned_dist_m+tpi+hli+ann_def_mm_norm+anom_summer_vpd_zscore)*Fire_interval, data=con.scale, nvmax = 5, nbest=5,
                         method = "exhaustive")

# use BIC for selection
plot(model.subsets)

out <- data.frame(bic = summary(model.subsets)$bic) %>%
  mutate(model_n = row_number()) %>%
  arrange(bic)
                  
head(out) # 4 models within 2 BIC

coef(model.subsets,11)
coef(model.subsets,12)
coef(model.subsets,6)
coef(model.subsets,16)

# fit top models, recheck assumptions
mod1 <- lm(log10(conifer_stems_ha)~ann_def_mm_norm+Fire_interval+Unburned_dist_m:Fire_interval, data=con.scale)
plot(mod1, 1)
qqPlot(mod1)
plot(mod1, 5)

mod2 <- lm(log10(conifer_stems_ha)~ann_def_mm_norm+Fire_interval+ann_def_mm_norm:Fire_interval, data=con.scale)
plot(mod2, 1)
qqPlot(mod2)
plot(mod2, 5)

mod3 <- lm(log10(conifer_stems_ha)~ann_def_mm_norm+Fire_interval, data=con.scale)
plot(mod3, 1)
qqPlot(mod3)
plot(mod3, 5)

mod4 <- lm(log10(conifer_stems_ha)~ann_def_mm_norm+Fire_interval+Unburned_dist_m:Fire_interval+ann_def_mm_norm:Fire_interval, data=con.scale)
plot(mod4, 1)
qqPlot(mod4)
plot(mod4, 5)

### some alternative model predictor formulations
# does including TSF or Elevation flag change predictor significance? no
mod1.tsf <- lm(log10(conifer_stems_ha)~TSF+ann_def_mm_norm+Fire_interval+Unburned_dist_m:Fire_interval, data=con.scale)
summary(mod1)
summary(mod1.tsf)
anova(mod1, mod1.tsf) # likelihood ratio test

mod1.elev <- lm(log10(conifer_stems_ha)~Elev_high+ann_def_mm_norm+Fire_interval+Unburned_dist_m:Fire_interval, data=con.scale)
summary(mod1)
summary(mod1.elev)
anova(mod1, mod1.elev) # likelihood ratio test

mod1.tsfElev <- lm(log10(conifer_stems_ha)~ann_def_mm_norm+Fire_interval+Unburned_dist_m:Fire_interval+TSF:Elev_high, data=con.scale)
summary(mod1)
summary(mod1.tsfElev)
anova(mod1, mod1.tsfElev) # likelihood ratio test

# what if climate is excluded?
model.noclim <- regsubsets(log10(conifer_stems_ha)~(TSF+Elev_high+Unburned_dist_m+tpi+hli)*Fire_interval, data=con.scale, nvmax = 5, nbest=5,
                            method = "exhaustive")

# use BIC for selection
plot(model.noclim)

data.frame(bic = summary(model.noclim)$bic) %>%
  mutate(model_n = row_number()) %>%
  arrange(bic) %>% head()

coef(model.noclim, 6)
coef(model.noclim, 1)
coef(model.noclim, 11)
# Elev_high takes place of climate, Unburned x SI fire interaction remains in top models

### results
mod_results <- function(mod_nbr, mod_name) {
  return(data.frame(mod_n = mod_nbr,
                    bic = out[mod_nbr,1],
                    adjrsq = summary(mod_name)$adj.r.squared) %>%
           cbind(data.frame(pred = names(mod_name$coefficients), 
                            coef = mod_name$coefficients,
                            se = summary(mod_name)$coefficients[,"Std. Error"],
                            t = summary(mod_name)$coefficients[,"t value"],
                            p = summary(mod_name)$coefficients[,"Pr(>|t|)"])))
}

mod.out <- mod_results(1,mod1) %>%
  rbind(mod_results(2,mod2)) %>%
  rbind(mod_results(3,mod3)) %>%
  rbind(mod_results(4,mod4))

# write out
write.csv(mod.out, "analysis/q1_postfire_regen/conifer_regen_models.csv", row.names=FALSE)

####
# 4. Q2: How do forest biomass and fuels vary following short v. long-interval fire?
####

head(fuels.in)

# add total live tree, total live aboveground, total dead woody
fuels.tots <- fuels.in %>%
  # add 0s for shrub NAs
  mutate(shrub_zeros = ifelse(is.na(shrub_total_Mg_ha),0,shrub_total_Mg_ha),
         tree_total_Mg_ha = conifer_total_Mg_ha + potr_total_Mg_ha,
         live_ag_total_Mg_ha = tree_total_Mg_ha + saps_pico_total_Mg_ha + shrub_zeros,
         dead_woody_total_Mg_ha = snag_total_Mg_ha + bm_1h_Mg_ha + bm_10h_Mg_ha + bm_100h_Mg_ha + bm_1000h_sound_Mg_ha + bm_1000h_rotten_Mg_ha)
  
# summarise mean, SE, min and max
fuels.summ <- fuels.tots %>%
  pivot_longer(cols=c(cfl_Mg_ha,cbd_kg_m3,saps_pico_total_Mg_ha:bm_1000h_rotten_Mg_ha,tree_total_Mg_ha:dead_woody_total_Mg_ha)) %>%
  group_by(Fire_interval,name) %>%
  summarise(median = median(value,na.rm=TRUE),
            mean = mean(value,na.rm=TRUE),
            se = std.error(value, na.rm=TRUE),
            min = min(value,na.rm=TRUE),
            max = max(value,na.rm=TRUE),
            var = var(value,na.rm=TRUE))

# write out
write.csv(fuels.summ, "analysis/q2_fuels/fri_fuels_summary.csv", row.names=FALSE)

### h2a: we expect lower fuel loads, one sided live AG total and dead woody total
# then two sided for individual fuel pools

head(fuels.tots)

# total pools
fuels.tots %>%
  pivot_longer(cols=c(cfl_Mg_ha,cbd_kg_m3,saps_pico_total_Mg_ha:bm_1000h_rotten_Mg_ha,tree_total_Mg_ha:dead_woody_total_Mg_ha)) %>%
  ggplot(aes(x=name,y=value, color=Fire_interval)) +
  facet_wrap(~name,scales="free") +
  geom_boxplot() +
  theme_bw()

# prep for testing
fuels.test <- fuels.tots %>%
  # add plot pair
  left_join(plots.in, by=c("Plot_code","Fire_interval")) %>%
  dplyr::select(-c(Plot_code,conifer_total_Mg_ha,potr_total_Mg_ha,shrub_zeros,Easting:TSF,Sample_year)) %>%
  # get plot pairs side by side
  pivot_longer(c("cfl_Mg_ha":"dead_woody_total_Mg_ha")) %>%
  pivot_wider(names_from="Fire_interval",values_from="value") %>%
  # omit NAs
  filter(!is.na(Long)) %>%
  # calculate difference
  mutate(pair_diff = Long-Short)

### total live and dead woody

# total live aboveground
hist(fuels.test[fuels.test$name=="live_ag_total_Mg_ha",]$pair_diff) # a bit skewed
hist(log10(fuels.test[fuels.test$name=="live_ag_total_Mg_ha",]$Long)-log10(fuels.test[fuels.test$name=="live_ag_total_Mg_ha",]$Short)) # much better
qqPlot(log10(fuels.test[fuels.test$name=="live_ag_total_Mg_ha",]$Long)-log10(fuels.test[fuels.test$name=="live_ag_total_Mg_ha",]$Short)) # ok

# t test pref
t.test(log10(fuels.test[fuels.test$name=="live_ag_total_Mg_ha",]$Long),log10(fuels.test[fuels.test$name=="live_ag_total_Mg_ha",]$Short),
       alternative="greater", paired=TRUE) # p < 0.001
# cross ck with wilcox, if not agree reason to pause
wilcox.test(fuels.test[fuels.test$name=="live_ag_total_Mg_ha",]$Long,fuels.test[fuels.test$name=="live_ag_total_Mg_ha",]$Short, 
       alternative="greater", paired=TRUE) # p = 0.001

# total dead woody
hist(fuels.test[fuels.test$name=="dead_woody_total_Mg_ha",]$pair_diff)
qqPlot(fuels.test[fuels.test$name=="dead_woody_total_Mg_ha",]$pair_diff) # mostly ok

# t test pref
t.test(fuels.test[fuels.test$name=="dead_woody_total_Mg_ha",]$Long,fuels.test[fuels.test$name=="dead_woody_total_Mg_ha",]$Short, 
       alternative="greater", paired=TRUE) # p < 0.001
# cross ck with wilcox, if not agree reason to pause
wilcox.test(fuels.test[fuels.test$name=="dead_woody_total_Mg_ha",]$Long,fuels.test[fuels.test$name=="dead_woody_total_Mg_ha",]$Short, 
            alternative="greater", paired=TRUE) # p < 0.001

### individual fuel pools

### check for poorly represented biomass/fuel pools
# do not expect live trees to be present until sufficient time since fire
fuels.tots %>%
  left_join(plots.in, by=c("Plot_code","Fire_interval")) %>%
  ggplot(aes(x=TSF,y=tree_total_Mg_ha)) +
  geom_point() +
  theme_bw() # at least 10 years

fuels.pres <- fuels.tots %>%
  pivot_longer(cols=c(cfl_Mg_ha,cbd_kg_m3,saps_pico_total_Mg_ha:bm_1000h_rotten_Mg_ha,tree_total_Mg_ha:dead_woody_total_Mg_ha)) %>%
  mutate(pres = ifelse(value>0,1,0)) %>%
  group_by(name) %>%
  summarise(pres=mean(pres,na.rm=TRUE))
# do not test snag 1h and snag 10h, absent in over half the plots; live tree biomass and canopy fuels, absent in >40% of plots and confounded with TSF

### snags
# 100h snags
hist(fuels.test[fuels.test$name=="snag_100h_Mg_ha",]$pair_diff) # skewed
hist((fuels.test[fuels.test$name=="snag_100h_Mg_ha",]$Long)^(1/3)-(fuels.test[fuels.test$name=="snag_100h_Mg_ha",]$Short)^(1/3)) # better
qqPlot((fuels.test[fuels.test$name=="snag_100h_Mg_ha",]$Long)^(1/3)-(fuels.test[fuels.test$name=="snag_100h_Mg_ha",]$Short)^(1/3)) # trend and skew

# wilcox
wilcox.test((fuels.test[fuels.test$name=="snag_100h_Mg_ha",]$Long),(fuels.test[fuels.test$name=="snag_100h_Mg_ha",]$Short), 
            alternative="two.sided", paired=TRUE) # p = 0.11

# 1000h snags
hist(fuels.test[fuels.test$name=="snag_1000h_Mg_ha",]$pair_diff) # skewed
hist((fuels.test[fuels.test$name=="snag_1000h_Mg_ha",]$Long)^(1/3)-(fuels.test[fuels.test$name=="snag_1000h_Mg_ha",]$Short)^(1/3)) # better
qqPlot((fuels.test[fuels.test$name=="snag_1000h_Mg_ha",]$Long)^(1/3)-(fuels.test[fuels.test$name=="snag_1000h_Mg_ha",]$Short)^(1/3)) # ok, a little off at the tail

# t test pref
t.test((fuels.test[fuels.test$name=="snag_1000h_Mg_ha",]$Long)^(1/3),(fuels.test[fuels.test$name=="snag_1000h_Mg_ha",]$Short)^(1/3),
       alternative="two.sided", paired=TRUE) # p < 0.001
# cross ck with wilcox, if not agree reason to pause
wilcox.test(fuels.test[fuels.test$name=="snag_1000h_Mg_ha",]$Long,fuels.test[fuels.test$name=="snag_1000h_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p < 0.001

# total snag
hist(fuels.test[fuels.test$name=="snag_total_Mg_ha",]$pair_diff) # skewed
hist(fuels.test[fuels.test$name=="snag_total_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="snag_total_Mg_ha",]$Short^(1/2)) # better
qqPlot(fuels.test[fuels.test$name=="snag_total_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="snag_total_Mg_ha",]$Short^(1/2)) # ok

# t test pref
t.test(fuels.test[fuels.test$name=="snag_total_Mg_ha",]$Long^(1/2),fuels.test[fuels.test$name=="snag_total_Mg_ha",]$Short^(1/2),
       alternative="two.sided", paired=TRUE) # p < 0.001
# cross ck with wilcox, if not agree reason to pause
wilcox.test(fuels.test[fuels.test$name=="snag_total_Mg_ha",]$Long,fuels.test[fuels.test$name=="snag_total_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p < 0.001

### sapling AGB
hist(fuels.test[fuels.test$name=="saps_pico_total_Mg_ha",]$pair_diff) # skewed
hist(log10(fuels.test[fuels.test$name=="saps_pico_total_Mg_ha",]$Long)-log10(fuels.test[fuels.test$name=="saps_pico_total_Mg_ha",]$Short)) # better
qqPlot(log10(fuels.test[fuels.test$name=="saps_pico_total_Mg_ha",]$Long)-log10(fuels.test[fuels.test$name=="saps_pico_total_Mg_ha",]$Short)) # no

# wilcox
wilcox.test(fuels.test[fuels.test$name=="saps_pico_total_Mg_ha",]$Long,fuels.test[fuels.test$name=="saps_pico_total_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p = 0.003

### shrub, litter, duff, dead woody have fewer plots
hist(fuels.test[fuels.test$name=="shrub_total_Mg_ha",]$pair_diff) # skewed
hist((fuels.test[fuels.test$name=="shrub_total_Mg_ha",]$Long)^(1/3)-(fuels.test[fuels.test$name=="shrub_total_Mg_ha",]$Short)^(1/3)) # good
qqPlot((fuels.test[fuels.test$name=="shrub_total_Mg_ha",]$Long)^(1/3)-(fuels.test[fuels.test$name=="shrub_total_Mg_ha",]$Short)^(1/3)) # borderline

# t test pref
t.test(fuels.test[fuels.test$name=="shrub_total_Mg_ha",]$Long^(1/3),fuels.test[fuels.test$name=="shrub_total_Mg_ha",]$Short^(1/3),
       alternative="two.sided", paired=TRUE) # p = 0.49
# cross ck with wilcox, if not agree reason to pause
wilcox.test(fuels.test[fuels.test$name=="shrub_total_Mg_ha",]$Long,fuels.test[fuels.test$name=="shrub_total_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p = 0.60

### litter
hist(fuels.test[fuels.test$name=="litter_Mg_ha",]$pair_diff) # skewed
hist(fuels.test[fuels.test$name=="litter_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="litter_Mg_ha",]$Short^(1/2)) # flat, not good
qqPlot(fuels.test[fuels.test$name=="litter_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="litter_Mg_ha",]$Short^(1/2)) # borderline

# wilcox
wilcox.test(fuels.test[fuels.test$name=="litter_Mg_ha",]$Long,fuels.test[fuels.test$name=="litter_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p=0.07

### duff
hist(fuels.test[fuels.test$name=="duff_Mg_ha",]$pair_diff) # not bad
hist(fuels.test[fuels.test$name=="duff_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="duff_Mg_ha",]$Short^(1/2)) # somewhat better
qqPlot(fuels.test[fuels.test$name=="duff_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="duff_Mg_ha",]$Short^(1/2)) # no

# wilcox
wilcox.test(fuels.test[fuels.test$name=="duff_Mg_ha",]$Long,fuels.test[fuels.test$name=="duff_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p = 0.12

### dwd 1h
hist(fuels.test[fuels.test$name=="bm_1h_Mg_ha",]$pair_diff) # skewed
hist(fuels.test[fuels.test$name=="bm_1h_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="bm_1h_Mg_ha",]$Short^(1/2)) # better
qqPlot(fuels.test[fuels.test$name=="bm_1h_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="bm_1h_Mg_ha",]$Short^(1/2)) # no

# wilcox
wilcox.test(bm_1h_Mg_ha~Fire_interval,data=fuels.tots, 
            alternative="two.sided", paired=TRUE) # p = 0.11

### dwd 10h
hist(fuels.test[fuels.test$name=="bm_10h_Mg_ha",]$pair_diff) # ok
qqPlot(fuels.test[fuels.test$name=="bm_10h_Mg_ha",]$pair_diff) # ok

# t test pref
t.test(fuels.test[fuels.test$name=="bm_10h_Mg_ha",]$Long,fuels.test[fuels.test$name=="bm_10h_Mg_ha",]$Short,
       alternative="two.sided", paired=TRUE) # p = 0.44
# cross ck with wilcox, if not agree reason to pause
wilcox.test(fuels.test[fuels.test$name=="bm_10h_Mg_ha",]$Long,fuels.test[fuels.test$name=="bm_10h_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p = 0.68

### dwd 100h
hist(fuels.test[fuels.test$name=="bm_100h_Mg_ha",]$pair_diff) # a little skewed
hist(fuels.test[fuels.test$name=="bm_100h_Mg_ha",]$Long^(1/3)-fuels.test[fuels.test$name=="bm_100h_Mg_ha",]$Short^(1/3)) # not better
qqPlot(fuels.test[fuels.test$name=="bm_100h_Mg_ha",]$pair_diff) # no
qqPlot(fuels.test[fuels.test$name=="bm_100h_Mg_ha",]$Long^(1/3)-fuels.test[fuels.test$name=="bm_100h_Mg_ha",]$Short^(1/3)) # no

# cross ck with wilcox, if not agree reason to pause
wilcox.test(fuels.test[fuels.test$name=="bm_100h_Mg_ha",]$Long,fuels.test[fuels.test$name=="bm_100h_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p = 0.08

### dwd 1000h sound
hist(fuels.test[fuels.test$name=="bm_1000h_sound_Mg_ha",]$pair_diff) # skewed plus outliers
hist(fuels.test[fuels.test$name=="bm_1000h_sound_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="bm_1000h_sound_Mg_ha",]$Short^(1/2)) # flat, not great
qqPlot(fuels.test[fuels.test$name=="bm_1000h_sound_Mg_ha",]$Long^(1/2)-fuels.test[fuels.test$name=="bm_1000h_sound_Mg_ha",]$Short^(1/2)) # borderline, still skewed

# wilcox
wilcox.test(fuels.test[fuels.test$name=="bm_1000h_sound_Mg_ha",]$Long,fuels.test[fuels.test$name=="bm_1000h_sound_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p = 0.23

### dwd 1000h rotten
hist(fuels.test[fuels.test$name=="bm_1000h_rotten_Mg_ha",]$pair_diff) # skewed
hist(fuels.test[fuels.test$name=="bm_1000h_rotten_Mg_ha",]$Long^(1/3)-fuels.test[fuels.test$name=="bm_1000h_rotten_Mg_ha",]$Short^(1/3)) # good
qqPlot(fuels.test[fuels.test$name=="bm_1000h_rotten_Mg_ha",]$Long^(1/3)-fuels.test[fuels.test$name=="bm_1000h_rotten_Mg_ha",]$Short^(1/3)) # ok

# t test pref
t.test(fuels.test[fuels.test$name=="bm_1000h_rotten_Mg_ha",]$Long^(1/3),fuels.test[fuels.test$name=="bm_1000h_rotten_Mg_ha",]$Short^(1/3),
       alternative="two.sided", paired=TRUE) # p = 0.21
# cross ck with wilcox, if not agree reason to pause
wilcox.test(fuels.test[fuels.test$name=="bm_1000h_rotten_Mg_ha",]$Long,fuels.test[fuels.test$name=="bm_1000h_rotten_Mg_ha",]$Short, 
            alternative="two.sided", paired=TRUE) # p = 0.49

### are differences also amplified at warmer-drier sites?
fuels.test %>%
  filter(name %in% c("live_ag_total_Mg_ha","dead_woody_total_Mg_ha")) %>%
  mutate(rel_diff = pair_diff/Long) %>%
  left_join(pair.clim, by=c("Plot_pair")) %>%
  # only norm and anom defict and vpd
  dplyr::select(c(Plot_pair:rel_diff, ann_def_mm_norm)) %>%
  pivot_longer(c(pair_diff,rel_diff),names_to="response") %>%
  ggplot(aes(y=value,x=ann_def_mm_norm)) +
  facet_wrap(response~name, scales="free", ncol=2) +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",") +
  geom_point() +
  theme_bw()

fuels.test %>%
  filter(name %in% c("live_ag_total_Mg_ha","dead_woody_total_Mg_ha")) %>%
  mutate(rel_diff = pair_diff/Long) %>%
  left_join(pair.clim, by=c("Plot_pair")) %>%
  # only norm and anom defict and vpd
  dplyr::select(c(Plot_pair:rel_diff, ann_def_mm_norm,summer_vpd_kpa_norm,anom_def_zscore,anom_summer_vpd_zscore)) %>%
  pivot_longer(c(ann_def_mm_norm:anom_summer_vpd_zscore),names_to="clim") %>%
  ggplot(aes(y=rel_diff,x=value)) +
  facet_wrap(clim~name, scales="free", ncol=2) +
  stat_cor(method="spearman", cor.coef.name="rho", p.digits=1, label.sep=",") +
  geom_point() +
  theme_bw()

### dead wood proportions: how allocated by size class across dwd and snags?

fuels.deadwood <- fuels.tots %>%
  filter(!is.na(bm_1000h_rotten_Mg_ha)) %>%
  # summarize by size class
  mutate(dead_woody_1h_Mg_ha = snag_1h_Mg_ha + bm_1h_Mg_ha,
         dead_woody_10h_Mg_ha =snag_10h_Mg_ha + bm_10h_Mg_ha,
         dead_woody_100h_Mg_ha = snag_100h_Mg_ha + bm_100h_Mg_ha,
         dead_woody_1000h_Mg_ha = snag_1000h_Mg_ha + bm_1000h_sound_Mg_ha + bm_1000h_rotten_Mg_ha,
         prop_1h_Mg_ha = dead_woody_1h_Mg_ha/dead_woody_total_Mg_ha,
         prop_10h_Mg_ha = dead_woody_10h_Mg_ha/dead_woody_total_Mg_ha,
         prop_100h_Mg_ha = dead_woody_100h_Mg_ha/dead_woody_total_Mg_ha,
         prop_1000h_Mg_ha = dead_woody_1000h_Mg_ha/dead_woody_total_Mg_ha) %>%
  pivot_longer(c(dead_woody_1h_Mg_ha:prop_1000h_Mg_ha)) %>%
  group_by(Fire_interval,name) %>%
  summarise(mean=mean(value),
            se = std.error(value))

write.csv(fuels.deadwood,"analysis/q2_fuels/dead_wood_proportions.csv",row.names=FALSE)

### h2b: post-fire change in tree/woody biomass/fuels over time

# group by decade
fuels.tsf <- fuels.tots %>%
  # add downed woody total
  mutate(downed_woody_total_Mg_ha = bm_1h_Mg_ha+bm_10h_Mg_ha+bm_100h_Mg_ha+
           bm_1000h_rotten_Mg_ha+bm_1000h_sound_Mg_ha) %>%
  left_join(plots.in, by=c("Plot_code","Fire_interval")) %>%
  # remove plots with no measured downed fuels
  filter(!is.na(downed_woody_total_Mg_ha)) %>%
  # bin by decade
  mutate(TSF_bin = ifelse(TSF<10,5,
                          ifelse(TSF>=10 & TSF<20,15,25))) %>%
  # keep only need columns
  dplyr::select(c(Plot_code,Fire_interval,TSF,TSF_bin,tree_total_Mg_ha,saps_pico_total_Mg_ha,snag_total_Mg_ha,downed_woody_total_Mg_ha))

# how many plots per bin, skewed a bit toward more recent TSF
fuels.tsf %>%
  group_by(Plot_code) %>%
  summarise(TSF_bin = mean(TSF_bin)) %>%
  ungroup() %>%
  group_by(TSF_bin) %>%
  tally()

fuels.tsf %>%
  pivot_longer(c(tree_total_Mg_ha:downed_woody_total_Mg_ha)) %>%
  ggplot(aes(x=TSF,y=value, color=Fire_interval)) +
  facet_wrap(~name, scales="free") +
  geom_point() + 
  geom_smooth()

# aggregate by TSF bin
fuels.tsfout <- fuels.tsf %>%
  pivot_longer(c(tree_total_Mg_ha:downed_woody_total_Mg_ha)) %>%
  # average by tsf
  group_by(Fire_interval,TSF_bin,name) %>%
  summarise(value=mean(value)) 

write.csv(fuels.tsfout, "analysis/q2_fuels/biomass_fuels_tsf.csv", row.names=FALSE)
