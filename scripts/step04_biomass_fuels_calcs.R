#####
#
## live biomass and fuels calculations 
#
#####

# wd inherited from project

# load libraries
library(openxlsx)
library(tidyverse)
library(car)

####
# 1. load data
####

### only 2021 field data

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
plots.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                     sheet="General_plot_measurements",
                     colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("Fire_interval"),factor) %>%
  # select only needed columns
  dplyr::select(Plot_code,Easting:Unburned_dist_m) %>%
  # tack on to plotinfo
  full_join(plotinfo.in, by="Plot_code") %>%
  # add sampling info
  mutate(Sample_year = 2021)

# tallies
counts.in <- read.csv("data/field_plots_2021/cleaned_data/tallies_postfire_cleaned_filled.csv", header=TRUE) %>%
  # drop count, not needed
  dplyr::select(-Count)

# tree measurements
# excludes SALIX 
# also excludes prefire survivors, present at plot edges for 2 short and 2 long interval plots that were located adjacent to a patch of unburned forest; these prefire trees were not representative of stand-replacing fire conditions within the plot
trees.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                     sheet="Trees_snags",
                     colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("T","Species","Stem_type"),factor) %>%
  # exclude salix
  filter(Species != "SALIX") %>%
  # exclude prefire trees
  filter(Stem_type != "Tree_prefire")

# sapling measurements - exclude salix
saps.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                    sheet="Saplings",
                    colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("T","Species"),factor)  %>%
  # exclude salix
  filter(Species != "SALIX")

# shrub cover, aggregated to plot level
shrubs.in <- read.csv("data/field_plots_2021/cleaned_data/shrubs_plotlvl.csv")

# litter and duff
litt.in <- read.csv("data/field_plots_2021/cleaned_data/litter_duff_cleaned_compiled.csv") 

# downed woody debris
surf.in <- read.csv("data/field_plots_2021/cleaned_data/surface_fuels_cleaned_compiled.csv")
cwd.in <- read.csv("data/field_plots_2021/cleaned_data/cwd_sorted_compiled.csv")

####
# 2. live tree biomass and canopy fuels
####

### Overview: 
# Used Brown 1978 species-specific equations to estimate tree biomass compartments
# Used trees &gt;1.4 m height for canopy fuels estimation (CBD, CFL, CH, CBH)
# Used Turner et al. 2004 field data to estimate sapling biomass compartments. First built regression to predict basal diameter from sapling height. Then used PICO 10-yo equations to estimate sapling biomass compartments. Excluded saplings of other, non-PICO species from live biomass calculations.

# quick look at stem type and species
summary(trees.in$Stem_type)
summary(trees.in$Species)
summary(saps.in$Species)

###
# tree biomass and canopy fuels: use species-specific equations from brown 1978 and ter-mikaelian & korzukhin 1997
###

# using dominant/codominant equations for all species, assuming ample
# light available for regenerating tree layer early in postfire recovery

### prep trees

head(trees.in)
unique(trees.in$Notes)
summary(trees.in)

# only live trees for live biomass and canopy fuels
trees.live <- trees.in %>%
  filter(!Stem_type %in% c("Snag"),
         !Species %in% c("D")) 

# look at notes to determine if need to correct/add missing data
trees.live %>%
  filter(!is.na(Notes))

trees.live %>%
  filter(Height_m<1.4) # 2 trees less than BH, drop these from tree averages

# some trees missing DBH, others CBH, use similar size trees to assign value
summary(trees.live) # 1 trees no DBH, 5 trees no CBH

trees.live %>%
  filter(is.na(DBH_cm) | is.na(CBH_m)) # all smaller trees, all POTR

# use ratios for smaller trees < 2.5 m height for DBH and CBH by species
trees.ratiosm <- trees.live %>%
  filter(Height_m < 2.5) %>%
  # calculate height:diameter and CBH:height ratios for each tree
  mutate(sm_tree_hd = Height_m/DBH_cm,
         sm_tree_ch = CBH_m/Height_m) %>%
  # average value by species, includes all trees recorded
  group_by(Species) %>%
  summarise(sm_tree_hd = mean(sm_tree_hd, na.rm=TRUE),
            sm_tree_ch = mean(sm_tree_ch, na.rm=TRUE))
  
# drop trees below breast height
# add in missing DBH and CBH values
# also, convert to english units for biomass equations
trees.eng <- trees.live %>%
  # drop trees < 1.4 m
  filter(Height_m >=1.4) %>%
  # join with df to fill in missing values
  left_join(trees.ratiosm, by="Species") %>%
  # DBH
  mutate(DBH_cm = ifelse(is.na(DBH_cm),Height_m/sm_tree_hd,DBH_cm)) %>%
  # CBH
  mutate(CBH_m = ifelse(is.na(CBH_m)&Height_m<2.5,Height_m*sm_tree_ch,CBH_m)) %>%
  # remove conversion and other columns
  dplyr::select(-c(Notes:sm_tree_ch)) %>%
  # conver to english units for biomass allometric equations
  mutate(DBH_in = DBH_cm / 2.54,
         Height_ft = Height_m * 3.28,
         CBH_ft = CBH_m * 3.28) %>%
  # add in other variables used in equations
  # crown length, assuming crown goes full height, reasonable for young trees
  # only 1 tree with "dead top" note, assume negligible impact on total biomass and fuels
  mutate(CL_ft = Height_ft - CBH_ft,
         # crown ratio
         CR = (CL_ft/Height_ft)*10)

summary(trees.eng) # no more missing values

### step 1: crown weights for conifers, diff eq for large (> 1 in DBH) and small (< 2 in DBH)
# all trees < 10 in DBH, so do not need equations for even larger size classes
# also calc dead crown

# tree codes: Brown 1978
# LP: lodgepole pine
# DF: douglas-fir - there are no DF trees
# S: Engelmann spruce
# AF: subalpine fir
# WBP: whitebark pine

# calculate crown weights
trees.crown <- trees.eng %>%
  # subset to only conifers
  filter(Species != "POTR") %>%
  # live crown equations for trees > 1 in DBH
  mutate(live_crown_lg_lb = 
           ifelse(Species=="PICO",
                  # chose second, simpler eq, better correspond with smaller trees
                  exp(0.1224 + 1.8820*log(DBH_in)),
                  ifelse(Species=="PIEN",
                         exp(1.0404 + 1.7096*log(DBH_in)),
                         ifelse(Species=="ABLA",
                                1.066 + 0.1862*(DBH_in^2)*CR,
                                ifelse(Species=="PIAL",
                                       0.65+0.06056*(DBH_in^3)+0.05477*(DBH_in^2)*CR,NA))))) %>%
  # live crown equations for trees < 2 in DBH
  mutate(live_crown_sm_lb = 
           ifelse(Species=="PICO",
                  0.03111*(Height_ft^2), 
                  ifelse(Species=="PIEN",
                         exp(-3.932 + 2.571*log(Height_ft)),
                         ifelse(Species=="ABLA",
                                exp(-3.335+2.303*log(Height_ft)),
                                ifelse(Species=="PIAL",
                                       0.070+0.02446*(Height_ft^2),NA))))) %>%
  # use 1.5 in DBH as cutoff between large and small equations, ensures no negative values
  # 1 ABLA tree has large impact on estimated biomass, keep eye on this
  mutate(live_crown_lb = 
           ifelse(DBH_in<1.5,
                  live_crown_sm_lb,live_crown_lg_lb)) %>%
  # dead crown equations for trees > 1 in DBH
  mutate(dead_crown_lg_lb =
           ifelse(Species=="PICO",
                  # DBHs are < 10 in
                  (0.026*DBH_in - 0.025)*live_crown_lg_lb,
                  ifelse(Species=="PIEN",
                         exp(3.2719*log(DBH_in*Height_ft) - 3.2603*log(CL_ft) - 6.8771),
                         # equation specific to DBH < 16 in
                         ifelse(Species=="ABLA",
                                exp(4.0365*log(DBH_in) - 6.5431),
                                # equation without minimum value because trees are small
                                ifelse(Species=="PIAL",
                                       0.06117*(DBH_in^2),NA))))) %>%
  # assume dead crown weight negligible below 1 in DBH cutoff, per Brown
  mutate(dead_crown_lb = 
           ifelse(DBH_in<1,0,dead_crown_lg_lb))

# plot checks
ggplot(aes(x=live_crown_lg_lb,y=live_crown_sm_lb,color=Species), data=trees.crown[trees.crown$DBH_in<1.5,]) +
  geom_point() +
  stat_smooth(method="lm",se=FALSE) +
  theme_bw()

ggplot(aes(x=live_crown_lg_lb,y=dead_crown_lg_lb, color=Species), data=trees.crown) +
  geom_point() +
  theme_bw()

ggplot(aes(x=DBH_in,y=dead_crown_lb/live_crown_lg_lb, color=Species), data=trees.crown) +
  geom_point() +
  theme_bw()

### step 2: stem weights including bark

# brown 1978 for trees < 4 in DBH (all species except a few large pico)
# ter-mikaelin & korzukhin for larger PICO (orig eq from Gholz et al. 1979)

trees.eng %>%
  filter(DBH_in>4) # all PICO

trees.stem <- trees.eng %>%
  # subset to conifers
  filter(Species != "POTR") %>%
  # bole weight (total stem weight) for trees < 4 in DBH, brown 1978
  mutate(stem_wt_sm_lb = 
           ifelse(Species=="PICO",
                  1.49 - 2.388*(DBH_in) + 2.297*(DBH_in^2),
                  ifelse(Species=="PIEN",
                         exp(0.8381 + 1.3803*log(DBH_in)),
                         ifelse(Species=="ABLA",
                                0.28+0.02692*(DBH_in^2)*Height_ft + 0.1912*(DBH_in)*Height_ft,
                                ifelse(Species=="PIAL",
                                       1.33+0.08614*(DBH_in^2)*Height_ft,NA))))) %>%
  # total stem weight for trees > 3 cm DBH, ter-mikaelin & korzukhin/gholz
  # also convert kg to lb
  mutate(stem_wt_lg_lb = 
           ifelse(Species=="PICO",
                  (0.0492*DBH_cm^(2.4287))*2.205,NA)) %>%
  # assign stem weight based on DBH
  mutate(stem_wt_lb =
           ifelse(DBH_in<4,
                  stem_wt_sm_lb,stem_wt_lg_lb))

# plot checks
ggplot(aes(x=stem_wt_sm_lb,y=stem_wt_lg_lb, color=Species), data=trees.stem) +
  geom_point() +
  theme_bw() # looks good

### step 3: compare with total weight from brown 1978

trees.totalck <- trees.eng %>%
  # subset to conifers
  filter(Species != "POTR") %>%
  # total tree weight for trees < 15 ft height, brown 1978
  mutate(total_wt_sm_lb = 
           ifelse(Species=="PICO",
                  exp(-3.720 + 2.411*log(Height_ft)),
                  ifelse(Species %in% c("PIEN","ABLA"),
                         exp(-3.385+2.560*log(Height_ft)),
                                ifelse(Species=="PIAL",
                                       exp(-2.876+2.175*log(Height_ft)),NA)))) %>%
  dplyr::select(c(Plot_code:Species,DBH_in,Height_ft,total_wt_sm_lb)) %>%
  left_join(trees.crown, by=c("Plot_code","T","Int","Stem_type","Species","DBH_in","Height_ft")) %>%
  left_join(trees.stem, by=c("Plot_code","T","Int","Stem_type","Species","DBH_in","Height_ft")) %>%
  # sum of weight based on compartments
  mutate(total_wt_sum = live_crown_lb+dead_crown_lb+stem_wt_lb)

# plot checks
ggplot(aes(x=total_wt_sum,y=total_wt_sm_lb, color=Species), data=trees.totalck[trees.totalck$Height_ft<15,]) +
  geom_point() +
  theme_bw() # some discrepancies, but looks good at smaller values

ggplot(aes(x=DBH_in,y=total_wt_sm_lb, color=Species), data=trees.totalck) +
  geom_point() +
  theme_bw()

ggplot(aes(x=DBH_in,y=total_wt_sum, color=Species), data=trees.totalck) +
  geom_point() +
  theme_bw()

# will use summed values for plot-level data, better correspond to tree DBH sizes

### step 4: crown compartments, using proportions from brown 1978 appendix. 

# small trees are < 1 in DBH
# medium are 1-2.9 or 1-3.9 in DBH depending on the species
# large are 2.9 or 3.9 in + DBH depending on the species
# first, what is the largest DBH tree by species
trees.crown %>%
  group_by(Species) %>%
  summarise(DBH_in = max(DBH_in)) # everything is < 2.9 in except PICO

# table with final tree biomass
# add in crown compartments
trees.total <- trees.crown %>%
  dplyr::select(c(Plot_code:DBH_cm,live_crown_lg_lb:dead_crown_lb)) %>%
  left_join(trees.stem, by=c("Plot_code","T","Int","Stem_type","Species","DBH_cm")) %>%
  mutate(total_wt_lb = live_crown_lb+dead_crown_lb+stem_wt_lb) %>%
  # crown biomass compartments
  # dbh <= 1 in
  mutate(foliage_sm_lb = 
           ifelse(Species %in% c("PICO","PIAL"),
                  0.52*live_crown_lb,
                  ifelse(Species %in% c("PIEN","ABLA"),
                                        0.62*live_crown_lb,NA))) %>%
  mutate(branch1h_sm_lb = 
           ifelse(Species %in% c("PICO","PIAL"),
                  0.27*live_crown_lb,
                  ifelse(Species %in% c("PIEN","ABLA"),
                         0.26*live_crown_lb,NA))) %>%
  mutate(branch10h_sm_lb = 
           ifelse(Species %in% c("PICO","PIAL"),
                  0.21*live_crown_lb,
                  ifelse(Species %in% c("PIEN","ABLA"),
                         0.12*live_crown_lb,NA))) %>%
  mutate(branch100h_sm_lb = 0) %>%
  # larger trees
  # first calculate proportions based on DBH
  mutate(prop_foliage =
           ifelse(Species=="PICO",
                  0.493 - 0.0117*DBH_in,
                  ifelse(Species=="PIEN",
                         0.578*exp(-0.0325*DBH_in),
                         ifelse(Species=="ABLA",
                                0.597*exp(-0.0425*DBH_in),
                                ifelse(Species=="PIAL",
                                       0.512*exp(-0.0374*DBH_in),NA))))) %>%
  mutate(prop_1h_lg =
           ifelse(Species=="PICO",
                  0.777 - 0.0146*DBH_in - prop_foliage,
                  ifelse(Species=="PIEN",
                         0.852*exp(-0.0281*DBH_in) - prop_foliage,
                         ifelse(Species=="ABLA",
                                0.864*exp(-0.0373*DBH_in) - prop_foliage,
                                ifelse(Species=="PIAL",
                                       0.864*exp(-0.0585*DBH_in) - prop_foliage,NA))))) %>%
  # prop_10h_lg only for large trees, which only includes PICO
  mutate(prop_10h_lg =
           ifelse(Species=="PICO",
                  1.049 - 0.0140*DBH_in - prop_foliage - prop_1h_lg,NA)) %>%
  # otherwise prop is 1 minus other compartments
  mutate(prop_10h_med = 1 - prop_foliage - prop_1h_lg) %>%
  # prop 100 is either 0 (medium trees) or 1 minus other compartments (larger trees)
  mutate(prop_100h_lg = 1 - prop_foliage - prop_1h_lg - prop_10h_lg) %>%
  # dbh > 1 in
  mutate(foliage_lg_lb = prop_foliage * live_crown_lb,
         branch1h_lg_lb = prop_1h_lg * live_crown_lb,
         branch10h_med_lb = prop_10h_med * live_crown_lb,
         branch10h_lg_lb = prop_10h_lg * live_crown_lb,
         branch100h_lg_lb = prop_100h_lg * live_crown_lb) %>%
  # final live compartment values, DBH separator only applies to PICO
  mutate(foliage_live_lb = 
           ifelse(DBH_in<=1,
                  foliage_sm_lb,foliage_lg_lb),
         branch1h_live_lb =
           ifelse(DBH_in<=1,
                  branch1h_sm_lb,branch1h_lg_lb),
         branch10h_live_lb =
           ifelse(DBH_in<=1,
                  branch10h_sm_lb,
                  ifelse(DBH_in<=3.9,
                         branch10h_med_lb,branch10h_lg_lb)),
         branch100h_live_lb =
           ifelse(DBH_in<=3.9,
                  0,branch100h_lg_lb)) %>%
  # check live crown biomass
  mutate(live_crown_ck = foliage_live_lb + branch1h_live_lb + branch10h_live_lb + branch100h_live_lb) %>%
  # dead compartments, only p1 and p2, foliage and 1h dead
  mutate(prop_foliage_dead_sm = 1) %>%
  mutate(prop_foliage_dead_lg = 
           ifelse(Species=="PICO",
                  1.353*(DBH_in^(-0.758)),
                  ifelse(Species=="PIEN",
                         1.466*(DBH_in^(-0.645)),
                         ifelse(Species=="ABLA",
                                1.210*(DBH_in^(-0.565)),NA)))) %>%
  mutate(prop_1h_dead_med = 1 - prop_foliage_dead_lg) %>%
  # final values, use cutoff to assign to small or large size class
  mutate(foliage_dead_lb =
           ifelse(DBH_in < 1.5 & Species %in% c("PICO","ABLA","PIAL"),
                  dead_crown_lb * prop_foliage_dead_sm,
                  ifelse(DBH_in < 1.8 & Species=="PIEN",
                         dead_crown_lb * prop_foliage_dead_sm,
                         dead_crown_lb * prop_foliage_dead_lg))) %>%
  mutate(branch1h_dead_lb = 
           ifelse(DBH_in >= 1.5 & Species %in% c("PICO","ABLA"),
                  dead_crown_lb*prop_1h_dead_med,0)) %>%
  # check dead crown biomass
  mutate(dead_crown_ck = foliage_dead_lb + branch1h_dead_lb)
  
summary(trees.total)

# plot checks
trees.total %>%
  pivot_longer(cols=c(live_crown_lb,foliage_live_lb,branch1h_live_lb,branch10h_live_lb,
                      branch100h_live_lb,foliage_dead_lb,branch1h_dead_lb)) %>%
  ggplot(aes(x=DBH_in,y=value,color=Species)) +
  facet_wrap(~name,scales="free_y") +
  geom_point() +
  theme_bw()

ggplot(aes(x=live_crown_lb,y=live_crown_ck, color=Species), data=trees.total) +
  geom_point()  +
  theme_bw()
ggplot(aes(x=dead_crown_lb,y=dead_crown_ck, color=Species), data=trees.total) +
  geom_point()  +
  theme_bw()

ggplot(aes(x=live_crown_lb+dead_crown_lb,y=foliage_live_lb+branch1h_live_lb+branch10h_live_lb+branch100h_live_lb+foliage_dead_lb+branch1h_dead_lb, color=Species), data=trees.total) +
  geom_point()  +
  theme_bw()

### step 5: Canopy fuels. Follow methods outlined in Donato et al. 2013 and Reinhardt et al. 2006.

# first, create output df and convert back to metric, add available canopy fuel loads
trees.met <- trees.total %>%
  dplyr::select(c(Plot_code:DBH_cm,Height_m:CBH_m,live_crown_lb,dead_crown_lb,
                  stem_wt_lb,total_wt_lb,foliage_live_lb:branch100h_live_lb,
                  foliage_dead_lb:branch1h_dead_lb)) %>%
  # convert to metric units
  mutate(crown_live_kg = live_crown_lb * 0.4536,
         crown_dead_kg = dead_crown_lb * 0.4536,
         stem_kg = stem_wt_lb * 0.4536,
         total_kg = total_wt_lb * 0.4536,
         foliage_live_kg = foliage_live_lb * 0.4536,
         branch1h_live_kg = branch1h_live_lb * 0.4536,
         branch10h_live_kg = branch10h_live_lb * 0.4536,
         branch100h_live_kg = branch100h_live_lb * 0.4536,
         foliage_dead_kg = foliage_dead_lb * 0.4536,
         branch1h_dead_kg = branch1h_dead_lb * 0.4536) %>%
  dplyr::select(-c(live_crown_lb:branch1h_dead_lb)) %>%
  # add available canopy fuel load per donato et al 2013
  # live foliage + dead foliage + 50% live 1h + 100% dead 1h
  mutate(cfl_kg = foliage_live_kg + foliage_dead_kg + branch1h_live_kg + branch1h_dead_kg)

# summarise canopy fuels and biomass for conifers
# plot-level conifer counts
counts.conplot <- counts.in %>%
  # only conifers, no psme trees but including to cross-check
  filter(Species %in% c("ABLA","PICO","PIEN","PIAL","PSME")) %>%
  # only trees
  filter(Stem_type=="Tree") %>%
  group_by(Plot_code,Species) %>%
  summarize(stems_ha = sum(stems_ha))

# plot-level sums
plot.conbm <- trees.met %>%
  ungroup() %>%
  # remove unneeded columns
  dplyr::select(-c(T:Stem_type)) %>%
  # sum separately by plot and species
  group_by(Plot_code,Species) %>%
  # average tree value, includes DBH, Height, CBH
  summarise_if(is.numeric,mean) %>%
  # join to plot counts
  left_join(counts.conplot, by=c("Plot_code","Species")) %>%
  # multiply by stem count to get kg/ha, divide by 1000 to get Mg/ha
  mutate(crown_live_Mg_ha = crown_live_kg*stems_ha/1000,
         crown_dead_Mg_ha = crown_dead_kg*stems_ha/1000,
         stem_Mg_ha = stem_kg*stems_ha/1000, 
         total_Mg_ha = total_kg*stems_ha/1000,
         foliage_live_Mg_ha = foliage_live_kg*stems_ha/1000, 
         branch1h_live_Mg_ha = branch1h_live_kg*stems_ha/1000, 
         branch10h_live_Mg_ha = branch10h_live_kg*stems_ha/1000, 
         branch100h_live_Mg_ha = branch100h_live_kg*stems_ha/1000,
         foliage_dead_Mg_ha = foliage_dead_kg*stems_ha/1000,
         branch1h_dead_Mg_ha = branch1h_dead_kg*stems_ha/1000, 
         cfl_Mg_ha = cfl_kg*stems_ha/1000) %>%
  # also get average DBH, Height, CBH by weighted averaging
  # first multiply by count
  mutate(DBH_cm = DBH_cm * stems_ha,
         Height_m = Height_m * stems_ha,
         CBH_m = CBH_m * stems_ha) %>%
  # remove old columns
  dplyr::select(-c(crown_live_kg:cfl_kg)) %>%
  ungroup() %>%
  # sum across all species in a plot
  group_by(Plot_code) %>%
  summarise_if(is.numeric,sum) %>%
  # divide DBH, Height, CBH for weighted average
  mutate(DBH_cm = DBH_cm / stems_ha,
         Height_m = Height_m / stems_ha,
         CBH_m = CBH_m / stems_ha)

# double check: any plots with conifer counts missing biomass data?
plot.conck <- trees.met %>%
  ungroup() %>%
  # sum separately by plot and species
  group_by(Plot_code,Species) %>%
  # average tree value, includes DBH, Height, CBH
  summarise_if(is.numeric,mean) %>%
  # join to plot counts
  full_join(counts.conplot, by=c("Plot_code","Species")) %>%
  filter(stems_ha>0, is.na(DBH_cm))
plot.conck # all good

head(plot.conbm)
summary(plot.conbm) # how do values align with nelson, donato; reasonable
# much higher ratio of cfl/foliage biomass to total crown biomass relative to nelson
# but makes sense in the context of small trees have higher ratio of foliage:crown biomass

# follow donato et al. 2013, from reinhardt et al. 2006 for CBD
# distribute CFL in 0.25-m bins along crown length for all trees
# then sum bins
trees.cbd <- trees.met %>%
  # round CBH and Height to nearest 0.25 m, this will be the bottom of lowest bin and top of highest bin
  mutate(CBH_m_round = round(CBH_m*40,-1)/40,
         Height_m_round = round(Height_m*40,-1)/40) %>%
  # number of bins
  mutate(n_bins = (Height_m_round-CBH_m_round)/0.25) %>%
  # cfl per bin
  mutate(cfl_bin = cfl_kg/n_bins)

# distribute CBD in height bins for each tree, save as data frame
cbd.out <- data.frame()

for(i in 1:dim(trees.cbd)[1]) {
  # read in each tree
  cbd.in <- trees.cbd[i,]

  # start at lowest bin
  bin_start <- cbd.in$CBH_m_round
  bin_next <- bin_start
  
  # continue to add bins until reach last bin
  while(cbd.in$Height_m_round > bin_next) {
    
    # cfl in bin
    bin_cfl <- cbd.in$cfl_bin
  
    # add to output
    cbd.out <- rbind(cbd.out, data.frame(Plot_code = cbd.in$Plot_code, Tree_index=i, Species = cbd.in$Species,
                                        bin_start=bin_next,bin_cfl=bin_cfl))
    
    # update bin index
    bin_next <- bin_next + 0.25
  
  }
  
}

head(cbd.out)
tail(cbd.out)
summary(cbd.out)

# check that totals match
cbd.out %>%
  group_by(Tree_index) %>%
  summarise(cfl_ck = sum(bin_cfl)) %>%
  cbind(trees.cbd) %>%
  mutate(cfl_diff = cfl_kg-cfl_ck) %>%
  summary() # looks good, cfl_diff~0
  
# calculate CBD within each bin
cbd.binned <- cbd.out %>%
  # first average bin by species
  group_by(Plot_code,Species,bin_start) %>%
  summarize(bin_cfl = mean(bin_cfl)) %>%
  # join to species counts
  left_join(counts.conplot, by=c("Plot_code","Species")) %>%
  # multiply by counts, divide by m2 in a ha, divide by m in bin to get m3
  # this gives cbd in kg/m3 within a bin for each species
  mutate(total_bin_cfl_kg_m3 = (bin_cfl * stems_ha)/10000/0.25) %>%
  # sum across all species to get cbd in kg/m3 in each bin
  ungroup() %>%
  group_by(Plot_code,bin_start) %>%
  summarise(total_bin_cfl_kg_m3=sum(total_bin_cfl_kg_m3))

# canopy base height = lowest bin with > 0.011 kg/m3
# almost everything with trees is 0, but most plots don't even have trees
cbd.cbh <- cbd.binned %>%
  filter(total_bin_cfl_kg_m3>= 0.011) %>%
  group_by(Plot_code) %>%
  summarise(bin_start=min(bin_start)) %>%
  # cbh as middle of the bin
  mutate(cbh_cbdcutoff = bin_start + 0.125) %>%
  dplyr::select(-bin_start)

# calculate 4-m running mean (based on 13-ft from FVS-FFE)
head(cbd.binned)

# output dataframe
cbd.runout <- data.frame()

# loop through each bin
for(j in 1:dim(cbd.binned)[1]){
  
  cbd.in <- cbd.binned[j,]
  
  # start from lowest bin, calculate bottom of bin by subtracting 4 m
  bin_end <- cbd.in$bin_start
  bin_begin <- bin_end - 3.75
  
  # subset df to only bins within that range (for bins at bottom, that range will be truncated by the ground)
  cbd.sub <- cbd.binned %>%
    filter(Plot_code==cbd.in$Plot_code,
           between(bin_start, bin_begin, bin_end))
  
  # running mean value within subset values
  cbd.calc <- mean(cbd.sub$total_bin_cfl_kg_m3)
  
  # add running mean to output df
  cbd.in$running_mean_cbd_kg_m3 <- cbd.calc
  cbd.runout <- rbind(cbd.runout, as.data.frame(cbd.in))

}
  
head(cbd.runout)
summary(cbd.runout)

# maximum running 4m mean CBD is plot-level CBD
cbd.final <- cbd.runout %>%
  group_by(Plot_code) %>%
  summarise(cbd_kg_m3 = max(running_mean_cbd_kg_m3))

summary(cbd.final)

# join calculated cbd and cbh
plot.canopyfuels <- plot.conbm %>%
  left_join(cbd.cbh, by="Plot_code") %>%
  left_join(cbd.final, by="Plot_code")

summary(plot.canopyfuels)
  
# write this out
write.csv(plot.canopyfuels, "processed_data/biomass_fuels/tree_biomass_canopy_fuels_conifers.csv", row.names=FALSE)

# subset version for main output
conifer.out <- plot.canopyfuels %>%
  dplyr::select(c(Plot_code,total_Mg_ha,cfl_Mg_ha,cbd_kg_m3))

### step 6: biomass for aspen
  
# from ter-mikaelian & korzukhin, try johnston & bartos (wyoming, but larger size) and ker 1984 (0-36 cm DBH)
# using ker 1984, only computing total aboveground biomass
# AB: total aboveground biomass

head(trees.eng)

# subset to POTR
# compare 2 allometric equations
trees.potrtest <- trees.eng %>%
  filter(Species=="POTR") %>%
  mutate(jb_total_kg = 0.1231 * DBH_cm^(2.2420),
         ker_total_kg = 0.1049 * DBH_cm^(2.3910))

ggplot(aes(x=jb_total_kg,y=ker_total_kg), data=trees.potrtest) +
  geom_point() +
  theme_bw() # pretty close, use ker because size range matches up

# plot-level potr counts
counts.potrplot <- counts.in %>%
  # subset to POTR
  filter(Species %in% c("POTR")) %>%
  # only trees
  filter(Stem_type=="Tree") %>%
  # sum by plot
  group_by(Plot_code,Species) %>%
  summarize(stems_ha = sum(stems_ha))

# total biomass for plot
trees.potr <- trees.eng %>%
  # subset to POTR
  filter(Species=="POTR") %>%
  # apply ker allometric equation
  mutate(total_kg = 0.1049 * DBH_cm^(2.3910)) %>%
  # remove unneeded columns
  dplyr::select(-c(T:Stem_type,DBH_in:CR)) %>%
  # average across all trees in plot
  group_by(Plot_code,Species) %>%
  summarise_if(is.numeric,mean) %>%
  # total for plot with stem density
  left_join(counts.potrplot, by=c("Plot_code","Species")) %>%
  # kg/ha to Mg/ha
  mutate(total_Mg_ha = (total_kg*stems_ha)/1000) %>%
  dplyr::select(-total_kg)

# double check: any plots with counts missing biomass data?
plot.potrck <- trees.eng %>%
  filter(Species=="POTR") %>%
  # average across all trees in plot
  group_by(Plot_code,Species) %>%
  summarise_if(is.numeric,mean) %>%
  # total for plot with stem density
  full_join(counts.potrplot, by=c("Plot_code","Species")) %>%
  filter(stems_ha>0, is.na(DBH_cm))
plot.potrck # all good

# main output
potr.out <- trees.potr %>%
  dplyr::select(c(Plot_code,total_Mg_ha)) %>%
  rename(potr_total_Mg_ha = total_Mg_ha)

####
# 3. live sapling biomass
####

# what proportion of live saplings are pico?
counts.ck <- counts.in %>%
  filter(Stem_type=="Sapling_gt_2") %>%
  # no dead stems
  filter(!Species %in% c("D")) %>%
  # tally number of pico and number of non-pico
  mutate(species_group = ifelse(Species=="PICO","PICO","non_PICO")) %>%
  group_by(Stem_type,species_group) %>%
  summarise(stems_ha=sum(stems_ha)) %>%
  # pivot to get proportion PICO
  pivot_wider(values_from=stems_ha,names_from=species_group) %>%
  mutate(total=non_PICO+PICO,
         prop_PICO = PICO/total)

counts.ck # pico accounts for 97% of all live saplings

saps.in %>%
  filter(Species=="PICO") %>%
  summary() # no missing values

### will not run, this dataset not included in deposit
### estimate basal diameter from height from turner et al. 2004 data
# read in stems from turner et al.
sapling.fit <- read.csv("data/Turner_etal2004/90points_sapling_data.csv") %>%
  rename(basal_diam_cm = Basal.Diam...cm.,
         height_cm = Height..cm.) %>%
  # only entries
  filter(basal_diam_cm>0) %>%
  # subset below breast height
  filter(height_cm < 140)

ggplot(aes(x=height_cm, y=basal_diam_cm), data=sapling.fit) +
  geom_point() +
  theme_bw()
# relationship looks fairly linear, some clear outliers with large basal diameters and short heights

# linear fit
lm.ht <- lm(basal_diam_cm~0+height_cm, data=sapling.fit)
plot(lm.ht,1) # residuals look good except a few outliers
qqPlot(lm.ht, 2) # upper tail skewed due to outliers, remove these for improved prediction of average conditions
outlierTest(lm.ht)
lm.out <- names(outlierTest(lm.ht)[[1]])
lm.out

sapling.sub <- sapling.fit[-c(as.numeric(lm.out)),]

# fit without outliers
lm.ht2 <- lm(basal_diam_cm~0+height_cm, data=sapling.sub)
plot(lm.ht2,1) # residuals look ok
qqPlot(lm.ht2) # still some skew at upper tail but much improved

summary(lm.ht2) # r2 = 0.93

lm.coef <- 0.0305738  

ggplot(aes(x=height_cm, y=basal_diam_cm), data=sapling.sub) +
  geom_point() +
  geom_line(aes(y=0+lm.coef*height_cm), color="red",size=1) +
  theme_bw()
### END section that will not run

lm.coef <- 0.0305738  

# estimate basal diameter for saplings
saps.pico <- saps.in %>%
  filter(Species=="PICO") %>%
  # height in cm
  mutate(Height_cm = Height_m * 100) %>%
  # estimate basal diameter
  mutate(Basal_diam_cm = lm.coef*Height_cm)

### calculate biomass, only pico for sapling layer, equations from Turner et al. 2004
head(saps.pico)
summary(saps.pico)

# sapling counts, pico only, omit seedlings
counts.saps <- counts.in %>%
  filter(Stem_type=="Sapling_gt_2",
         Species=="PICO")

# turner et al. 2004 equations, total biomass only
saps.bm <- saps.pico %>%
  mutate(total_g = 13.144 * Basal_diam_cm^2.888) %>%
  # remove unneeded columns
  dplyr::select(-c(T,Int,Height_m:Basal_diam_cm)) %>%
  # summarize by plot
  group_by(Plot_code,Species) %>%
  summarise_if(is.numeric,mean) %>%
  # add counts
  left_join(counts.saps, by=c("Plot_code","Species")) %>%
  mutate(total_Mg_ha = total_g/1000000 * stems_ha)

summary(saps.bm)

# main out
saps.out <- saps.bm %>%
  dplyr::select(c(Plot_code, total_Mg_ha)) %>%
  rename(saps_pico_total_Mg_ha = total_Mg_ha) 
  

####
# 4. shrub biomass
####

### estimate shrub biomass from percent cover
# different equations for shorter (< 0.5 m) and taller shrubs
# VASC equations (turner et al. 2004) for short shrubs 
# use CEVE equations (BIOPAK) for taller shrubs

head(shrubs.in)

# whats included in short
shrubs.in %>%
  filter(Height_wtavg_m<=0.5) %>%
  group_by(Species) %>%
  summarise(Cover_pct = sum(Cover_pct),
            Height_avg = mean(Height_wtavg_m)) %>%
  arrange(desc(Cover_pct)) # mostly SPBE, VASC, VAME, MARE; smaller proportions of other species
  
# and tall
shrubs.in %>%
  filter(Height_wtavg_m>0.5) %>%
  group_by(Species) %>%
  summarise(Cover_pct = sum(Cover_pct),
            Height_avg = mean(Height_wtavg_m)) %>%
  arrange(desc(Cover_pct)) # mostly CEVE; some Ribes

# some shrubs no height measured, assign short or tall based on average height for that species
shrub.avght <- shrubs.in %>%
  group_by(Species) %>%
  summarise(Height_spec_avg = mean(Height_wtavg_m, na.rm=TRUE)) %>%
  # PAMY is short, give generic height
  mutate(Height_spec_avg = ifelse(Species=="PAMY",0.1,Height_spec_avg))

shrubs.in %>%
  filter(is.na(Height_wtavg_m))
shrub.avght

shrubs.bm <- shrubs.in %>%
  # add avg height and assign to missing values
  left_join(shrub.avght, by="Species") %>%
  mutate(Height_wtavg_m = ifelse(is.na(Height_wtavg_m),Height_spec_avg,Height_wtavg_m)) %>%
  # calculate biomass, in g/m2
  mutate(total_g =
           # VASC equation
           ifelse(Height_wtavg_m <= 0.5,
                  0.7134*Cover_pct + 0.372,
                  # CEVE equation
                  -0.088 + 1.038*Cover_pct))

# sum shrubs by plot, main output
shrubs.out <- shrubs.bm %>%
  group_by(Plot_code) %>%
  # convert to Mg/ha
  summarise(shrub_total_Mg_ha = sum(total_g/100))

summary(shrubs.out)

####
# 5. standing dead snags > BH, include also dead trees if present
####

### prepare for biomass estimation
# snag counts
count.snags <- counts.in %>%
  filter(Stem_type %in% c("Snag","Tree"),
         Species %in% c("RF","PF","D"))%>%
  group_by(Plot_code,Species) %>%
  summarise(stems_ha=sum(stems_ha))

# 2 snags had 2 stems, 1 trunk; use DBH from largest stem to estimate biomass (DBH_cm, ignore DBH_cm_2)
# 2 snags were < BH; remove from data for estimating biomass

snags.in <- trees.in %>%
  filter(Stem_type %in% c("Snag","Tree"),
         Species %in% c("RF","PF","D"))%>%
  dplyr::select(c(Plot_code:Height_m))

ggplot(aes(x=DBH_cm, y=Height_m, color=Species), data=snags.in) +
  geom_point() +
  theme_bw()

summary(snags.in)
head(snags.in)

# fix data with missing measurements
snags.na <- full_join(count.snags, snags.in, by=c("Plot_code","Species")) %>%
  filter(stems_ha>0, is.na(DBH_cm))
snags.na

# For dead trees: assign size based on average of live tree size
snags.est <- trees.in %>%
  filter(Plot_code %in% c(snags.na$Plot_code),
         Stem_type %in% c("Tree")) %>%
  # drop Clover_LeHardy
  filter(Plot_code != "Clover_LeHardy_1_1_long_2") %>%
  group_by(Plot_code,Stem_type,Species) %>%
  summarise(across(c(DBH_cm,Height_m), mean)) %>%
  # add counts to get weighted avg for Astringent_Tern
  left_join(counts.in, by=c("Plot_code","Stem_type","Species")) %>%
  # weighted avg
  mutate(DBH_avg = DBH_cm*stems_ha,
         Ht_avg = Height_m*stems_ha) %>%
  ungroup() %>%
  group_by(Plot_code,Stem_type) %>%
  summarise(across(c(stems_ha,DBH_avg,Ht_avg),sum)) %>%
  mutate(DBH_cm = DBH_avg/stems_ha,
         Height_m = Ht_avg/stems_ha) %>%
  # add species, prepare to add to snags.in
  mutate(Species="D",T=1,Int=1) %>%
  dplyr::select(c(Plot_code,T,Int,Stem_type,Species,DBH_cm,Height_m))

# add estimated dead tree size to snags.in, convert to English units
snags.eng <- snags.in %>%
  rbind(snags.est) %>%
  # english units
  mutate(DBH_in = DBH_cm / 2.54,
         Height_ft = Height_m * 3.28,
         HD = Height_m*100/DBH_cm) %>%
  # remove any measurements on trees < BH
  filter(Height_m>=1.4)

# check, just Clover_LeHardy remains
full_join(count.snags, snags.eng, by=c("Plot_code","Species")) %>%
  filter(stems_ha>0, is.na(DBH_cm))

### biomass estimates based on lodgepole pine
# individual tree volume equation from cole 1971, takes DBH and height
# multiply by 0.405 g/cm3 wood density for decay class 1 from harmon & sexton 1996
snags.bm <- snags.eng %>%
  # volume from DBH in in and height in ft
  mutate(snag_volume_ft3 = 0.002782 * (DBH_in^2)*Height_ft * (Height_ft^0.0488)/(DBH_in^0.0959),
         # convert to cm3
         snag_volume_cm3 = snag_volume_ft3 * 28317,
         # multiply by wood density, convert to kg
         snag_kg = 0.405*snag_volume_cm3 /1000,
         # some quality checks
         # compare to volume of tapered cylinder
         snag_volume_ck = 0.46952*(pi*(DBH_in/(2*12))^2)*Height_ft,
         # compare to allometric equations for live trees, Brown & Gholz equations
         snag_brown = (1.49 - 2.388*(DBH_in) + 2.297*(DBH_in^2))*0.4536,
         snag_gholz = 0.0492*DBH_cm^2.4287) 

ggplot(aes(x=snag_volume_ck, y=snag_volume_ft3), data=snags.bm) +
  geom_point() +
  theme_bw()

ggplot(aes(x=DBH_cm, y=snag_kg), data=snags.bm) +
  geom_point() +
  geom_point(aes(y=snag_brown), color="red") +
  geom_point(aes(y=snag_gholz),color="blue") +
  theme_bw()

ggplot(aes(x=DBH_cm, y=snag_kg, color=Species), data=snags.bm) +
  geom_point() +
  theme_bw()
  
### join with count data, summarize at plot level
snags.tot <- snags.bm %>%
  group_by(Plot_code,Species) %>%
  # average measurement
  summarise(snag_kg = mean(snag_kg)) %>%
  full_join(count.snags, by=c("Plot_code","Species")) %>%
  # PF snag not measured in Clover_LeHardy_1_1_long_2, so add PF to RF counts
  mutate(stems_ha = ifelse(Plot_code=="Clover_LeHardy_1_1_long_2" & Species=="RF",stems_ha + 66.6667,stems_ha)) %>%
  mutate(snag_total_kg = snag_kg * stems_ha) %>%
  # sum across all snags, convert to Mg/ha
  group_by(Plot_code) %>%
  summarise(snag_Mg_ha = 10*(sum(snag_total_kg, na.rm=TRUE)/10000))

# number of snags sampled by type
snags.nsample <- snags.bm %>%
  group_by(Plot_code,Species) %>%
  tally()

# snag biomass by size class
snags.size <- snags.bm %>%
  # add size class
  mutate(size_class = ifelse(DBH_cm < 0.6, "size_1h",
                             ifelse(DBH_cm>=0.6 & DBH_cm < 2.5, "size_10h",
                                    ifelse(DBH_cm>=2.5 & DBH_cm < 7.6,"size_100h",
                                           "size_1000h")))) %>%
  # add number of sampled snags for a plot
  left_join(snags.nsample, by=c("Plot_code","Species")) %>%
  # add total plot counts, correct count for Clover LeHardy
  left_join(count.snags, by=c("Plot_code","Species")) %>%
  mutate(stems_ha = ifelse(Plot_code=="Clover_LeHardy_1_1_long_2",stems_ha + 66.6667,stems_ha)) %>%
  # divide stem count by n represented
  mutate(stems_ha = stems_ha/n) %>%
  # calculate biomass
  mutate(snag_total_kg = snag_kg * stems_ha) %>%
  # sum across snags by size class, convert to Mg/ha
  group_by(Plot_code,size_class) %>%
  summarise(snag_Mg_ha = 10*(sum(snag_total_kg, na.rm=TRUE)/10000))

# check against sums
snags.ck <- snags.size %>%
  group_by(Plot_code) %>%
  summarise(snag_ck = sum(snag_Mg_ha)) %>%
  right_join(snags.tot, by="Plot_code")

ggplot(aes(x=snag_Mg_ha, y=snag_ck),data=snags.ck) +
  geom_point() +
  theme_bw() # good

### main output
snags.out <- snags.size %>%
  pivot_wider(id_cols=c(Plot_code),names_from=size_class,values_from=snag_Mg_ha) %>%
  rename(snag_1000h_Mg_ha = size_1000h,
         snag_100h_Mg_ha = size_100h,
         snag_10h_Mg_ha = size_10h,
         snag_1h_Mg_ha = size_1h) %>%
  # add stands with no snags, replace NAs with 0
  right_join(snags.tot, by=c("Plot_code")) %>%
  mutate_if(is.numeric, replace_na, replace=0) %>%
  rename(snag_total_Mg_ha=snag_Mg_ha) %>%
  # reorder columns
  dplyr::select(Plot_code,snag_1h_Mg_ha,snag_10h_Mg_ha,snag_100h_Mg_ha,snag_1000h_Mg_ha,snag_total_Mg_ha)

####
# 6. litter and duff
####

### estimate biomass from average depth
# duff equation from brown et al. 1982
# w = 3,630 * Bd, B for pico is 8.7
# litter bulk density from nelson et al 2016

litt.plot <- litt.in %>%
  group_by(Plot_code) %>%
  summarise(Litter_cm = mean(Litter_cm),
            Duff_cm = mean(Duff_cm)) %>%
  # english units for duff
  mutate(Duff_in = Duff_cm / 2.54) %>%
  # litter bulk density from nelson et al. 2016
  # first calculate volume in m3/ha, then multiply by bulk density in kg/m3
  mutate(litter_kg = 10000*(Litter_cm / 100) * 50.2,
         # convert to Mg/ha
         litter_Mg_ha = litter_kg * 0.001) %>%
  # duff loading equation
  mutate(duff_lb_ac = 3630 * 8.7 * Duff_in,
         # convert to Mg/ha
         duff_tons_ac = duff_lb_ac/2000,
         duff_Mg_ha = duff_lb_ac * 0.0004536 * 2.471)

summary(litt.plot)
# retain selected columns
litt.out <- litt.plot %>%
  dplyr::select(Plot_code,litter_Mg_ha,duff_Mg_ha)

####
# 7. downed woody debris
####

### downed wood
# source: Brown 1974

# 3 tallies for 1, 10, 100 hr fuels
# 1h: w[tons/acre] = 0.09533 * nc / (N * l) [ft]
# 10h: w[tons/acre] = 1.825 * nc / (N * l) [ft]
# 100h: w[tons/acre] = 14.52 * nc / (N * l) [ft]
# c = slope correction factor. c = sqrt(1 + (% slope/100)^2)
# N * l = length of sampling plane, with N as # of transects and l as length of each
# convert to Mg/ha: constant * 0.3048 [1/m to 1/ft] * 2.242 [tons/acre to Mg/ha]
0.09533*0.3048*2.242 # 0.06514
1.825*0.3048*2.242 # 1.247
14.52*0.3048*2.242 # 9.922

# 2 separate tallies, one for sound wood (classes 1-3) with specific gravity = 0.4,
# one for rotten (classes 4-5) with specific gravity = 0.3
# sound wood: w [tons/acre] = 4.656 * sum(dbh^2) [in^2] * c / N * l [ft]
# rotten wood: w = 3.492 * sum(dbh^2) * c / N * l
# where: w = weight of downed woody material
# dbh = dbh
# c = slope correction factor. c = sqrt(1 + (% slope/100)^2)
# N * l = length of sampling plane, with N as # of transects and l as length of each
# constant incorporates differences in specific gravity between sound and rotten

# I have converted these to metric units so that
# sound: w [Mg/ha] = 0.493 * sum(dbh^2) [cm^2] * c / N * l [m]
# rotten: w [Mg/ha] = 0.370 * sum(dbh^2) [cm^2] * c / N * l [m]
# this is constant * 0.155 [cm^2 to in^2] * 0.3048 [1/m to 1/ft] * 2.242 [tons/acre to Mg/ha]

# convert slope degrees to radians
deg2rad <- function(deg) {(deg*pi)/180}

# 1h, 10h, 100h from tallies
surf.bm <- surf.in %>%
  left_join(plots.in,by="Plot_code") %>%
  group_by(Plot_code,Fire_interval,Slope_deg,Elev_m,Fuel_type) %>%
  # tally by size class
  summarise(Count=sum(Count)) %>%
  # derive slope in percent, calculate correction
  mutate(Slope_pct = tan(deg2rad(Slope_deg))*100) %>%
  mutate(c=sqrt(1+(Slope_pct/100)^2)) %>%
  pivot_wider(names_from="Fuel_type",values_from="Count") %>%
  # calculate biomass, intercepts of different length
  mutate(bm_1h_Mg_ha = (0.06514*`1h`*c)/(5*3),
         bm_10h_Mg_ha = (1.247*`10h`*c)/(5*3),
         bm_100h_Mg_ha=(9.922*`100h`*c)/(5*10)) %>%
  ungroup() %>%
  dplyr::select(-c(Slope_deg:`1h`))

# 1000h from tallies and diameters
cwd.bm <- cwd.in %>%
  left_join(plots.in, by="Plot_code") %>%
  # assign sound or rotten status
  mutate(Sound = ifelse(Class %in% c(4:5),"Rotten","Sound"),
         Diam_cm2 = Diam_cm^2) %>%
  # slope in pct, calculate correction
  mutate(Slope_pct = tan(deg2rad(Slope_deg))*100) %>%
  mutate(c=sqrt(1+(Slope_pct/100)^2)) %>%
  group_by(Plot_code,Fire_interval,Sound,c) %>%
  # sum CWD diameters^2
  summarise(Diam_cm2=sum(Diam_cm2)) %>%
  # calculate biomass
  pivot_wider(names_from="Sound",values_from="Diam_cm2") %>%
  mutate(bm_1000h_sound_Mg_ha = (0.493 * Sound * c)/(5*20),
         bm_1000h_rotten_Mg_ha = (0.370*Rotten*c)/(5*20)) %>%
  ungroup() %>%
  dplyr::select(-c(c:Sound)) %>%
  mutate(bm_1000h_sound_Mg_ha=replace_na(bm_1000h_sound_Mg_ha,0),
         bm_1000h_rotten_Mg_ha=replace_na(bm_1000h_rotten_Mg_ha,0))

surf.out <- surf.bm %>%
  left_join(cwd.bm, by=c("Plot_code","Fire_interval")) %>%
  dplyr::select(-Fire_interval)

summary(surf.out)

####
# 8. biomass and fuels output
####

# full set with measurements taken at all plots
bm.full <- plotinfo.in %>%
  dplyr::select(c(Plot_code,Fire_interval)) %>%
  # sort by plot name
  arrange(Plot_code) %>%
  left_join(conifer.out, by="Plot_code") %>%
  left_join(potr.out, by="Plot_code") %>%
  left_join(saps.out, by="Plot_code") %>%
  left_join(snags.out, by="Plot_code") %>%
  rename(conifer_total_Mg_ha = total_Mg_ha) %>%
  # replace nas with 0
  mutate(across(where(is.numeric), ~replace_na(.,0)))

# add measurements taken only at subset of plots, leave NAs
bm.all <- bm.full %>%
  left_join(shrubs.out, by="Plot_code") %>%
  left_join(litt.out, by="Plot_code") %>%
  left_join(surf.out, by="Plot_code")

summary(bm.all)

write.csv(bm.all, "processed_data/biomass_fuels/biomass_fuels.csv", row.names=FALSE)

