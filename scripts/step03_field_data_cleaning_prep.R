#####
#
## field plot data inspection, cleaning, and prep
#
#####

# wd inherited from project

# load libraries
library(openxlsx)
library(tidyverse)
library(sf)

####
# 1. general plot data
####

### plot_info sheet

plotinfo.in  <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                   sheet="Plot_info",
                   colNames=TRUE) %>%
  # add column for each unique reburn
  mutate(Fire_combo = paste0(Fire_1,"_",Fire_2)) %>%
  # change various columns to factors
  mutate_at(c("Fire_1","Fire_2","Fire_interval","Fire_combo"),factor) 

str(plotinfo.in)
summary(plotinfo.in)
summary(plotinfo.in$Fire_combo)
length(unique(plotinfo.in$Fire_combo))

###  general info on plots sampled in 2021

# 16 unique reburns, 22 plot pairs, 44 total plots
# FRI 12-28 years
# TSF 3-27 years
# fuels only measured in 21 plot pairs (not in Wilcox_Berry due to time constraints)
# shrub cover & ht (understory) only measured in 20 plot pairs (not in Huck_Berry due to NK unavailable)

# reviewed plot_id_comments

### GPS sheet

gps.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                        sheet="GPS",
                        colNames=TRUE)

str(gps.in)


### general_plot_measurements sheet

plots.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                   sheet="General_plot_measurements",
                   colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("Fire_interval"),factor) 

str(plots.in)
summary(plotinfo.in$Fire_interval)  # 22 each
summary(plots.in)
hist(plots.in$Aspect_deg)
hist(plots.in$Slope_deg)
hist(plots.in$Elev_m)

# 11 NAs for unburned edge dist, 6 NAs for seed source dist
# distance to seed source for NA plots will be estimated from GIS data

# check that final GPS coords match easting and northing
gps.ck <- plots.in %>%
  left_join(gps.in, by="Plot_code") %>%
  mutate(Northing_diff = Northing - Final_northing,
         Easting_diff = Easting - Final_easting)

summary(gps.ck) # 0s, looks good

####
# 2. tallies
####

### counts datasheet

counts.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                     sheet="Tallies",
                     colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("T","Stem_type","Species"),factor) 

length(unique(counts.in$Plot_code)) # 44
summary(counts.in) # no count nas
summary(counts.in$Stem_type) # all make sense, just the 1 category (Tree_ramets_dead) with 1 obs
summary(counts.in$Species) # all make sense

# which plots had pre_fire survivors?
counts.in %>%
  filter(Stem_type %in% c("Sapling_prefire","Tree_prefire")) # 4 plots

# all plot names match up with general plot measurements?
counts.chk <- plots.in %>%
  full_join(counts.in, by="Plot_code")

summary(counts.chk)

### some initial plotting to identify any potential data entry errors

# quick look at data, stem type x species
counts.in %>% 
  ggplot(aes(x=Species, y= Count)) +
  facet_wrap(~Stem_type) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw() 

# quick look at data, stem type x species x fire interval
counts.chk %>% 
  ggplot(aes(x=Species, y= Count, color=Fire_interval)) +
  facet_wrap(~Stem_type) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw()

# look at ratio of RF to PF by fire interval, any long-intervals with more PF?
counts.plot <- counts.chk %>%
  group_by(Plot_code,Fire_interval,Species) %>%
  summarise_at(c("Count"), sum) 

counts.plot %>% 
  pivot_wider(names_from=Species,values_from=Count) %>%
  mutate(RF_PF_ratio = (RF-PF)/(RF+PF)) %>%
  filter(RF_PF_ratio<0) # 5 plots with fewer RF than PF, short-interval okay
# look at long-interval plots: Astringent_Tern_1_2_long_2 and NFork_Owl_2_1_long_1
# NFork TSF is 14 years, similar #s PF and RF
# Astringent 27 years TSF, just the 1 PF, no other snags

# quick look at overall trends by long v short interval, plotting count + 1 to be on log scale
# create filled counts to include 0 values for species not recorded by default
counts.filled <- counts.plot %>%
  ungroup() %>%
  expand(Plot_code,Fire_interval,Species) %>%
  left_join(counts.plot, by=c("Plot_code","Fire_interval","Species")) %>%
  mutate(Count = replace_na(Count,0)) 

# plot
counts.filled %>%
  ggplot(aes(x=Species, y= Count+1, color=Fire_interval)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw()

### cleaned plot-level post-fire tree recovery tallies, standing dead stems
# Tallies represent post-fire tree recovery and include live and dead seedlings, saplings, and trees of focal species: ABLA, PIAL, PICO, PIEN, POTR, and PSME. POTR ramets are counted as individual stems. Dead tallies are not classified by species. 
# Also include tallies for standing dead snags from the most recent fire (RF) and dead prior to the most recent fire (PF).
# Tallies exclude prefire survivors, POTR clusters (because individual stems are included in ramet counts), SALIX counts. The 4 plots with live trees that survived most recent fire include 2 short and 2 long interval.

# cleaned version for post-fire recovery
counts.out <- counts.in %>%
  mutate_at(c("Stem_type","Species"),as.character) %>%
  # filter by stem type and species
  filter(Stem_type %in% c("Sapling_gt_2","Sapling_gt_2_ramets","Seedling_lt_2","Snag","Tree","Tree_ramets","Tree_ramets_dead"),
         ! Species %in% "SALIX") %>%
  # recode ramets to group with stem counts
  mutate(Stem_type = recode(Stem_type, Sapling_gt_2_ramets="Sapling_gt_2",Tree_ramets="Tree",Tree_ramets_dead="Tree")) %>%
  # aggregate to plot level
  group_by(Plot_code,Stem_type,Species) %>%
  summarise_at("Count",sum) %>% 
  ungroup()
  
# create filled version for all stem type and species combinations
counts.outfilled <- counts.out %>%
  # expand to include entries for all species combinations that appear in data
  # nesting excludes combos that never appear, like RF trees
  expand(Plot_code,nesting(Stem_type,Species)) %>%
  # join to data
  left_join(counts.out, by=c("Plot_code","Stem_type","Species"))  %>%
  # replace NAs with 0s
  mutate(Count = replace_na(Count,0)) %>%
  # convert to stems/ha, 3 transects that were 50 m x 2 m (100m2), total 300m2 sampled, 10000m2/300m2
  mutate(stems_ha = Count * 10000/300)

summary(counts.outfilled)

# export
write.csv(counts.outfilled, "data/field_plots_2021/cleaned_data/tallies_postfire_cleaned_filled.csv", row.names = FALSE)

####
# 3. sapling measurements
####

### sapling measurements datasheet

saps.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                    sheet="Saplings",
                    colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("T","Species"),factor) 

length(unique(saps.in$Plot_code)) # 44
summary(saps.in) # no height NAs
summary(saps.in$Species) # all make sense
# any taller than breast height?
saps.in %>% filter(Height_m>=1.4) # 4 POTR appear to have accidentally been measured as if saplings (ht only)
# no trees tallied in either plot, so just include in average sapling height
unique(saps.in$Notes)

# all plot names match up with general plot measurements?
saps.chk <- plots.in %>%
  full_join(saps.in, by="Plot_code")

summary(saps.chk)

# do all tallies have associated heights?
counts.saps <- counts.chk %>%
  # include POTR ramet counts
  mutate(Stem_type = as.character(Stem_type),
         Stem_type = ifelse(Stem_type=="Sapling_gt_2_ramets","Sapling_gt_2",Stem_type)) %>%
  group_by(Plot_code,Fire_interval,Species,Stem_type) %>%
  summarise_at(c("Count"), sum) 

saps.htchk <- saps.in %>%
  group_by(Plot_code,Species) %>%
  summarise_at(c("Height_m"),mean) %>%
  mutate(Stem_type = "Sapling_gt_2") %>%
  right_join(counts.saps, by=c("Plot_code","Stem_type","Species")) %>%
  filter(Stem_type %in% c("Sapling_gt_2","Sapling_prefire"), Count > 0)

summary(saps.htchk) #7 NAs

saps.htchk %>% 
  filter(is.na(Height_m))
# can ignore prefire. NA values will be addressed in fuels/biomass calculations

# initial plot check
saps.htchk %>%
  ggplot(aes(x=Species, y=Height_m, color=Fire_interval)) +
  geom_boxplot() +
  theme_bw()

####
# 4. tree and snag measurements
####

### tree/snag measurements datasheet

trees.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                      sheet="Trees_snags",
                      colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("T","Species"),factor) 

length(unique(trees.in$Plot_code)) # 44

### examine data for any errors, data cleaning needs
summary(trees.in) # 2 DBH NAs, no height NA

trees.in %>%
  filter(is.na(DBH_cm)) # 2 entries, 1 ABLA, 1 POTR

trees.in %>%
  filter(Stem_type%in% c("Tree","Tree_prefire") & is.na(CBH_m)) # 2 entries, 1 ABLA, 1 POTR

summary(trees.in$Species) # all make sense
# any shorter than breast height?
trees.in %>% filter(Height_m<1.4) # 2 snags, 2 PICO in Mystic_BearpawBay_2_3_long_3
# can exclude these when calculating plot-level average values

unique(trees.in$Notes) # will need to make assumptions to fill in missing data, deal with broken/dead tops, include/exclude survivors from most recent fire in counts or fuels

trees.in %>%
  filter(!is.na(Notes))

# all plot names match up with general plot measurements?
trees.chk <- plots.in %>%
  full_join(trees.in, by="Plot_code")

summary(trees.chk)

# do all tallies have associated heights?
counts.trees <- counts.chk %>%
  # include POTR ramet counts
  mutate(Stem_type = as.character(Stem_type),
         Stem_type = ifelse(Stem_type=="Tree_ramets","Tree",Stem_type)) %>%
  group_by(Plot_code,Fire_interval,Species,Stem_type) %>%
  summarise_at(c("Count"), sum) 

trees.htchk <- trees.in %>%
  group_by(Plot_code,Stem_type,Species) %>%
  summarise_at(c("Height_m"),mean) %>%
  right_join(counts.trees, by=c("Plot_code","Stem_type","Species")) %>%
  filter(Stem_type %in% c("Tree","Snag","Tree_prefire"), Count > 0)

summary(trees.htchk) #4 NAs

trees.htchk %>% 
  filter(is.na(Height_m))
# 2 deads, can use avg live ht to estimate dead height, group PF with RF snags in Clover_LeHardy_1_1_long_2, omit prefire from calculations.

# check that trees and snags coded correctly
trees.in %>%
  filter(Stem_type=="Snag") %>%
  group_by(Species) %>%
  tally()

trees.in %>%
  filter(Stem_type=="Tree") %>%
  group_by(Species) %>%
  tally()

# check astringent heights
trees.in %>%
  filter(Plot_code == "Astringent_Tern_1_2_long_2") %>%
  filter(Stem_type=="Tree") %>%
  summary() # mean DBH 6.7, mean ht 4.4
# turner et al. 2016: for 24-year-old trees mean stand height was 0.4-5.7 m and mean stand DBH was 0.9-10.1. We are well within that range here

### some initial plotting
# look at ht x dbh plots by species
trees.in %>%
  ggplot(aes(x=DBH_cm, y=Height_m, color=Species, fill=Species)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw()

trees.in %>%
  left_join(plots.in, by="Plot_code") %>%
  ggplot(aes(x=DBH_cm, color=Fire_interval)) +
  facet_wrap(~Species, scales="free") +
  geom_histogram(fill="white") +
  theme_bw()

trees.in %>%
  left_join(plots.in, by="Plot_code") %>%
  ggplot(aes(x=Height_m, color=Fire_interval)) +
  facet_wrap(~Species, scales="free") +
  geom_histogram(fill="white") +
  theme_bw()

# check for other issues with data
# trees where CBH > Height
trees.in %>%
  filter(CBH_m>=Height_m) # none

# missing values and other issues will be dealt with in fuels/biomass calculations

####
# 5. Surface fuel tallies
####

### surface fuels datasheet

fwd.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                     sheet="Surface_fuels",
                     colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("SF","Fuel_type"),factor) 

length(unique(fwd.in$Plot_code)) # 44
summary(fwd.in) # 2 plots measured 8 SF transects, 2 plots no fuels measured so excluded from surface fuels analyses

# randomly assign 5 SF transects to surface fuels for 2 plots where 8 transects originally measured, seed chosen randomly
set.seed(643)
sample(1:8, 5, replace=FALSE) # 7, 4, 6, 2, 3

# read in 2 missing plots
fwd.orig <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                   sheet="Surface_fuels_original",
                   colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("SF","Fuel_type"),factor) %>%
  filter(SF %in% c(2,3,4,6,7))

summary(fwd.orig)

# add to fwd.in
fwd.out <- fwd.in %>%
  filter(!Plot_code %in% c("Mystic_BearpawBay_1_1_short_0","Mystic_BearpawBay_1_1_long_1")) %>%
  rbind(fwd.orig) %>%
  # remove Wilcox_Berry plots with no measurements
  filter(!is.na(SF)) %>%
  # change count to numeric
  mutate_at("Count",as.numeric) 

summary(fwd.out)

# quick look
fwd.out %>%
  full_join(plots.in, by="Plot_code") %>%
  filter(!is.na(SF)) %>%
  mutate_at("Count",as.numeric) %>%
  group_by(Plot_code,Fire_interval,Fuel_type) %>%
  summarise_at(c("Count"),sum) %>%
  ggplot(aes(x=Fuel_type,y=Count,color=Fire_interval)) +
  geom_boxplot() +
  theme_bw()

# write out
write.csv(fwd.out, "data/field_plots_2021/cleaned_data/surface_fuels_cleaned_compiled.csv",row.names=FALSE)

####
# 6. Litter and duff
####

### litter and duff datasheet

ld.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                   sheet="Litter_duff",
                   colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("SF","Nbr"),factor) 

length(unique(ld.in$Plot_code)) # 44
summary(ld.in) # 2 plots need meas, 2 plots no fuels measured

# use same SF transects as above 

# read in 2 missing plots
ld.orig <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                     sheet="Litter_duff_original",
                     colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("SF","Nbr"),factor) %>%
  filter(SF %in% c(2,3,4,6,7))

summary(ld.orig)

# add to ld.in
ld.out <-ld.in %>%
  filter(!Plot_code %in% c("Mystic_BearpawBay_1_1_short_0","Mystic_BearpawBay_1_1_long_1")) %>%
  rbind(ld.orig) %>%
  # remove Wilcox_Berry plots with no measurements
  filter(!is.na(SF)) %>%
  # change count to numeric
  mutate_at(c("Litter_cm","Duff_cm"),as.numeric) %>%
  # remove 1 NA value where litter and duff not measured, simply exclude since plot level values will be averaged
  filter(!is.na(Litter_cm))

summary(ld.out)

# quick look
ld.out %>%
  full_join(plots.in, by="Plot_code") %>%
  filter(!is.na(SF)) %>%
  group_by(Plot_code,Fire_interval) %>%
  summarise_at(c("Litter_cm","Duff_cm"),mean) %>%
  pivot_longer(cols=c(Litter_cm,Duff_cm),values_to="Depth_cm") %>%
  ggplot(aes(x=name,y=Depth_cm,color=Fire_interval)) +
  geom_boxplot() +
  theme_bw()

# write out
write.csv(ld.out, "data/field_plots_2021/cleaned_data/litter_duff_cleaned_compiled.csv",row.names=FALSE)


####
# 7. CWD
####

### CWD datasheet

cwd.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                  sheet="CWD",
                  colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("SF"),factor) 

length(unique(cwd.in$Plot_code)) # 44
summary(cwd.in) # 2 plots need meas, 2 plots no fuels measured

# use same SF transects as above 

# read in 2 missing plots
cwd.orig <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                    sheet="CWD_original",
                    colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("SF"),factor) %>%
  filter(SF %in% c(2,3,4,6,7))

summary(cwd.orig)

# add to cwd.in
cwd.out <-cwd.in %>%
  filter(!Plot_code %in% c("Mystic_BearpawBay_1_1_short_0","Mystic_BearpawBay_1_1_long_1")) %>%
  rbind(cwd.orig) %>%
  # remove Wilcox_Berry plots with no measurements
  filter(!is.na(SF)) %>%
  # change count to numeric
  mutate_at(c("Intersect_begin","Intersect_end","Diam_cm","Class"),as.numeric) %>%
  mutate_at(c("Position","Multi_intersect"),factor) %>%
  # order ascending
  arrange(Plot_code, SF, Intersect_begin,Intersect_end)

summary(cwd.out)

# all NA Intersect_end coded as Multi_intersect
cwd.out %>%
  filter(is.na(Intersect_end)) %>%
  summary()

# ensure no missed intersection overlaps

cwd.ck <- cwd.out %>%
  # remove multi-intersects
  filter(!is.na(Intersect_end))


# loop through each row, except last one because na
for(i in 1:(dim(cwd.ck)[1]-1)) {
  # extract end of intersect in given row plus 15 cm
  # and beginning of intersect in next row
  i.end = round(cwd.ck[i,"Intersect_end"],2)
  i.next = round(cwd.ck[i+1,"Intersect_begin"],2)
  
  # only compare when in same plot and transect
  # if intersect + 15 cm overlaps, print error message
  if(cwd.ck[i,"SF"] == cwd.ck[i+1,"SF"] & 
     cwd.ck[i,"Plot_code"] == cwd.ck[i+1,"Plot_code"] &
     i.end > i.next) {
    print(paste("data record error at",cwd.ck[i,"Plot_code"],cwd.ck[i,"SF"],
                cwd.ck[i,"Intersect_begin"]))
  }
}  # good!

# go through each intercept with multiple entries
# and ensure have the same starting point

cwd.out %>%
  filter(Multi_intersect=="Y") %>%
  group_by(Plot_code,SF) %>%
  summarise(Intersect_diff = mean(round(Intersect_begin,2)/round(Intersect_begin,2))) %>%
  summary() # good!

# examine whether cover estimates might be low due to 
# measurement methods issues
# i.e., when intercept is less than diameter
# only using first diameter if multi-intersect
head(cwd.ck)

cwd.dia <- cwd.ck %>%
  mutate(Intersect_length = (Intersect_end - Intersect_begin) * 100) %>%
  mutate(Intersect_ratio = Intersect_length/Diam_cm)

summary(cwd.dia) # most > 1

summary(cwd.dia[cwd.dia$Intersect_ratio<0.8,])  # 16 with especially problematic measurements

cwd.dia %>%
  filter(Intersect_ratio < 0.8) # double checked data sheets

# check very long intersects too
cwd.dia %>%
  filter(Intersect_ratio > 5, Multi_intersect=="N") # double checked data sheets

cwd.plots <- cwd.dia %>%
  group_by(Plot_code) %>%
  summarise(Intersect_ratio_mean = mean(Intersect_ratio),
            Intersect_ratio_median = median(Intersect_ratio))

cwd.plots
summary(cwd.plots)
# when aggregated to plot level, initial glance is that looks like
# nothing is completely off at entire plot level
# so moving on for now

# quick look
cwd.out %>%
  mutate(Intersect_length = Intersect_end - Intersect_begin) %>%
  mutate(Sound = ifelse(Class==3,"Sound","Rotten")) %>%
  group_by(Plot_code,Sound) %>%
  summarise(Diam_cm = mean(Diam_cm),
            CWD_cover_pct = sum(Intersect_length,na.rm=TRUE)) %>% # divide by 100 m sampled, multiple by 100 to get %
  full_join(plots.in, by="Plot_code") %>%
  filter(!is.na(Diam_cm)) %>%
  pivot_longer(cols=c(Diam_cm,CWD_cover_pct),values_to="values") %>%
  ggplot(aes(x=Sound,y=values,color=Fire_interval)) +
    facet_wrap(~name,scales="free") +
    geom_boxplot() +
  theme_bw()

# write out
write.csv(cwd.out, "data/field_plots_2021/cleaned_data/cwd_sorted_compiled.csv",row.names=FALSE)

####
# 8. shrubs
####

### shrub data sheet

shrubs.in <- read.xlsx("data/field_plots_2021/raw_data/SILI_field_data_2021.xlsx", 
                   sheet="Shrubs",
                   colNames=TRUE) %>%
  # change various columns to factors
  mutate_at(c("T","Species"),factor) 

length(unique(shrubs.in$Plot_code)) # 40, no shrubs for Huck_Berry
summary(shrubs.in) # 2 plots with no shrubs in quadrats

unique(shrubs.in$Notes)

# summarise at plot level
shrubs.ht <- shrubs.in %>%
  # weight average height by percent cover in a quadrat
  mutate(Height_wt = Cover_pct * Height_m) %>%
  # remove shrubs with no height recorded, also removes plots where no shrubs present in quadrats
  filter(!is.na(Height_wt)) %>%
  group_by(Plot_code,Species) %>%
  # weighted height is the sum of weighted hts divided by the sum of cover sampled, remove NA values
  summarise(Height_wtavg_m = sum(Height_wt)/sum(Cover_pct))

# total pct cover
shrubs.cov <- shrubs.in %>%
  # assign NA plots SPBE as placeholder shrub, cover_pct is already set to 0
  mutate(Species = as.factor(ifelse(is.na(Species),"SPBE",as.character(Species)))) %>%
  group_by(Plot_code,Species) %>%
  summarise(Cover_pct = sum(Cover_pct)/25) %>% # 25 quadrats sampled per plot
  left_join(shrubs.ht, by=c("Plot_code","Species"))
  
summary(shrubs.cov) # 6 still need height
shrubs.cov %>%
  filter(is.na(Height_wtavg_m)) # AMAL, RIVI, PAMY
# estimate height when calculating biomass

# expand to get complete cover for shrub species measured
shrubs.out <- shrubs.cov %>%
  ungroup() %>%
  # complete cases
  expand(Plot_code,Species) %>%
  full_join(shrubs.cov, by=c("Plot_code","Species")) %>%
  # replace NAs with 0 for cover only, not height
  mutate(Cover_pct = replace_na(Cover_pct,0))

# quick look
shrubs.out %>%
  left_join(plots.in, by="Plot_code") %>%
  ggplot(aes(x=Species,y=Cover_pct,color=Fire_interval)) +
  geom_boxplot() +
  theme_bw()

shrubs.out %>%
  left_join(plots.in, by="Plot_code") %>%
  ggplot(aes(x=Species,y=Height_wtavg_m,color=Fire_interval)) +
  geom_boxplot() +
  theme_bw() +
  theme_bw()

# how about total cover and average height
shrubs.out %>%
  group_by(Plot_code) %>%
  summarise(Cover_pct = sum(Cover_pct,na.rm=TRUE))%>%
  left_join(plots.in, by="Plot_code") %>%
  ggplot(aes(y=Cover_pct,color=Fire_interval)) +
  geom_boxplot() +
  theme_bw() +
  theme_bw()

shrubs.out %>%
  mutate(Height_avg2 = Cover_pct * Height_wtavg_m) %>%
  group_by(Plot_code) %>%
  summarise(Height_wtavg_m = sum(Height_wtavg_m,na.rm=TRUE)/sum(Cover_pct,na.rm=TRUE)) %>%
  left_join(plots.in, by="Plot_code") %>%
  ggplot(aes(y=Height_wtavg_m,color=Fire_interval)) +
  geom_boxplot() +
  theme_bw()


# write output, use shrubs.cov rather than filled version for now
write.csv(shrubs.cov, "data/field_plots_2021/cleaned_data/shrubs_plotlvl.csv",row.names=FALSE)


####
# 9. Schoennagel data, collected in 2000, published 2003
####

# function to transform radians to degrees for slope

rad2deg <- function(rad) {rad * (180 / pi)}

### schoennagel et al. 2003 data
# this data is not included in deposit, and neither are full MTBS fire perimeter data, so this section of code will not run
# but cleaned output data is saved and reloaded for the next section

scho.in <- read.csv("data/Schoennagel_etal2000/SCHOENNAGEL 2000 YNP FIRE INTERVAL_FinalPICO_10.8.2021.csv", header = TRUE) %>%
  # aspect is degrees, need to convert slope to degrees
  mutate(Slope_deg = rad2deg(atan(Slope/100))) %>%
  mutate_at("SiteName", as.character) %>%
  # update some site names
  mutate(SiteName = recode(SiteName, "Heart N" = "Heart Lake N","Madison Jnct" = "Madison Junct"))

# id plots relevant for this study by FRI
scho.sites <- scho.in %>%
  filter(Int<=30) %>%
  mutate_at("SiteName", as.character)

# select just these sites
scho.sel <- scho.in %>%
  filter(SiteName %in% c(scho.sites$SiteName))

# double check TSF, use MTBS to get most recent fire info, sampling year in 1999 for Charcoal Bay and 2000 for all else
mtbs.in <- st_read("data/MTBS/mtbs_perimeter_data/mtbs_perims_DD.shp")

# compare points to fire perimeters
scho.pts <- st_as_sf(scho.sel,
                coords = c("Easting","Northing"),
                crs="+init=epsg:26912") %>%
  st_transform(crs = st_crs(mtbs.in)) %>%
  st_intersection(mtbs.in)
# all fires 1988 except Astringent
# 3 plots have reburned since being sampled in 2000!

# manually add in Fire_year_2, Fire_year_1, TSF
scho.out <- scho.sel %>%
  rename(FRI=Int) %>%
  mutate(Fire_year_2 = ifelse(SiteName == "Astringent",1994,1988),
         Fire_year_1 = Fire_year_2 - FRI,
         TSF = ifelse(SiteName=="Charcoal Bay",1999-Fire_year_2,2000-Fire_year_2)) %>%
  # add fire interval
  mutate(Fire_interval=ifelse(Trt==0,"Short","Long"))

# write out
write.csv(scho.out,"data/field_plots_2021/cleaned_data/Schoennagel_etal2003_cleaned.csv",row.names=FALSE)

####
# 10. write out plot gps points for climate data extraction
####

# read in schoennagel data
scho.out <- read.csv("data/field_plots_2021/cleaned_data/Schoennagel_etal2003_cleaned.csv") 

str(plots.in)
str(scho.out)

scho.gps <- scho.out %>%
  mutate(Plot_code = paste0(SiteName,"_",SiteID)) %>%
  # add fire interval
  dplyr::select(Plot_code,Fire_interval,Easting,Northing)

gps.out <- plots.in %>%
  dplyr::select(Plot_code,Fire_interval,Easting,Northing) %>%
  rbind(scho.gps) %>%
  st_as_sf(coords = c("Easting","Northing"),
           crs="+init=epsg:26912") %>%
  # transform to EPSG 4326
  st_transform(crs="+init=epsg:4326")

final.points <- as.data.frame(gps.out) %>%
  dplyr::select(-geometry) %>%
  cbind(st_coordinates(gps.out))

# write out, in lat lon, EPSG 4326
write.csv(final.points,"processed_data/plot_selection/final_plot_coords_epsg4326.csv", row.names=FALSE)
