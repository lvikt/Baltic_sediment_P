# Data prep
rm(list = ls())

# load packages/libraries
library(plyr)     # install.packages('dplyr')   for data manipulation
library(dplyr)    # install.packages('dplyr')   for data manipulation 
library(tidyr)    # install.packages('tidyr')   for data manipulation 
library(stringr)  # install.packages('stringr') for string manipulation

## DATA --------------
# reading data and adding needed columns (position, bottom depth).
# each data source needs to contain, sediment depth, water depth, position, TP concentration.
Ahlgren_etal_2006 <- read.csv(file = 'Ahlgren_etal_2006.csv')
Ahlgren_etal_2006 = mutate(Ahlgren_etal_2006, degW = 58 + 42/60 + 936.68/3600 , degE = 17 + 56/60 + 935.37/3600, Depth = 91.5)

station_info <- read.table(file = 'Station_info.csv',
                                        sep = ",", skip= 1, na.strings = "NA", header = TRUE)

Slomp_etal2013_Pfractions <- read.table(file = 'Slomp_etal_2013_TableS4_P-fractions.csv',
                                        sep = ",", skip= 1, na.strings = "NA", header = TRUE)
Slomp_etal2013_positions <- read.table(file = 'Slomp_etal_2013_positions.csv',
                                        sep = ",", skip= 1, na.strings = "NA", header = TRUE)
Mort_etal2010_Pfractions <- read.table(file = 'Mort_etal_2010_P-fractions.csv',
                                        sep = ",", skip= 1, na.strings = "NA", header = TRUE)
Jilbert_etal2011_Pfractions <- read.table(file = 'Jilbert_etal_2011_TableB1_P-fractions.csv',
                                       sep = ",", skip= 1, na.strings = "NA", header = TRUE)

test <- full_join(Slomp_etal2013_Pfractions, Mort_etal2010_Pfractions)
test <- full_join(Jilbert_etal2011_Pfractions, test)
sapply(test, class)

## P-content -------------
# Calculating P content
# constant dry sediment density, g/cm3 for dry sediment. NB! NOT the same as dry bulk density!
p = 2.55

# Inventory = [layer thickness * conc in umol/g] / dry sed density. Unit umol/cm2
# Inventory: umol/cm2 * 31 *10^-6 * 10^10 = 31*10^4 g/km2 * 10^-6 = 31*10^-2 ton/km2
# Slomp2013 = mutate(Slomp_etal2013_Pfractions, TP_inv = (thickness * Tot_P) / p )
# Slomp2013 = Slomp2013 %>% group_by(station) %>% mutate(TP_inv_0to4cm = cumsum(TP_inv))

tmp = select(station_info, station, lat, long, bottom_depth, SAR)
tmp = merge(test, tmp, by = "station")
test = 
  tmp %>% 
  mutate(Tot_P_interp = na.approx(Tot_P), Org_P_interp = na.approx(Org_P))  %>% 
  mutate(TP_inv = (thickness * Tot_P_interp) / p, ref= 'Slomp_etal2013', OrgP_inv = (thickness * Org_P_interp) / p) %>% 
  arrange(station, sed_depth) %>%
  group_by(station) %>% 
  mutate(TP_tonperkm2_inv_0to4cm = cumsum(TP_inv)*31*10^-2, OrgP_tonperkm2_inv_0to4cm = cumsum(OrgP_inv)*31*10^-2) %>%
  select(station, lat, long, sed_depth, TP_inv, TP_tonperkm2_inv_0to4cm, OrgP_tonperkm2_inv_0to4cm, bottom_depth, SAR, ref) %>%
  filter(sed_depth <= 4 & sed_depth >= 3.5)

tmp = select(Slomp_etal2013_positions, station, lat, long, bottom_depth, SAR)
tmp = merge(Slomp_etal2013_Pfractions, tmp, by = "station")
Slomp_etal2013 = 
  tmp %>% 
  mutate(Tot_P_interp = na.approx(Tot_P), Org_P_interp = na.approx(Org_P))  %>% 
  mutate(TP_inv = (thickness * Tot_P_interp) / p, ref= 'Slomp_etal2013', OrgP_inv = (thickness * Org_P_interp) / p) %>% 
  arrange(station, sed_depth) %>%
  group_by(station) %>% 
  mutate(TP_tonperkm2_inv_0to4cm = cumsum(TP_inv)*31*10^-2, OrgP_tonperkm2_inv_0to4cm = cumsum(OrgP_inv)*31*10^-2) %>%
  select(station, lat, long, sed_depth, TP_inv, TP_tonperkm2_inv_0to4cm, OrgP_tonperkm2_inv_0to4cm, bottom_depth, SAR, ref) %>%
  filter(sed_depth <= 4 & sed_depth >= 3.5)

tmp = select(station_info, station, lat, long, bottom_depth, SAR)
tmp = merge(Mort_etal2010_Pfractions, tmp, by = "station")
Mort_etal2010 = 
  tmp %>% 
  mutate(Tot_P_interp = na.approx(Tot_P), Org_P_interp = na.approx(Org_P))  %>% 
  mutate(TP_inv = (thickness * Tot_P_interp) / p, ref= 'Mort_etal2010', OrgP_inv = (thickness * Org_P_interp) / p) %>% 
  arrange(station, sed_depth) %>%
  group_by(station) %>% 
  mutate(TP_tonperkm2_inv_0to4cm = cumsum(TP_inv)*31*10^-2, OrgP_tonperkm2_inv_0to4cm = cumsum(OrgP_inv)*31*10^-2) %>%
  select(station, lat, long, sed_depth, TP_inv, TP_tonperkm2_inv_0to4cm, OrgP_tonperkm2_inv_0to4cm, bottom_depth, SAR, ref) %>%
  filter(sed_depth <= 4 & sed_depth >= 3.5)

tmp = select(station_info, station, lat, long, bottom_depth, SAR)
tmp = merge(Jilbert_etal2011_Pfractions, tmp, by = "station")
Jilbert_etal2011 = 
  tmp %>% 
  mutate(Tot_P_interp = na.approx(Tot_P), Org_P_interp = na.approx(Org_P))  %>% 
  mutate(TP_inv = (thickness * Tot_P_interp) / p, ref= 'Jilbert_etal2011', OrgP_inv = (thickness * Org_P_interp) / p) %>% 
  arrange(station, sed_depth) %>%
  group_by(station) %>% 
  mutate(TP_tonperkm2_inv_0to4cm = cumsum(TP_inv)*31*10^-2, OrgP_tonperkm2_inv_0to4cm = cumsum(OrgP_inv)*31*10^-2) %>%
  select(station, lat, long, sed_depth, TP_inv, TP_tonperkm2_inv_0to4cm, OrgP_tonperkm2_inv_0to4cm, bottom_depth, SAR, ref) %>%
  filter(sed_depth <= 4 & sed_depth >= 3.5)

All=rbind(Mort_etal2010,Jilbert_etal2011,Slomp_etal2013)

## Saving as csv files ----------


# write.csv(All, file = "P_inv.csv",)


## Plotting ------------

plot(All$bottom_depth,All$tonP_perkm2_TP_inv_0to4cm, xlim = c(0, 500), ylim = c(0, 40))
points(All$bottom_depth,All$tonP_perkm2_OrgP_inv_0to4cm, col = 'blue')

plot(All$SAR,All$tonP_perkm2_TP_inv_0to4cm, xlim = c(0, 5000), ylim = c(0, 40))
points(All$SAR,All$tonP_perkm2_OrgP_inv_0to4cm, col = 'blue')

# plot(Mort_etal2010$bottom_depth,Mort_etal2010$TP_inv_0to4cm, xlim = c(0, 500), ylim = c(0, 200))
# points(Jilbert_etal2011$bottom_depth,Jilbert_etal2011$TP_inv_0to4cm)
# points(Slomp_etal2013$bottom_depth,Slomp_etal2013$TP_inv_0to4cm)
# 
# plot(Mort_etal2010$SAR,Mort_etal2010$TP_inv_0to4cm, xlim = c(0, 5000), ylim = c(0, 200))
# points(Jilbert_etal2011$SAR,Jilbert_etal2011$TP_inv_0to4cm)
# points(Slomp_etal2013$SAR,Slomp_etal2013$TP_inv_0to4cm)
