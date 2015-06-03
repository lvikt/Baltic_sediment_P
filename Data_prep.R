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
Mort_etal2010_Pfractions <- read.table(file = 'Mort_et_al_2010_P-fractions.csv',
                                        sep = ",", skip= 1, na.strings = "NA", header = TRUE)

## P-content -------------
# Calculating P content
# constant dry sediment density, g/cm3 for dry sediment. NB! NOT the same as dry bulk density!
p = 2.55

# Inventory = [layer thickness * conc in umol/g] / dry sed density. Unit umol/cm2
# Slomp2013 = mutate(Slomp_etal2013_Pfractions, TP_inv = (thickness * Tot_P) / p )
# Slomp2013 = Slomp2013 %>% group_by(station) %>% mutate(cum_TP_inv = cumsum(TP_inv))

tmp = select(Slomp_etal2013_positions, station, lat, long, bottom_depth)
tmp = merge(Slomp_etal2013_Pfractions, tmp, by = "station")
Slomp_etal2013 = 
  tmp %>% 
  mutate(TP_inv = (thickness * Tot_P) / p) %>% 
  arrange(station, sed_depth) %>%
  group_by(station) %>% 
  mutate(cum_TP_inv = cumsum(TP_inv)) %>%
  select(station, lat, long, sed_depth, TP_inv, cum_TP_inv, bottom_depth) %>%
  filter(sed_depth == 3.5)

tmp = select(station_info, station, lat, long, bottom_depth)
tmp = merge(Mort_etal2010_Pfractions, tmp, by = "station")
Mort_etal2010 = 
  tmp %>% 
  mutate(TP_inv = (thickness * Tot_P) / p) %>% 
  arrange(station, sed_depth) %>%
  group_by(station) %>% 
  mutate(cum_TP_inv = cumsum(TP_inv)) %>%
  select(station, lat, long, sed_depth, TP_inv, cum_TP_inv, bottom_depth) %>%
  filter(sed_depth == 3.5)


## Saving as csv files ----------


write.csv(Slomp_etal2013, file = "Slomp_etal2013_P_Inventory.csv",)
write.csv(Mort_etal2010, file = "Mort_etal2010_P_Inventory.csv",)

