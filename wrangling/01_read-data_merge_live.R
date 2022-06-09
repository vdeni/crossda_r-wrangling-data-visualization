library(here)
library(dplyr)
library(haven)

path_1 <- here('data',
               'GGS_Wave1_France_V.4.4.sav')

path_2 <- here('data',
               'France_Wave2_V.1.3.sav')

path_3 <- here('data',
               'GGS_Wave3_France_V.1.0.sav')

d_wave_1 <- read_sav(path_1)

read_sav(file = 'data/GGS_Wave1_France_V.4.4.sav')

d_wave_2 <- read_sav(path_2)

d_wave_3 <- read_sav(path_3,
                     encoding = 'latin1')

glimpse(d_wave_1)

d_wave_1_2 <- inner_join(d_wave_1,
           d_wave_2,
           by = c('arid' = 'brid'))

d_merged <- inner_join(d_wave_1_2,
                       d_wave_3,
                       by = c('arid' = 'crid'))

write_sav(d_merged,
          here('data',
               'data_waves-merged.sav'))
