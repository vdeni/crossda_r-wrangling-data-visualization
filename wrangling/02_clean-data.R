###########################
##### The R libraries #####
###########################

# We'll be using some additional libraries here. The first one is {stringr},
# which has a large set of functions for working with character strings.

library(stringr)

library(here)
library(haven)
library(dplyr)

data_waves_merged <- haven::read_sav(here::here('data',
                                                'data_waves-merged.sav'))
