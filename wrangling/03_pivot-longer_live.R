library(dplyr)
library(haven)
library(here)
library(tidyr)

data_waves_merged <- haven::read_sav(here::here('data',
                                                'data_waves-merged.sav'))

. <- dplyr::select(data_waves_merged,
                   arid, asex, ayear, amonth,
                   bsex, byear, bmonth,
                   csex, cyear, cmonth)

.$brid <- .$arid
.$crid <- .$arid

d_long <- tidyr::pivot_longer(.,
                    cols = everything(),
                    names_pattern = '(a|b|c)(.*)',
                    names_to = c('wave',
                                 '.value'))

.dumb <- pivot_longer(.,
             cols = everything(),
             names_to = 'name',
             values_to = 'values')

d_long <- tidyr::pivot_longer(.,
                              cols = everything(),
                              names_pattern = '(.*)(_t\\d)$',
                              names_to = c('.value',
                                           'wave'))

tidyr::pivot_wider(.long,
                   id_cols = 'rid',
                   names_from = 'wave',
                   values_from = !matches('rid|wave'),
                   names_glue = '{wave}{.value}')
