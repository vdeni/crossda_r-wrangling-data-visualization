library(here)
library(dplyr)
library(haven)
library(stringr)

d_merged <- read_sav(here('data',
                          'data_waves-merged.sav'))

d_merged$arid

dplyr::select(d_merged,
              aint_st:arid,
              'aage',
              ayear)

. <- select(d_merged,
            1:10)

select(d_merged,
       ends_with('09'))

select(d_merged,
       contains('221'))

select(d_merged,
       matches('\\d$'))

haven::print_labels(d_merged$acountry)

# get label for variable
attr(d_merged$acountry, 'label')

filter(d_merged,
       aage %in% 47:50)

filter(d_merged,
       aage %in% 47:50 & asex == 1)

d_younger_than_20 <- filter(d_merged,
       aage < 20)

filter(d_merged,
       aage >= 20)

d_merged$age_2 <- d_merged$ayear - d_merged$abyear

d_merged$age_2 == d_merged$aage

sum(d_merged$age_2 == d_merged$aage)

nrow(d_merged)

d_merged <- mutate(d_merged,
       age_3 = ayear - abyear)

sum(d_merged$age_2 == d_merged$age_3)

. <- select(d_merged,
            ends_with('age'))

. <- mutate(.,
            across(starts_with('a'),
                   ~ .x^2))
. <- mutate(.,
            across(contains('marriage'),
                   ~ .x + 3))

. <- mutate(.,
            across(starts_with('a'),
                   sqrt))

. <- mutate(.,
       across(starts_with('a'),
              list('_sqr' = ~ .x^2,
                   '_sqrt' = sqrt)))

. <- mutate(.,
            across(contains('age'),
                   list('_sqr' = ~ .x^2,
                        '_sqrt' = sqrt)))

summarise(d_merged,
           mean_aage = mean(aage))

summarise(d_merged,
          mean_aage = mean(aage),
          sd_aage = sd(aage))

summarise(d_merged,
          across(contains('age'),
                 list('m' = mean,
                      'sd' = sd)))

. <- group_by(d_merged,
              asex)

summarise(.,
          mean_aage = mean(aage))

. <- group_by(d_merged,
              bsex,
              beduc)

means <- summarise(.,
          mean_bage = mean(bage))

as.data.frame(means)
