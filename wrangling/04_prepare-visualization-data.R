library(dplyr)
library(haven)
library(tidyr)
library(here)

data_waves_merged <- haven::read_sav(here::here('data',
                                                'data_waves-merged.sav'))

data_subset <-
    dplyr::select(data_waves_merged,
                  dplyr::matches('nkids$'),
                  dplyr::matches('(a|b|c)(age|sex|parstat|educ|int_pr|n225)$'),
                  dplyr::matches('401a_[a-g]$|408_[a-i]|.721_?.'),
                  dplyr::matches('(1002|1009|(a|b|c)119|1001_i|1001_j)$'),
                  dplyr::matches('(a|b|c)(145|402|407)$'))

data_subset$an225 <- 1

data_subset <- haven::zap_labels(data_subset)

data_subset <-
    dplyr::rename_with(data_subset,
                       .fn = ~ stringr::str_replace(.x,
                                                    'c721([:lowercase:])',
                                                    'c721_\\1'),
                       .cols = dplyr::matches('c721'))

data_subset_long <- tidyr::pivot_longer(data_subset,
                                        cols = dplyr::everything(),
                                        names_pattern = '^(a|b|c)(.*)',
                                        names_to = c('wave', '.value'))

data_subset_long <- dplyr::rename(data_subset_long,
                                  'pres_oth' = 'int_pr',
                                  'same_partner' = 'n225')

data_subset_long <- dplyr::rename_with(data_subset_long,
                                       .cols = dplyr::matches('401'),
                                       .fn = ~ paste0('inv_',
                                                      .x))

data_subset_long <- dplyr::rename_with(data_subset_long,
                                       .cols = dplyr::matches('408'),
                                       .fn = ~ paste0('dsag_',
                                                      .x))

data_subset_long <- dplyr::rename_with(data_subset_long,
                                       .cols = dplyr::matches('721'),
                                       .fn = ~ paste0('sad_',
                                                      .x))

data_subset_long <- dplyr::rename(data_subset_long,
                                  'n_rooms' = '119',
                                  'hh_ends_meet' = '1002',
                                  'hh_income' = '1009',
                                  'hh_scar' = '1001_i',
                                  'hh_shome' = '1001_j',
                                  'sat_dwe' = '145',
                                  'sat_task' = '402',
                                  'sat_part' = '407')

data_subset_long$educ <- ifelse(data_subset_long$educ %in% c(1501, 1502),
                                1508,
                                data_subset_long$educ)

data_subset_long$educ <- ifelse(data_subset_long$educ == 99,
                                NA,
                                data_subset_long$educ)

data_subset_long <-
    dplyr::mutate(data_subset_long,
                  dplyr::across(!dplyr::matches('^(inv|dsag|sad|age|n_rooms|sat|nkids)'),
                                as.factor))

levels(data_subset_long$sex) <- c('male',
                                  'female')

levels(data_subset_long$educ) <- c('isc_3a',
                                   'isc_3b',
                                   'isc_3c',
                                   'isc_5a-6',
                                   'isc_5d',
                                   'isc_0-2')

levels(data_subset_long$educ) <- levels(data_subset_long$educ)[c(6, 1, 2,
                                                                 3, 4, 5)]

levels(data_subset_long$parstat) <- c('co-resident',
                                      'non-resident',
                                      'no partner')

levels(data_subset_long$same_partner) <- c('same',
                                           'different')

data_subset_long <- dplyr::mutate(data_subset_long,
                                  dplyr::across(dplyr::matches('inv_'),
                                                .fns = ~ ifelse(.x >= 6,
                                                                NA,
                                                                .x)))

levels(data_subset_long$hh_income) <- c('less than 499 EUR',
                                        '500 to 999 EUR',
                                        '1000 to 1499 EUR',
                                        '1500 to 1999 EUR',
                                        '2000 to 2499 EUR',
                                        '2500 to 2999 EUR',
                                        '3000 to 4999 EUR',
                                        '5000 EUR or more')

levels(data_subset_long$hh_scar) <- c('yes',
                                      'no',
                                      'no')

levels(data_subset_long$hh_shome) <- c('yes',
                                       'no',
                                       'no')
