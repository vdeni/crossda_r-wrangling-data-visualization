library(ggplot2)
library(here)
library(dplyr)

source(here::here('wrangling',
                  '04_prepare-visualization-data.R'))

dplyr::select(data_subset,
              'ankids')

red <- 'red'

ggplot2::ggplot(data_subset,
                aes(x = aage)) +
    ggplot2::geom_histogram(color = red,
                            size = 1,
                            linetype = 'dashed')

ggplot(data_subset,
       aes(x = aage,
           y = bage)) +
    ggplot2::geom_point(position = 'jitter')

ggplot(data_subset,
       aes(x = aage,
           y = bage)) +
    ggplot2::geom_point(position = position_jitter())

dplyr::mutate(data_subset,
              dplyr::across(aage,
                            sqrt)

dplyr::mutate(data_subset,
            dplyr::across(aage,
                          ~ sqrt(.x)))
