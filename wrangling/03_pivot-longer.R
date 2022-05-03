###########################
##### The R libraries #####
###########################

# Here, we'll use functions from the {tidyr} package to convert our data from
# a wide format to a long format.

library(tidyr)

library(here)
library(haven)
library(dplyr)

# As we didn't save our transformed data to an external file, we'll `source` the
# data cleaning script. This will execute all of the commands in the
# `02_clean-data.R` script, including the `library` calls. We've loaded the same
# libraries earlier, but this won't do any harm.
source(here::here('wrangling',
                  '02_clean-data.R'))

#############################################
##### Converting data from wide to long #####
#############################################

# At this point in time, we have a merged dataset, where each row holds the
# data for one participant, over three waves of data collection. However,
# for some analyses, we'd like to have the data in a long format, where
# each row contains the data for one point of measurement. Therefore, each
# participants data is cast over multiple rows of the `tibble`. We can do this
# with the `pivot_longer` command from the {tidyr} package. We can also go in the
# opposite direction - from long to wide - which we'll demonstrate later.

#############################################
##### Converting data from long to wide #####
#############################################
