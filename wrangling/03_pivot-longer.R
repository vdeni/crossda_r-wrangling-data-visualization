###########################
##### The R libraries #####
###########################

# Here, we'll use functions from the {tidyr} package to convert our data from
# a wide format to a long format.

library(tidyr)

library(here)
library(haven)
library(dplyr)

data_waves_merged <- haven::read_sav(here::here('data',
                                                'data_waves-merged.sav'))

#############################################
##### Converting data from wide to long #####
#############################################

# We currently have a merged dataset, where each row holds the
# data for one participant, over three waves of data collection. However,
# for some analyses, we'd like to have the data in a long format, where
# each row contains the data for one point of measurement. Therefore, each
# participants data is cast over multiple rows of the `tibble`. We can do this
# with the `pivot_longer` command from the {tidyr} package. We can also go in
# the opposite direction - from long to wide - which we'll demonstrate later.

# Remember that variable names from each data collection wave are prefixed by a
# letter: 'a' for the first wave, 'b' for the second, and 'c' for the third. We
# can use this information to quickly pivot the dataset to the long format, so
# that each participant gets one row in the dataset for each wave of data
# collection.

# To make the transformations clearer, we'll select only three columns from each
# data collection wave (plus the ID column):

. <- dplyr::select(data_waves_merged,
                   arid, asex, ayear, amonth,
                   bsex, byear, bmonth,
                   csex, cyear, cmonth)

# We'll also re-add the participant IDs for waves 2 and 3 so as to have that
# identifier in each row

.$brid <- .$arid
.$crid <- .$arid

# Now, onto the `pivot_longer function`.
# First, we have to specify the dataset we're operating on. This will be the
# `.` `tibble` with a subset of columns from `data_waves_merged`.
# Next, under `cols` we specify the columns which we want to pivot. Here,
# we're setting this to `dplyr::everything`, which is a helper function that
# just returns all the columns in the dataset.
# Then, we specify `names_pattern`. This is a regular expression that tells
# `pivot_longer` how to parse the column names in search for relevant information.
# For us, the relevant information is stored in each variable name's prefix;
# this is why we've put 'a|b|c' at the beginning. We've put 'a|b|c' in a group
# using parentheses; this is required by `names_pattern` - this group will be
# used later to put the information about the data collection wave into a
# variable. The second group we've defined is '(.*)' which tells `pivot_longer`
# to accept anything. We'll see what's the point of that next.
# Finally, we supply a vector of two characters to `names_to`. The first
# element of the vector is 'wave', and this gives the column name where the
# values captured by the first group defined in `names_pattern` will be written.
# In our case, we'll get a column `wave` whose values will be 'a', 'b' or 'c'.
# The second element of the vector is '.value', which `pivot_longer` interprets
# in a special way. It tells `pivot_longer` that the second group (our '(.*)'
# match anything group) contains the column names to which the cell values
# will be written. In our case, '.value' actually stands for multiple column
# names. Let's see what we get when we run the

.long <- tidyr::pivot_longer(.,
                             cols = dplyr::everything(),
                             names_pattern = '(a|b|c)(.*)',
                             names_to = c('wave', '.value'))

# What we get is a data frame in the long format. But what we also get are a lot
# of warnings. They appear because the same variables have different labels in
# different data collection waves. For example:

# This function shows us the unique values present in a variable. In this case,
# it also shows us the available value labels.
unique(.$asex)
unique(.$bsex)
unique(.$csex)

# `pivot_longer` is also informing us that it'll use the variable labels from
# the first variable it encounters; in this case, that's `asex`. Of course,
# this may not be what we want. So, we'll use some of our newly acquired data
# transformation knowledge to try to hack a solution. Our dataset has both
# unlabeled and labeled variables; the labeled ones are those that contain
# value labels, as given in SPSS. We'll try selecting all labeled variables,
# and transforming them into `factor`s using {haven}s `as_factor` function.
# We'll use this function because the base R `as.factor` would return numeric
# factors (e.g. '1' and '2' instead of 'male' and 'female', which would make
# it difficult to navigate through the dataset).

# We can do this using `mutate` and `across`. Instead of providing a function
# such as `matches` to `across`, we'll provide the function `is.labelled` from
# the {haven} package; this function returns `TRUE` if a column is labeled,
# allowing us to apply our transformation only on those columns. The
# transformation we'll apply is `as_factor` from the {haven} package, as
# mentioned earlier.

data_waves_merged <- dplyr::mutate(data_waves_merged,
                                   across(dplyr::everything(),
                                          haven::as_factor))

# Trying to use pivot_longer to recast the data as long would face us with an
# error whose origin, honestly, I haven't been able to pinpoint quickly. If
# you're interested in tackling it, uncomment the code below and run it:

# tidyr::pivot_longer(data_waves_merged,
#                     cols = dplyr::everything(),
#                     names_pattern = '(a|b|c)(.*)',
#                     names_to = c('wave',
#                                  '.value'))

# However, there is a simple workaround - we can split the dataset into three
# separate data frames (one for each data collection wave), harmonize the
# variable names, and then stack them on top of each other using `bind_rows`.
# This sounds more complicated than it is. Let's see how this could be done.
# First, we'll just subset the columns of each wave and store them to their own
# data frames. But before doing that, we'll also re-add the participant ID
# columns, just to ease our orientation in the dataset.

data_waves_merged$brid <- data_waves_merged$arid
data_waves_merged$crid <- data_waves_merged$arid


data_wave_1 <- dplyr::select(data_waves_merged,
                             dplyr::matches('^a'))

data_wave_2 <- dplyr::select(data_waves_merged,
                             dplyr::matches('^b'))

data_wave_3 <- dplyr::select(data_waves_merged,
                             dplyr::matches('^c'))

# Next, let's remove the prefixes from the column names. We've seen how to do
# that in bulk by using `rename_with`.

data_wave_1 <- dplyr::rename_with(data_wave_1,
                                  .cols = dplyr::everything(),
                                  .fn = stringr::str_replace,
                                  pattern = '^a',
                                  replacement = '')

data_wave_2 <- dplyr::rename_with(data_wave_2,
                                  .cols = dplyr::everything(),
                                  .fn = stringr::str_replace,
                                  pattern = '^b',
                                  replacement = '')

data_wave_3 <- dplyr::rename_with(data_wave_3,
                                  .cols = dplyr::everything(),
                                  .fn = stringr::str_replace,
                                  pattern = '^c',
                                  replacement = '')

# Finally, we'll use `bind_rows` from the {dplyr} package to stack these one
# on top of each other. `bind_rows` will take care of matching the variable
# names.

data_merged_long <- dplyr::bind_rows(data_wave_1,
                                     data_wave_2,
                                     data_wave_3)

# If we now check the `sex` variable, for example, we'll notice that it contains
# all the labels from all data collection waves:

levels(data_merged_long$sex)

# Even though this workaround seems to work fine, it's pretty hackish. If we
# wanted to conduct a real analysis outside a workshop context, we'd approach
# this merge far more carefully. For example, we could subset only the columns
# we're interested in for a given analysis or visualization, and carefully
# cast it to a long format, checking all value labels in the process, making
# sure that no data is lost or corrupted during the transformation.

#############################################
##### Converting data from long to wide #####
#############################################

# Let's take the long data frame that we've stored in the `.` variable, and
# convert it back to a long format. To do that, we'll us the `pivot_wider`
# function.

# We can use `id_cols` to name one or more columns that make it possible to
# uniquely identify each observation. In our case, this is the `rid` variable.
# `names_from` is a set of columns in the long data frame that hold the names
# that should be given to the columns of the wide data frame. Here, we're
# setting this to the `wave` variable of the long data frame, whose values
# are 'a', 'b' and 'c', which denote the data collection wave.
# Next, in the `values_from` column, we specify the set of columns in the long
# data frame which hold the cell values of the columns that will be created in
# the wide dataset. Here, we're using `matches` and the negation operator `!`
# to select all columns of the `long` data frame except `rid` (which is used
# as an ID column) and `wave` (which we're using to get the column names).
# Finally, we use `names_glue` to specify a pattern for the column names of the
# `wide` dataset. Here, the elements within curly braces get evaluated to the
# listed variables' values. So, the pattern we've constructed says that each
# variable name should start with the appropriate value of the `wave` variable,
# which is 'a', 'b' or 'c'. '.value' is, again, a special name which refers to
# the column names of the variables from which the cell values are being taken.

tidyr::pivot_wider(.long,
                   id_cols = 'rid',
                   names_from = 'wave',
                   values_from = !matches('rid|wave'),
                   names_glue = '{wave}{.value}')
