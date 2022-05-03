# Here, we'll see how to read data from an external file into R. As the data we're
# going to be working on is in SPSS's SAV format, we'll be using the {haven}
# R package to load it. We'll also give a quick tour of packages and
# functions available for some of the popular file formats; the general principle
# is the same for these files, but the functions are a bit different.

###########################
##### The R libraries #####
###########################

# To load an R package (or library; i.e. a collection of functions, variables or
# data that's not part of R itself), we use the `library` function, to which
# we provide a single argument - a package name. We'll be using several packages
# here and throughout the rest of the course. Below, we'll load each package,
# and give a short description of its contents.

# First, we'll load the {here} package. Here is a small and simple package that
# has only one function that's important to us - and that's `here`. When we
# provide a sequence of comma-separated strings to the function, it will return
# a file path that's adjusted depending on your operating system and the
# structure of your files and folders. A short demonstration will make things
# clearer. First, let's load the {here} package.

library(here)

# As we can see, after the package is loaded, R informs us that `here()` starts
# at a certain location on our computer. When {here} is loaded, it searches
# for the nearest .Rproj file, which we've created at he beginning of the course.
# If such a file is found, {here} sets the project's root folder to the one
# containing the .Rproj file. From now on, we can use the `here` function to
# construct paths relative to the project's root folder.

# For example, our data files are located in the `data/` subfolder within
# our project's root folder. Now, let's use the `here` function to construct
# the paths to each of the .SAV files containing the data. We'll need those paths
# later, when we'll read in the data.

path_wave_1 <- here::here('data',
                          'GGS_Wave1_France_V.4.4.sav')

path_wave_2 <- here::here('data',
                          'France_Wave2_V.1.3.sav')

path_wave_3 <- here::here('data',
                          'GGS_Wave3_France_V.1.0.sav')

# Note: the double-colon notation `::` allows us to specify from which package
# we are calling a certain function. This can be helpful if we have loaded two
# packages where each defines a function with a same name. For example, both
# package {A} and package {B} have a function named `myFunction`, that doesn't
# necessarily do the same thing. Then, if we'd like to call `myFunction` from
# {A}, we could write `A::myFunction()`.
# This notation also allows us to call a function from a package that is
# installed on our computer, but that wasn't loaded via `library`.
# However, neither of those cases applies here; there is no other function named
# `here`, and the {here} package was already loaded. However, I'll continue
# using the `::` notation so that you always know which library a function is
# coming from.

# Next, we will load the {haven} library that allows us to read in SPSS's SAV
# files.

library(haven)

# Note: Of course, SAV files are not the only file types you may want to read.
# Other popular file formats in the social sciences are CSV/TSV and XLS(X) files.
# Luckily, we can handle all of these in R. All the functions work in a similar
# fashion - they have practically identical arguments; some have more options,
# some less - you can check the documentation if unsure. We cover some of the
# functions below.
# TSV and CSV: R has built-in functions `read.csv` for reading in CSV files, and
# `read.delim` for reading in TSV files. Each function also has a second
# version - `read.csv2` and `read.delim2` - which has some different default
# arguments. For example, `read.csv2` uses semicolons (`;`) as field separators
# instead of commas (`,` which are the usual CSV suspects). There's also the
# more general `read.table` function, which uses any whitespace as a field
# separator.
# There's also the `readr` package, which provides similar functions, such as
# `read_csv` or `read_delim`. Some of those functions' defaults are different
# than the ones set by their base R counterparts. Also, they return `tibble`s
# instead of `data.frame`s.
# XLS(X): For reading XLS(X) files, you can use the {readxl} package, which
# provides the `read_excel`, `read_xls` and `read_xlsx` functions. Again, we
# won't go into the differences between those in here.

# We will be using the {dplyr} package for data wrangling.

library(dplyr)

################################
##### Reading in .SAV data #####
################################

# The {haven} package provides us with the `read_sav` function, which we can
# use to read data from SPSS's SAV files into R. We'll pass the paths we've
# defined earlier to the function in order to read in the data.

data_wave_1 <- haven::read_sav(path_wave_1)

data_wave_2 <- haven::read_sav(path_wave_2)

# here, we have to set the file encoding to 'latin1' because otherwise `read_sav`
# throws an error and the file doesn't get loaded
data_wave_3 <- haven::read_sav(path_wave_3,
                               encoding = 'latin1')

# A quick side note: you'll notice that `read_sav` didn't return a `data.frame`,
# but a `tibble`. `tibble`s are very similar to `data.frame`s; almost all of
# the functionality that we've covered earlier remains unchanged. probably the
# most noticeable difference is that it's printed differently than a `data.frame`.
# we won't dwell too much on the differences.

# These datasets are pretty big...
dim(data_wave_1)
dim(data_wave_2)
dim(data_wave_3)

# ... so it's a bit difficult to grasp them without referring to external
# documentation. Still, a function we could use is `glimpse` from the {dplyr}
# package. It returns similar information as `str` does, but may provide an
# overview that's a bit clearer.

glimpse(data_wave_1)

# Our first task is going to be to create one longitudinal dataset out of these
# three. To do that, we'll take only the participants who've provided answers
# in all three waves, and merge their responses into one dataset. First, we
# need a way to uniquely identify each participant. The participant codes
# are given in the variables `arid`, `brid` and `crid` for waves one through
# three, respectively.

str(data_wave_1$arid)
str(data_wave_2$brid)
str(data_wave_3$crid)

# To merge the datasets, we'll use the `inner_join` function from the {dplyr}
# package. This function takes two `tibble`s, and returns a new one. This new one
# contains the columns from both input datasets. Furthermore, it contains only
# the rows identified by a unique ID that appear in both input datasets.

data_merge_1_2 <- dplyr::inner_join(data_wave_1,
                                    data_wave_2,
                                    # since the ID variables are named differently
                                    # in our two datasets (`arid` vs `brid`),
                                    # we have to match them in the `by` argument.
                                    # we do that by creating a named vector
                                    by = c('arid' = 'brid'))

# We now have a single `tibble` that contains data of all the participants
# that participated in waves 1 and 2.
data_merge_1_2

# We'll repeat the inner join, combining the merged dataset for the first two
# waves with the dataset for the third wave.

data_waves_merged <- dplyr::inner_join(data_merge_1_2,
                                       data_wave_3,
                                       # the `brid` variable was removed from
                                       # the dataset at merge, so we're using
                                       # `arid` here
                                       by = c('arid' = 'crid'))

# Now, we have a single `tibble` containing the data of all the people who
# participated in all three data collection waves.
data_waves_merged

# We may want to save the merged dataset to a file. Luckily, {haven} offers the
# `write_sav` function, which allows us to write the `data_waves_merged` to a
# SAV file. Alternatively, we could write the data to some other format, like
# CSV.

# Here, we'll save the merged dataset to an external file.
haven::write_sav(data_waves_merged,
                 here::here('data',
                            'data_waves-merged.sav'))

# Also, we could simply `source` this script in another script. What this means
# is that we'd call the `source` function with just a single argument - the
# file path of this file - at the beginning of another R file. This would execute
# all the code written in the `source`d script (i.e. this script). Therefore,
# the merged dataset would become available in the new R session. An advantage
# of this approach over saving the data to an external file is that it makes it
# easier to consistently update the data if we make relevant changes to the
# `source`d script. Let's illustrate this with an example.

# Imagine that we've run all the code above, and that we now have a `tibble`
# that contains data from all three waves. We intend to use this merged dataset
# further down the line; for example, for visualization. However, at some point,
# we decide that we'd like to merge only the first and second wave.

# Scenario 1: We've previously saved the merged dataset to an external file.
# This means that we're reading in the data from an external file in the
# visualization script. Therefore, if we don't run this script again after
# modifying it (to remove the third wave data), the external file will still
# contain data from all three waves. Now, this is no big deal if we only want to
# exclude a wave of data (we can just filter it out in the visualization script).
# However, if we had made some changes to the data (e.g. recoded some variable),
# does changes wouldn't be reflected in the external file unless we ran this
# script again after modifying it.

# Scenario 2: We're `source`ing the merge script in the visualization script.
# Now, each time we source the merge script, the objects in the R session
# get updated. This means that, if we've made a change to the merge script at
# some later point in time, those changes would be reflected in the visualization
# script each time we `source` the merge script (which will usually happen at
# the beginning of a session).
