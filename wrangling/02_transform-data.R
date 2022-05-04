# Here we'll get to know some of the functions available for subsetting rows or
# columns of our dataset, executing simple transformations, and getting useful
# summaries. We'll use our newly gained regex knowledge to execute some of these
# tasks.

###########################
##### The R libraries #####
###########################

# We'll be using some of the libraries we've met earlier. Mostly, we'll be
# relying on functions from the {dplyr} library.

library(here)
library(haven)
library(dplyr)

data_waves_merged <- haven::read_sav(here::here('data',
                                                'data_waves-merged.sav'))


#############################
##### Selecting columns #####
#############################

# When working with data, there are two simple operations that you're doing
# pretty frequently. The first one is selecting just a subset of the
# available columns of a dataset; the other is selecting a subset of rows. We'll
# first look at subsetting columns.

# We've already seen how we can use basic R syntax (`$` and `[]` or `[[]]`)
# to get subsets of columns of a `data.frame` or `tibble` (as they're really
# similar, I won't bother with writing this out fully every time; I'll
# just write 'data frame' or 'dataset' to refer to both). However, as our
# needs grow, it becomes more and more cumbersome to use the basic syntax. For
# example, we've mentioned elsewhere that we can distinguish the variables of
# our merged dataset that come from various data collection waves by the prefix
# in their name, which is 'a', 'b' or 'c' for waves 1-3. If we'd like to select
# only the columns from the first wave by using the basic syntax, we'd have to:
# (1) write out the column names individually,
# (2) select the columns by their indices (which can change due to various
# transformations), or
# (3) combine the square brackets with a call to a regex function to obtain the
# columns we want even if their indices change.
# The third point actually describes what we'll be doing. However, we won't do
# this by writing out a hard to read series of brackets and functions, but
# will use the nice and clear `select` function from the {dplyr} package.

# For example, I've already mentioned that maybe we'd like to extract all the
# variables that contain data from the first wave. Now, option (1) which was
# mentioned earlier is just too much work (we have more than a 1000 variables in
# wave 1); we won't even consider it.

# Option (2) requires us to know all the column indices. In that case, we could
# just write `data_waves_merged[m:n]` where `m` and `n` are the start and end
# indices, respectively. There's a bit (or could be a lot) more work if the
# indices aren't consecutive, so we'd have to do something like this
# `data_waves_merged[c(a:b, c:d, e:f, f:g, ...)]` where each pair of letters
# denotes a sequence of column indices. This is less cumbersome than writing
# out all the variable names, but we may end up with wrong columns if the column
# indices change (for example, because we've deleted one or more columns from the
# dataset).

# We'll show option (3), just so you have a reference point for the clarity of
# using `select`. We've mentioned earlier that we can subset rows and columns by
# anything that provides valid indices, including function output. Therefore,
# we can use the {stringr} functions we've learned earlier to select only
# the columns starting with 'a'. There are multiple ways to do this, but they're
# pretty similar. Here, we'll use `str_subset` to find the names of the columns
# starting with 'a' and supply this to the bracket notation to select the
# desired columns.

data_waves_merged[stringr::str_subset(colnames(data_waves_merged),
                                      '^a')]

# This gives us a subset of the columns of our dataset, where we've retained
# only those whose name starts with 'a'. Let's do it using `select`:

dplyr::select(data_waves_merged,
              dplyr::starts_with('a'))

# We get the exact same thing, but with less opaque notation. The `select`
# function takes a data frame as its first argument. Next, we supply the names
# of the columns we wish to select. Here, we've done that using a helper
# function named `starts_with` which selects all the columns whose names start
# with the string supplied to it; here, we're selecting all columns whose name
# starts with an 'a' (which is equivalent to the '^a' regex).
# There's a few other helper functions which we can use in `select`:
# - `ends_with`: equivalent to `starts_with` but looks at the end of the name
# - `contains`: selects variables whose names contain a string
# - `matches`: which allows us to use regular expression to select variables.

# All three functions are used the same way as `starts_with`. Since ends_with`
# and `contains` are pretty simple, we'll ignore them and jump to `matches`,
# which is far more powerful. So, let's try selecting only variables from the
# first and third wave, which contain data on the participants' age and sex.
# Those variables are 'aage', 'asex', 'cage' and 'csex'. Using `matches`, we do
# this:

dplyr::select(data_waves_merged,
              dplyr::matches('^(a|c)(sex|age)'))

# Here's some new regex notation, so let's explain it. The pipe (`|`) is an
# alternation operator. If we write 'a|c', we're saying that we're looking for
# either 'a' or 'c'. We could have done this using a custom character class
# '[ac]'. However, we can't use a character class for 'sex|age'.
# The parentheses define a *group* of characters or symbols. This is different
# from a class of characters. Basically, what we're saying in the whole regex
# above is look for strings such that:
# 1. they start with an 'a' OR a 'c'
# 2. which are followed by the characters 'sex' (in that exact order) OR 'age'
# (in that exact order).

# Now, this seems just a bit shorter than writing out the variable names. And it
# is. But it's more scalable - we can easily add more terms to the group on the
# right to quickly expand the columns we're getting. Speaking of writing out
# column names, `select` supports that too:

dplyr::select(data_waves_merged,
              ayear,
              aage)

# Notice that we didn't have to quote the variable names here. We won't go into
# the reasons why, just keep in mind that it's not mandatory, as it's the case
# with square bracket notation.

dplyr::select(data_waves_merged,
              ayear,
              aage,
              'bage')

# We can also supply column *name* ranges:

dplyr::select(data_waves_merged,
              acountry:aage)

# Or combine the various ways of subsetting:

dplyr::select(data_waves_merged,
              matches('year'),
              aage, bage)

# Keep in mind that `select` returns a `tibble`. So, we can save a subset of the
# columns to a new variable for further use.

#####################
##### Filtering #####
#####################

# Let's see how we can filter rows of data frames. Until now, we've covered
# the basic R way, using square brackets:

data_waves_merged[1:5, ]

# There's also two nifty functions we haven't mentioned earlier - `head` and
# `tail`. These show us the first or last N entries, respectively (the default is
# 6). This is as good a time as any:

head(data_waves_merged)
tail(data_waves_merged, n = 10)

# Now, subsetting rows by indices is fine. However, usually it's not what we want.
# Using bracket notation is cumbersome. For example, let's subset all people whose
# `asex` value is equal to 1:

data_waves_merged[data_waves_merged$asex == 1, ]

# Now imagine the horror of filtering by sex and age:

data_waves_merged[data_waves_merged$asex == 1 & data_waves_merged$aage == 67, ]

# Fortunately, there's an easier way. {dplyr} provides the `filter` function,
# which allows us to do the same thing in a more sensible manner:

dplyr::filter(data_waves_merged,
              asex == 1 & aage == 67)

# Note: there's also the base R function `subset` which behaves identically for
# our purposes. However, as we're covering {dplyr}, we're using `filter`.

# There's another binary operator that I'd like to introduce - `%in%`. We can use
# it to check whether elements of its left hand side are found in its right
# hand sound. Esoteric, so let's see:

c('a', 'b', 'c') %in% c('b', 'c', 'd', 'e')

# This also works with nubmers, so we could use it, for example, to subset
# only participants who were 45, 55 or 65 in the first data collection wave:

dplyr::filter(data_waves_merged,
              aage %in% c(45, 55, 65))

# We can also specify ranges of values:

dplyr::filter(data_waves_merged,
              aage >= 45 & aage <= 65)


# As such subsetting by range is quite common, there's a `between` helper
# function, which allows us to concisely specify an (inclusive) range:

dplyr::filter(data_waves_merged,
              dplyr::between(aage, 45, 65))
