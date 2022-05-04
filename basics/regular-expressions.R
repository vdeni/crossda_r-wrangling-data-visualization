# Here, we'll take a detour from learning the basics of R, and look at something
# that goes beyond the language itself, but is pretty useful when working with
# any kind of text, be it variable values or variable names. We're going to talk
# about

###############################
##### Regular expressions #####
###############################

# Regular expressions, or *regex*, are a special kind of syntax used for
# describing patterns of text. There are several different flavors of regular
# expressions, with varying degrees of capabilities they provide. Here, we'll
# focus on the {stringr} package, which implements the ICU regex flavor, which
# is based on Perl's regex. This is not information you'll need to know by heart.
# Why it's important, though, is because it'll help you navigate this useful
# regex cheat sheet: https://remram44.github.io/regex-cheatsheet/regex.html.
# So, let's load the {stringr} package.

library(stringr)

# To get started, we'll need some strings. I've taken 10 column names from our
# dataset. They're written out as a vector of characters below, so you don't
# need to load any data file to go through this script.

column_names <- c("ayear", "aage", "asex", "ahhtype", "ahhsize", "aactstat",
                  "ahg3_15", "ahg3_16", "ahg3_17")

# Now, onto the regular expressions.

# Regular expressions are character strings that use special syntax and symbols
# to describe text patterns. Therefore, each regex we'll be using will be written
# as an R character value, surrounded by either single or double quotes. We'll
# supply those character strings as arguments to various {stringr} functions.
# Let's get started.

#######################
##### Quantifiers #####
#######################

# Note: I'll try using "symbol" for text that has special meaning in regular
# expression syntax, and "character" for ordinary text.

# The first thing we'll cover are various regex quantifiers. These are symbols
# or short expressions which specify how many times a certain character appears
# in our character string.

#####
# * #
#####

# The first quantifier we'll look at is `*` (asterisk), which denotes that the
# character preceding it appears 0 or more times. In other words, the character
# may either not appear at all, or appear an arbitrary number of times. It's
# important to note that this, and every other quantifier, applies only to the
# character preceding it (we'll extend this a bit later on).

# So, let's see this in action. We'll use the `str_detect` function from the
# {stringr} package, which takes a character or vector of characters and a
# regex pattern, and returns `TRUE` or `FALSE` depending on whether the regex
# pattern was found in the supplied character or vector of characters.

# Let's define this pattern:
pattern_h_asterisk <- 'h*'

# And call the `str_detect` function on our vector of column names:
stringr::str_detect(string = column_names,
                    pattern = pattern_h_asterisk)

# The function returns `TRUE` for all column names in the vector. Note that some
# of them contain only one 'h' character, some contain two, and others contain
# none. Remember, `*` looks for either *zero* ar an arbitrary number of
# characters. Therefore, `str_detect` returns `TRUE` for all entries, because
# all of them don't have any 'h' or have some number of 'h'.

# Note: we didn't have to write the 'h*' pattern to a variable before supplying
# it to `str_detect`; this would have also worked:
# `str_detect(column_names, 'h*')`
# Notice that we also did not have to write the argument names, as `string` is
# the first argument in `str_detect`, and `pattern` the second.

#####
# + #
#####

# Next, we look at the `+` quantifier, which is similar to `*`, but instead
# of looking for zero or more occurences, it looks for *one* or more. Let's
# again pass this to `str_detect`.

pattern_h_plus <- 'h+'

stringr::str_detect(column_names,
                    pattern_h_plus)

# We now see that `str_detect` returns `TRUE` only for those column names that
# have `h` *somewhere* in its name.

#####
# ? #
#####

# The next quantifier we'll look at is `?`, which checks whether a character
# appears zero times or once. Let's repeat:

pattern_h_question <- 'h?'

stringr::str_detect(column_names,
                    pattern_h_question)

#########
# {m,n} #
#########

# The last quantifier will look at now is the `{m,n}` quantifier. This is a
# modular quantifier, in a way, which allows us to check whether a character
# appears:
# 1. between `m` and `n` times (inclusive)
# 2. exactly `m` times
# 3. at least `m` times
# To see it in action, we'll create another vector of characters:

character_vector <- c('one a',
                      'three aaa',
                      'four aaaa',
                      'seven aaaaaaa')

# Now, let's look at all the three variants. First, we can check whether a
# character appears between `m` and `n` times. Let's try setting `m = 2`,
# `n = 4`:

# note that there are no spaces around the comma!
pattern_a_range <- 'a{2,4}'

stringr::str_detect(character_vector,
                    pattern_a_range)

# This returns `TRUE` for all strings but the first, even though 'seven aaaaaaa'
# contains seven 'a's. Strange. Let's take a look at another {stringr} function,
# `str_extract_all` which will return all detected substrings. There's also
# `str_extract`, which returns only the first detected substring.

stringr::str_extract_all(character_vector,
                         pattern_a_range)

# We get a list of the same length as the number of elements in
# `character_vector`, where each list element holds a vector of detected
# patterns. We see that `str_detect` detected two substrings in 'seven aaaaaaa':
# one which consists of four 'a's and one of three 'a's. Remember, we're looking
# for 2 to 4 occurences of 'a' anywhere in the string, so it doesn't matter that
# we have seven 'a's in a row when we're only looking for 2 to 4.

# Let's try looking for exactly four occurences of 'a':
pattern_a_4 <- 'a{4}'

stringr::str_detect(character_vector,
                    pattern_a_4)

# We again get `TRUE` for the last entry. However, let's look at the output of
# `str_extract_all`:
stringr::str_extract_all(character_vector,
                         pattern_a_4)

# Now, we have found only one pattern match in the last entry. Once we've found
# the first substring which consists of four 'a's, we have, in a way, "spent"
# those characters, so what remains are only three 'a's, which aren't matched
# by our rule.

# Finally, let's look at `m = 4` or more occurrences:

# note that we've put a comma, but left the space after it empty.
pattern_a_4_more <- 'a{4,}'

stringr::str_detect(character_vector,
                    pattern_a_4_more)

stringr::str_extract_all(character_vector,
                         pattern_a_4_more)

# That's it for the quantifiers. As we've seen earlier, we were looking for
# patterns anywhere in the string. However, we can narrow this search only to
# parts of the string.

##########################################
##### Anchors and expanding patterns #####
##########################################

# There's two special symbols that allow us to look for characters only on the
# start or the end of the string. These are sometimes called anchors.

#####
# ^ #
#####

# The caret (`^`) symbol is used to specify that we only want to search for
# patterns at the beginning of a string. Let's return to our column names; we'll
# try to look for two occurrences of 'a' at the beginning of a string. We
# write:
pattern_a_start <- '^a{2}'

stringr::str_detect(column_names,
                    pattern_a_start)

# Here, `str_detect` returns `TRUE` only for the column names which have two
# 'a's at the begining. We can use `str_subset` to extract only those strings
# where a match was found:
stringr::str_subset(column_names,
                    pattern_a_start)

#####
# $ #
#####

# Similarly, we can use the dollar sign `$` symbol to look only for patterns
# at the end of a string. For example, we could subset only those column names
# whose name ends with a single 'e':
pattern_e_end <- 'e$'

stringr::str_subset(column_names,
                    pattern_e_end)

# This is all fine and well, but I believe it doesn't look too useful yet. So
# let's expand our regular expression vocabulary with character classes.

#############################
##### Character classes #####
#############################

# Regular expression have special symbols or expression that define whole classes
# of characters. There's quite a few available clases, so we'll cover just two
# basic ones, and one nice extension.

#######
# \\d #
#######

# We use `\\d` to define a class of digits. For example, with the following
# pattern
pattern_digits <- '\\d'

# we could extract only the column names that contain digits anywhere in their
# name:

stringr::str_subset(column_names,
                    pattern_digits)

# Character classes can also be combined with anchors and quantifiers. We'll
# create a short, ad hoc vector of characters to demonstrate:
character_vector <- c('17_a',
                      'a_27',
                      'a_331',
                      'a_34',
                      'a_37',
                      'a_38')

# We can extract only those strings ending in two digits, where one digit is
# a '3', and the other can be anything else. Here, we'll make use of the fact
# that the digits at the ends of strings are preceded by an underscore. Using
# such information about the structure of strings can greatly simplify our
# regular expressions, and is another reason why it's good to have structured
# variable names (more on that later in the course).
pattern_digits_3 <- '_3\\d$'

stringr::str_subset(character_vector,
                    pattern_digits_3)

# We could also look for strings that contain exactly three digits:
pattern_digits_two <- '\\d{3}'

stringr::str_subset(character_vector,
                    pattern_digits_two)

#######
# \\w #
#######

# The second class we'll cover is the class of word characters - these are
# alphanumeric characters and the underscore. Again, we'll create a character
# vector for demonstration purposes:
character_vector <- c('name - named',
                      'lastName - unnamed',
                      'middle_name: none')

# Let's extract all substrings of an arbitrary (but positive) number of word
# characters appearing at the beginning of a string:
pattern_word_beginning <- '^\\w+'

stringr::str_extract_all(character_vector,
                         pattern_word_beginning)

############################
# Custom character classes #
############################

# We can also define custom character classes that we want to look for. Again,
# let's create a character vector:
character_vector <- c('203',
                      '11',
                      '12',
                      '13',
                      '14',
                      '23',
                      '24',
                      '25',
                      '27',
                      '52',
                      'v',
                      '+value',
                      '-value')

# Custom character classes are defined by surrounding a set of characters with
# square brackets. We'll create two patterns to demonstrate. The first one
# will look for the letter 'v' and the '+' character.
pattern_class_plusv <- '[+v]'

stringr::str_extract_all(character_vector,
                         pattern_class_plusv)

# Note that even though our character class admits two characters ('v' and '+'),
# it represents only *one space* in the string. So, when we've defined our
# previous pattern, we weren't saying that we're looking for '+' followed by 'v',
# for example. Instead, we said that we're looking for one occurrence (notice the
# absence of any quantifiers) of '+' or 'v', no matter the order.

# We can also use custom classes to define ranges of acceptable characters.
# We do to this by connecting the start and end points of the desired range
# with a hyphen (`-`). This makes sense for alphanumerics, of course. Let's
# try subsetting all strings from our character vector that have two digits,
# where the first one is between 1 and 2, and the second one is between 2 and 4:
pattern_class_range <- '[1-2][2-4]'

stringr::str_subset(character_vector,
                    pattern_class_range)

# I understand that regular expressions may seem a bit esoteric for now. So, why
# are we talking about them? One reason is that regular expressions pop up
# everywhere; for example, they're available in the Google Docs text search.
# Another reason is that a lot of work is done with text (this whole script
# is just text!), so learning regular expressions allows you to do a lot of things
# with a lot of things (eloquent, yes). Yet another reason is that having
# nicely (or less nicely) structured variable names in R's data structures
# allows us to use regular expressions to access variables in bulk. For example,
# in the data we're working with, all wave 1 variables start with an 'a', all
# wave 2 variables start with a 'b', and all wave 3 variables with a 'c'.
# Therefore, we could, for example, access all wave 1 variables with a simple
# regex pattern '^a'. Later, we'll see how, exactly.
