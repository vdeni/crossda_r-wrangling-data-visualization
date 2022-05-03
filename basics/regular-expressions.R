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
# So, let's get started.

library(stringr)
