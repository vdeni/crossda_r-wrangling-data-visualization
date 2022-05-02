###################################################
##### Introduction - basics of the R language #####
###################################################

# This script covers the basics of the R programming language.
# We will go through the basics of the R syntax and its variable types. More
# "advanced" topics covering data wrangling will be presented in the second part.

##########################################
##### Basic mathematical operations #####
##########################################

# The syntax for most of the basic mathematical operations should be fairly
# intuitive, but we will cover it nevertheless.

# To add two numbers, we use the `+` binary operator. It's called a binary
# operator because it operates on two values. I'm mentioning this here because
# sometimes you'll see error messages talking about "binary" or "unary" operators,
# so this may give you a hint for what's gone wrong.

2 + 2

# Similarly, to subtract two numbers, we use the `-` operator.

5 - 1

# Division, multiplication and exponentiation may be familiar if you've had any
# experience with programming or similar. In any case, we use `/` for division,
# `*` for multiplication, and `^` for exponentiation. For example:

4 / 2

2 * 3

3^2

# Of course, as in mathematics, we have to be mindful of the way we group the
# terms in our mathematical expressions. For example, this expression

2 + 4 / 2

# reads as 2 + (4 / 2). If we'd like to add 2 and 4 first, we have to group
# the terms using parentheses:

(2 + 4) / 2

#####################
##### Functions #####
#####################

# The basic mathematical operations won't get us far by themselves. R also comes
# with *functions* - mappings that take a set of arguments, do something with
# them, and return a value.

# R's functions have the general form `function_name(argument1, argument2, ...)`

# The first function that we'll meet completes our set of basic mathematical
# operations - it is the `sqrt` function, which gives us the square root of a
# number supplied as its argument. For example:

sqrt(4)

# Another R function, which is very commonly used, is `c` (short for "combine").
# `c` takes N arguments and combines them into a *vector* (more on vectors
# later). For example, if we'd like to make a vector of numbers, we could do
# this:

c(1, 2, 3, 4, 5)

# We could also create a vector of text strings, like this:

c('duck', "cow", 'hedgehog', "bat")

# Pay attention to the quotation marks. We can use both single ('') or double
# ("") quotation marks to create text strings, but we *have to* use a matching
# pair for each string. For example, we cannot write "duck', because R will
# complain, but we can write either 'duck' or "duck".

#####################
##### Variables #####
#####################

# Up until now, we've executed mathematical operations or functions, applying
# them to specific, realized values (e.g. `2 + 2`). The results of these
# operations were displyed in the R console, but weren't stored anywhere for
# easy usage. To be able to do more with R, we'll need to use variables.
# Variables allow us to store the results of various operations or function calls,
# or to pass variable values to them (like `x + y`).

# In R, we assign values to variables by connecting a name with a value using
# the `<-` operator. For example, to store the value `2` in a variable named
# `a`, we'd execute:

a <- 2

# Now we can use the `a` variable in other operations. For example, we can
# write it in the console and hit `Enter`, which will make R display the value
# of the variable (i.e. `2`). We'd get the same result if we were to call the
# `print` function with `a` as an argument:

print(a)

# We are not entirely free to choose our variable names. Variable names can
# contain letters, numbers, dots (`.`), and underscors `_`. However, a variable
# name cannot start with a number, nor can they start with a dot followed by
# a number. R will complain in both cases.

# Furthermore, variable names cannot be strings reserved in the R language. For
# example, R uses the `for` statement for iterating over values (more about that
# later), so we cannot create a variable named `for`; e.g. `for <- 2` wouldn't
# work.

# A list of the reserved values can be found by typing `?Reserved` in the
# console. The `?` syntax allows us to search through R's documentation. We'll
# be seeing a lot of it during the course, so we won't dwell on it now.

# Now that we know how to store values in variables, we can assign the vectors
# that we've created earlier using `c`. Now, even though we *can* use a lot of
# things as variable names, we *should* try to make our variable names clear,
# meaningful and systematic. In the long term, this will save us a lot of mental
# effort, and will make manipulating variables in R easier. Also, note: try to
# avoid using non-ASCII letters in your variable names. For example, don't use
# characters such as ö, é or č.

# Now, assign our vector of animals to a variable called `animals`:

our.animals <- c('duck', 'cow', 'hedgehog', 'bat')

# We'll do the same for the numbers 1 through 5:

numbers_one_to_five <- c(1, 2, 3, 4, 5)

# Just as when we've assigned the value `2` to the variable `a`, now we also
# don't get any output from R. However, when we enter the variable name into
# R's console, we get the values stored in our vectors.

our.animals

numbers_one_to_five

# We don't have to print out the whole vector each time. Instead, we can use
# square brackets to get the element in the Nth position. For example, if we
# want to get the fourth value stored in the variable `our.animals`, we can
# do this:

our.animals[4]

# The number `4` in this case is an index. The indexing in R starts at 1, which
# means that the fist element of our vectors has an index equal to 1.
# For example, to get the first number from our vector of numbers 1 through 5,
# we would write:

numbers_one_to_five[1]

# We always know the index of the first element of a vector (1), but we may
# not always know the index of the last element of a vector. However, there's
# a nice trick we can use to grab the last element. R provides a `length`
# function. When supplied a vector, the function returns the number of elements
# in the vector. This also means that the number returned by the `length`
# function provides an index of the last element of a vector. For example:

length(our.animals)

# Now, keep in mind that the function `length` doesn't just write out the number
# of elements; it *returns* the number of elements. In other words, the number
# of elements in a vector is the output of the function. We could store that
# output into a variable.

animals_length <- length(our.animals)

animals_length

# The number of is a *valid index* (i.e. it is an integer less than or equal
# to the number of elements in the vector `our.animals`). Therefore, we can
# use the output of the `length` function as an index to grab the last element
# of our `animals_length` vector.

our.animals[animals_length]

# However, we could also to that *without storing* the length in a separate
# variable:

our.animals[length(our.animals)]

# Keep in mind: the square brackets work because we've stored a vector in the
# `our.animals` variable, and the square bracket notation allows us to access
# different elements of vectors. If `our.animals` were of another type (which
# we'll touch upon later in the course), the square bracket notation could be
# unavailable. In short: just keep in mind that a variable is a semi-arbitrary
# name that stores a *specific object*. Variables by themselves have no
# properties; the values they store do.

# Before moving on to the types of variables, we'll take a quick look at another
# frequently used binary operator - `==`. This duble-equal operator allows us
# to compare two values. If they're the same ("equal-equal") the operation returns
# `TRUE`, and if they're different, the operation returns `FALSE`.
# For example

4 == 2 + 2

3 == 2 + 2

# Note: as we will be seeing later, R also frequently uses the `=` symbol to
# assing values to parameters of functions. Keep this distinction in mind; `=` is
# very different from `==`!

#######################
##### Value types #####
#######################

# R has several basic types of values:
# - `character`: these are textual values, also known as "strings". For example,
# `duck` is of type character. We can check this using the `is.character` function.
# - `integer`: these are integers. For example, values `0`, `-1` or `5` are all
# integers, but `0.5` is not.
# - `double`: the set of real numbers. For example, `0.5` is `double`. However,
# R's default is to consider all numbers to be `double`, so even `1` would be
# `double`, unless R is told otherwise. To force R to consider `1` as an integer,
# we'd write `1L`. A bit more on that later.
# - `logical` : these are logical values; the two that you'll probably know from
# elsewhere are `TRUE` (which can be written as `T` for short), and `FALSE`
# (which can be written `F` for short). Keep in mind that the capitalization has
# to be respected! `false` or `False` have no meaning to R, unless we use it
# as a variable name (which you should *not* be doing).

# Not, let's take a look at some of these types, and see how we can check whether
# a certain value has a certain type.

# There's a wide set of `is.something(x)` functions that we can use to check
# whether object `x` is of type `something`. These functions return `TRUE` or
# `FALSE` depending on whether the object `x` is of type `something` or not.
# For example, to check whether a value is of type `character`,
# we'd use `is.character`:

is.character('duck')
is.character(our.animals[4])
is.character(1)

# Now to the numbers. To check whether a value is of type `integer`, we use
# `is.integer`. However, remember what we've said earlier: by default R considers
# all numbers to be of the type `double`

is.integer(1)
is.double(1)

# But if we append `L` to a number, R will treat it as an integer.

is.integer(1L)
is.double(1L)

# This is not something that's cruical to your usage of R. We're covering it for
# completeness, but also because you *may* find yourself in a situation where you
# really do need an integer. Now, you'll know how to do it.

# There's also the `is.numeric` function, which tells us whether a value is a
# number. It returns true both for `1.5` and for `1L` because they're both numeric
# values.

is.numeric(1.5)
is.numeric(1L)

# Keep in mind: we *must* use a decimal *point* when writing decimal numbers.
# R will complain if we try using a comma.

is.numeric(1,4141)

1,5151 + 1

# The last type we'll cover is `logical`:

TRUE == T
FALSE == F

is.logical(TRUE)

is.logical(F)

# Next, we take a look at some of the basic data structures in R.

###########################
##### Data structures #####
###########################

# Data structures are data organization, management and storage formats that allow
# us to efficiently acces and modify data. We've already met one data structure -
# the vector. R has several basic data structures. We'll cover those that appear
# most often.

# To start, let's remember the vector:

fruit <- c('apple', 'banana')

# We can check whether an object is a vector using `is.vector`:

is.vector(fruit)

# One important thing to note regarding vectors is that they can contain only
# values of a signle type. For example, we cannot have a vector where some values
# are characters and others are numbers. If we were to mix the types of values
# in a vector, R wouldn't neccessarily throw an error, but would first try to
# convert the elements to a common type. For example, let's try creating a
# vector of numbers; however, we'll put some of the numbers in quotation marks,
# thereby making them characters (even though they contain digits).

is.numeric('1')

numbers_vector <- c('1', 2, 3)

numbers_vector

# We can see that the last two elements have been converted to characters,
# so that all elements are of the same type. Which conversions happen, and when
# is too broad a topic for this course, but just be aware that they *do* happen.

######################
##### data.frame #####
######################

# `data.frame`s are one of the most important data structures in R (or at least
# one with which you'll be spending a lot of time). A `data.frame` roughly
# corresponds to what you've probably been used to seeing in SPSS's 'Data view'
# or MS Excel. A `data.frame` has rows which represent our units of analysis,
# and columns which represent variables. Each variable in a `data.frame` has to
# contain values of the same type, but different variables can hold different
# types of values. So, for example, we can have one column with `character` values
# and another one with `integer` values.

# We can create a `data.frame` by using the `data.frame` function. This is not
# something you'll usually do in your day-to-day work; you'll probably use some
# other function to read data from an external file (like an XLSX file), and
# that function will return a `data.frame`. We'll see this later on. However,
# there are times when you'd like to create a small `data.frame`, and this is
# how you could do it:

data.frame(numbers = c(1, 2, 3, 4, 5),
           'letters' = c('a', 'b', 'd', 'c', 'f'),
           'logicals' = c(F, F, T, T, F))

# Note that the column names can be written either with or without quotes. I
# prefer putting them in quotes, so that's what I'll be doing from now on.

# When we're creating a `data.frame` this way, all rows must have values in all
# columns, otherwise R will throw an error:

data.frame('numbers' = c(1, 2, 3, 4, 5),
           'letters' = c('a', 'b', 'd', 'c', 'f'),
           # we removed the las values from the 'logicals' column
           'logicals' = c(F, F, T, T))

# This is a good time to introduce you to R's `NA` value, which represents
# missing data. So now, instead of writing a `F` or `T` value in the 'logicals'
# column, we'll just put an `NA`.

data.frame('numbers' = c(1, 2, 3, 4, 5),
           'letters' = c('a', 'b', 'd', 'c', 'f'),
           # we've put an `NA` element as the last value
           'logicals' = c(F, F, T, T, NA))

# We'll store this `data.frame` to a variable named `nll` (numbers, letters,
# logicals). Note that this time around we've used the 1:5 notation to create
# the 'numbers' column. This notation can be used to create simple sequences of
# integers in R. For example:

1:5

nll <- data.frame('numbers' = 1:5,
                  'letters' = c('a', 'b', 'd', 'c', 'f'),
                  'logicals' = c(F, F, T, T, NA))

# We can access elements of a `data.frame` using square brackets, similarly as
# we do with vectors. However, we have to keep in mind that `data.frame`s are
# two-dimensional, which is why we have two indices - the first one indexing rows,
# and the second one - separated from the first one by a comma - indexing
# columns. For example, the code below returns the value found in the first row
# and second column ('letters'):

nll[1, 2]

# If we put just one index, R will return the corresponding column:

nll[2]

# We obtain the same result if we leave out one of the indices, but put a comma
# in the appropriate place.

# All columns of the first row:
nll[1, ]

# All rows of the second column
nll[, 2]

# We can also use the `n:m` syntax to get a range of values. For example, we
# can get the first three rows and all the columns:

nll[1:3, ]

# We can also get non-consecutive values by combining the indices using `c`:

# rows 1, 3, 4, 5 and columns 1 and 3
nll[c(1, 3:5), c(1, 3)]

# We can, actually, use anything that provides a valid index to get our rows
# or columns:

nll[2^2, ]

# Columns can also be accessed by using their names

nll[1:3, 'letters']
nll[1:3, c('letters', 'numbers')]

# Finally, there's two more ways in which we can access whole columns. We can
# put a variable name in square brackets, without any commas:

nll['letters']

# And we can use the dollar sign operator:

nll$letters

# Note a subtle difference between the two: `nll['letters']` returned a
# `data.frame`. We can check this using the `str` function, which shows us
# the structure of an object:

str(nll['letters'])

# On the other hand, `nll$letters` returned a vector of characters:

str(nll$letters)

# There is, however, yet another way of accessing `data.frame` columns, by
# using double square brackets:

nll[['letters']]

# This returns the vector of characters, just as `$` does. We can think of the
# `[]` notation as returning a more "high-level" type of object (in this case
# a `data.frame`), while `[[]]` returns a more "low-level" type of object (in
# this case a vector of characters).

# Next, we turn to lists.

##### list

# Lista je uređeni skup elemenata. Listu možemo definirati koristeći funkciju
# `list()`:

list('franz', 'liszt')

# Objekti u listi ne moraju biti istog tipa. Na primjer, možemo napraviti listu
# koja sadrži jedan `character`, jedan `integer` i jedan `numeric`.

spisak <- list('franz', 1L, 3.14)
spisak

# Brojevi u dvostrukim uglatim zagradama (`[[n]]`) daju nam do znanja da lista
# ima 3 elementa. To možemo potvrditi pozivom funkcije `str()` na `spisku`.

str(spisak)

# Ovdje vidimo i da `spisak` sadrži elemente različitih tipova. Liste možemo
# puniti raznolikim objektima, čak i drugim listama.

# pojedine elemente listi možemo i imenovati
raznoliki_objekti <- list('imena' = c('Ramiro', 'Zorro', 'Vladimir'),
                          'brojevi' = c(3.61, 4.15, 7.151, 20:25),
                          'inception' = list('glumci' = c('Leonardo di Caprio',
                                                          'ostali'),
                                             'broj_kamera' = 5))

str(raznoliki_objekti)

# Imenovanim elementima listi možemo pristupati isto kao i stupcima
# `data.framea`:

raznoliki_objekti$imena

raznoliki_objekti[2]

# Također, možemo dohvatiti više elemenata odjednom.

raznoliki_objekti[c('imena', 'brojevi')]

# Kad imamo ugniježđene (eng. *nested*) strukture, možemo ulančavati operatore
# za dohvaćanje kako bismo ušli dublje u strukture.

raznoliki_objekti$inception$glumci

# Posljednja struktura koju ćemo pogledati je matrica.

##### matrix

# Matrica je 2D objekt koji sadrži elemente istog tipa. Možemo je stvoriti
# koristeći funkciju `matrix()`.

postava <- matrix(c('Neo', 150, 'Morpheus', 165, 'Agent Smith', 140),
                  # broj stupaca matrice
                  ncol = 2,
                  # broj redova matrice
                  nrow = 3,
                  # trebaju li se podaci upisivati red po red ili stupac po
                  # stupac default je F
                  byrow = T)
postava

# U primjeru iznad koristili smo `=` kako bismo *imenovali* argumente funkcije.
# Ako pogledamo dokumentaciju za funkciju `matrix()`, vidjet ćemo da su oni,
# redom, `data`, `nrow`, `ncol`, `byrow`. Da smo funkciju iz prethodnog primjera
# pozvali bez imenovanja argumenata, R bi pretpostavio da se broj 2 odnosi na
# broj redova, a broj 3 na broj stupaca jer je broj redova (`nrow`) prije broja
# stupaca (`ncol`) u definiciji funkcije. Međutim, imenovanjem argumenata, mogli
# smo zamijeniti redoslijed

# Dimenzije matrice možemo dohvatiti funkcijom `dim()`, koja je primijenjiva i na
# `data.frame` (ali ne i na liste). Funkcija nam vraca dva broja; prvi je broj
# redova, a drugi je broj stupaca.

dim(postava)

# Redovima i stupcima matrica možemo dati imena, radi lakšeg orijentiranja:

dimnames(postava) <- list(# imena redova
    c('ozbiljni', 'pametni', 'zli'),
    # imena stupaca
    c('ime', 'visina'))

postava

# Imena redova možemo dohvatiti funkcijom `rownames()`, a imena stupaca
# funkcijom `colnames()`.

rownames(postava)

colnames(postava)

# Iste funkcije možemo koristiti i na `data.frameu`.

# Za dohvaćanje imena elemenata u listi možemo koristiti funkciju `names()`.

names(raznoliki_objekti)

# Elementima matrice možemo pristupati pomoću `[]` operatora, ali ne i pomoću
# `$` operatora. Također, pristupanje elementima pomoću indeksa nije isto kao
# kod `data.framea`.

# Naša matrica `postava` ima 3 reda i 2 stupca. Pogledajmo sljedeći primjer:

# matrica ima manje od 4 reda i manje od 4 stupca.
# ipak, ovo funkcionira
postava[1:4]

# ovo također funkcionira
postava[2:3, 1:2]

# i ovo
postava[2:3, 'ime']

# Ovdje se imena "redova" nalaze iznad svake vraćene vrijednosti (dakle, iznad
# `"Morpheus"` i `"Agent Smith"`)

# Ovime ćemo završiti uvod u R te se baciti na pripremu podataka za obradu.
