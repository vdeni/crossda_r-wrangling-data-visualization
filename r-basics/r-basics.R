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

##### Tipovi varijabli

# R razlikuje nekoliko osnovnih tipova podataka:
# - `character` : "stringovi", tj. tekstualni podaci. Npr. `'patka'`
# - `integer` : cijeli brojevi. Npr. `1`
# - `numeric` : realni brojevi. Npr. `1.161`
# - `logical` : logičke vrijednosti. Postoje ukupno dvije - `TRUE` (može se
# kratiti u `T`) i `FALSE` (može se kratiti u `F`)
# Pogledat ćemo nekoliko primjera ovih tipova, te vidjeti kako možemo provjeriti
# kojeg je neka varijabla ili vrijednost tipa.

# Da bismo provjerili je li neka vrijednost character, koristimo `is.character()`

is.character('susjed')
is.character(domace_zivotinje[4])
is.character(1)

# Kod `integer` i `numeric` tipova postoje neke specifičnosti.
# Da bismo provjerili je li neka vrijednost integer koristimo `is.integer()`:

is.integer(1)

# Pozivanje funkcije `is.integer()` s vrijednosti `1` vraća `FALSE`. To je zato
# jer R brojeve automatski sprema kao `numeric`.

# Kako bismo natjerali R da nam da `integer` vrijednost, možemo staviti `L` na
# kraj broja:

is.integer(1L)

# Ovo je zgodno znati jer se može dogoditi da funkcija traži `integer`, ali
# odbija prihvatiti (recimo) `5` kao odgovarajuću vrijednost.

# Kako bismo provjerili je li neka vrijednost `numeric` koristimo
# `is.numeric()`:

is.numeric(1.5115)

# Za pisanje decimalnih brojeva *moramo koristiti točku* jer se zarez koristi za
# odvajanje argumenata u funkcijama.

is.numeric(1,4141)

1,5151 + 1

# Posljednji tip je `logical`:

TRUE == T
FALSE == F

is.logical(TRUE)

is.logical(F)

# Nakon upoznavanja s osnovnim tipovima vrijednosti i varijabli, pogledat ćemo
# osnovne strukture podataka.

##### Strukture podataka

# Strukture podataka su formati organiziranja, upravljanja i spremanja podataka
# koji omogućuju efikasno pristupanje podacima i njihovo modificiranje
# Već smo se upoznali s jednim tipom strukture podataka u R-u, a to je vektor. R
# ima nekoliko osnovnih struktura podataka. Ovdje ćemo proći kroz one koje se
# najčešće javljaju.

# Za ponavljanje, stvorit ćemo novi vektor:

c('vektor', 'od', '4', 'elementa')

# Možemo provjeriti je li neki objekt vektor koristeći `is.vector()`:
is.vector(c('vektor', 'od', '4', 'elementa'))

##### data.frame

# `data.frame` je vjerojatno najvažnija osnovna struktura (ili barem ona s kojom
# ćete se najčešće družiti). On otprilike odgovara onom što možemo vidjeti u
# *Data viewu* SPSS-a - sastoji se od redova koji predstavljaju jedinice analize
# i stupaca koji predstavljaju varijable. Može sadržavati varijable koje su
# različitih tipova (za razliku od nekih drugih struktura, poput vektora, koje
# primaju samo jedan tip podataka).

# `data.frame` možemo stvoriti koristeći istoimenu funkciju:

data.frame(brojke = c(1, 2, 3, 4, 5),
           'slova' = c('a', 'b', 'd', 'c', 'f'),
           'logike'= c(F, F, T, T, F))

# Pri stvaranju novog `data.framea`, svi redovi moraju imati vrijednosti na svim
# stupcima jer će se R inače požaliti.

data.frame('brojke' = c(1, 2, 3, 4, 5),
           'slova' = c('a', 'b', 'd', 'c', 'f'),
           # maknuli smo zadnjji element (F) iz stupca 'logike'
           'logike'= c(F, F, T, T))

# Tome možemo doskočiti tako što ćemo eksplicitno neku vrijednost proglasiti
# nedostajućom, što činimo pomoću posebne vrijednosti `NA`:

data.frame('brojke' = c(1, 2, 3, 4, 5),
           'slova' = c('a', 'b', 'd', 'c', 'f'),
           # umjesto posljednjeg elementa u stupcu 'logike' stavili smo NA
           'logike'= c(F, F, T, T, NA))

# Spremit ćemo ovaj data.frame u varijablu brojke_i_slova. Primijetite da smo
# sad pri definiranju vrijednosti stupca `brojke` koristili sintaksu `n:m`. Ta
# sintaksa nam daje niz brojeva između n i m.

brojke_i_slova <- data.frame('brojke' = 1:5,
                             'slova' = c('a', 'b', 'd', 'c', 'f'),
                             'logike'= c(F, F, T, T, NA))

# Sad kad smo proširili `brojke_i_slova`, pogledat ćemo kako možemo pristupati
# vrijednostima u `data.frameu`.

# Elementima možemo pristupati korištenjem uglatih zagrada, kao i kod vektora.
# Pritom treba imati na umu da je `data.frame` *dvodimenzionalni objekt*, zbog
# čega traži *dva indeksa* odvojena zarezom - *prvi* se odnosi na
# *redove*, a *drugi* na *stupce*.

# Ako jedan od indeksa izostavimo, ali stavimo zarez, R će vratiti sve elemente
# na odgovarajućem mjestu, odnosno vratit će sve redove ako izostavimo prvi
# indeks i sve stupce ako izostavimo drugi indeks.

# svi stupci prvog  reda
brojke_i_slova[1, ]

# svi redovi prvog stupca
brojke_i_slova[, 1]

# Ovdje također možemo koristiti `n:m` sintaksu za dohvaćanje raspona
# vrijednosti. Na primjer, da bismo dohvatili prva tri reda i sve stupce
# `brojki_i_slova`, napravili bismo sljedeće:

# prva tri reda, svi stupci
brojke_i_slova[1:3, ]

# Za dohvaćanje vrijednosti koje nisu uzastopne, možemo koristiti funkciju `c()`,
# koju također možemo kombinirati s `n:m` sintaksom:

# prvi red i redove 3 do 5, te stupce 1 i 3
brojke_i_slova[c(1, 3:5), c(1, 3)]

# Stupcima možemo pristupati i pomoću njihovih imena:

brojke_i_slova[1:3, c('logike', 'brojke')]

# Naposljetku, *jednom* određenom stupcu možemo pristupiti koristeći `$`
# operator:

brojke_i_slova$logike

# Prije nego što prijeđemo na sljedeću strukturu podataka, upoznat ćemo se s
# funkcijom `str()` (structure). To je funkcija koja kao input prima neki objekt
# i vraća prikaz njegove strukture. Primjerice, možemo pogledati kakva je
# struktura našeg `data.framea` `brojke_i_slova`.

str(brojke_i_slova)

# R nas informira da je `brojke_i_slova` objekt tipa `data.frame` te da sadrži
# 5 redova (`5 obs.`) i 3 varijable. Uz svaku varijablu naveden je njen tip te
# je prikazano prvih nekoliko elemenata.

# Iduća struktura podataka koju ćemo pogledata je lista.

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
