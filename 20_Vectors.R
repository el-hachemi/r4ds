#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#
#                              20. VECTORS                                  #
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===# 

# 1. Introduction
#~~~~~~~~~~~~~~~~~~

  # It is possible to write functions that work with tibbles (like ggplot2, dplyr, and tidyr), but the tools you need to write such functions are currently idiosyncratic and immature. I am working on a better approach, https://github.com/hadley/lazyeval, but it will not be ready in time for the publication of the book. Even when complete, you’ll still need to understand vectors, it’ll just make it easier to write a user-friendly layer on top.

  # 1.1 Prerequisities
  #~~~~~~~~~~~~~~~~~~~~~
  
    # The focus of this chapter is on base R data structures, so it isn’t essential to load any packages. We will, however, use a handful of functions from the purrr package to avoid some inconsistencies in base R.

library(tidyverse)

# 2. Vector basics
#~~~~~~~~~~~~~~~~~~~

  # There are two types of vectors:

    # 1. Atomic vectors, of which there are six types: logical, integer, double, character, complex, and raw. Integer and double vectors are collectively known as numeric vectors

    # 2. Lists, which are sometimes called recursive vectors because lists can contain other lists

  # The chief difference between atomic vectors and lists is that atomic vectors are **homogeneous**, while lists can be **heterogeneous**. There’s one other related object: NULL. NULL is often used to represent the absence of a vector (as opposed to NA which is used to represent the absence of a value in a vector). NULL typically behaves like a vector of length 0.

  # Every vector has two key properties:

    # 1. Its **type**, which you can determine with typeof().
typeof(letters)
typeof(1:10)

    # 2. Its **length**, which you can determine with length().
x <- list("a", "b", 1:10)
length(x)

  # Vectors can also contain arbitrary additional metadata in the form of attributes. These attributes are used to create **augmented vectors** which build on additional behaviour. There are three important types of augmented vector:

    # * Factors are built on top of integer vectors.
    # * Dates and date-times are built on top of numeric vectors.
    # * Data frames and tibbles are built on top of lists.

# 3. Important types of atomic vector
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # The four most important types of atomic vector are logical, integer, double, and character. Raw and complex are rarely used during a data analysis, so I won’t discuss them here.

  # 3.1 Logical
  #~~~~~~~~~~~~~

    # Logical vectors are the simplest type of atomic vector because they can take only three possible values: FALSE, TRUE, and NA.

1:10 %% 3 == 0
c(TRUE, TRUE, FALSE, NA)

  # 3.2 Numeric
  #~~~~~~~~~~~~~~

    # Integer and double vectors are known collectively as numeric vectors. In R, numbers are doubles by default. To make an integer, place an L after the number:

typeof(1)
typeof(1L)
1.5L

    # The distinction between integers and doubles is not usually important, but there are two important differences that you should be aware of:

      # 1. Doubles are approximations. Doubles represent floating point numbers that can not always be precisely represented with a fixed amount of memory. This means that you should consider all doubles to be approximations.

x <- sqrt(2) ^ 2
x # [1] 2
x - 2 # [1] 4.440892e-16

        # This behaviour is common when working with floating point numbers: most calculations include some approximation error. Instead of comparing floating point numbers using ==, you should use dplyr::near() which allows for some numerical tolerance.

      # 2. Integers have one special value: NA, while doubles have four: NA, NaN, Inf and -Inf. All three special values NaN, Inf and -Inf can arise during division:

c(-1, 0, 1) / 0 # [1] -Inf  NaN  Inf

        # Avoid using == to check for these other special values. Instead use the helper functions is.finite(), is.infinite(), and is.nan():
_____________________________________________________
              |   0   |   Inf   |   NA    |   NaN   |
_____________________________________________________
is.finite()   |   x   
is.infinite() |            x
is.na()       |                     x           x
is.nan()      |                                 x
_____________________________________________________

  # 3.3 Character
  #~~~~~~~~~~~~~~~~

    # Character vectors are the most complex type of atomic vector, because each element of a character vector is a string, and a string can contain an arbitrary amount of data.

    # One important feature of the underlying string implementation: R uses a global string pool. This means that each unique string is only stored in memory once, and every use of the string points to that representation. This reduces the amount of memory needed by duplicated strings. You can see this behaviour in practice with pryr::object_size():

x <- "This is a reasonably long string."
pryr::object_size(x) # 152 B

y <- rep(x, 1000)
pryr::object_size(y) # 8.14 kB

    # y doesn’t take up 1,000x as much memory as x, because each element of y is just a pointer to that same string. A pointer is 8 bytes, so 1000 pointers to a 136 B string is 8 * 1000 + 136 = 8.13 kB.

  # 3.4 Missing values
  #~~~~~~~~~~~~~~~~~~~~

    # Note that each type of atomic vector has its own missing value:
NA            # logical
NA_integer_   # integer
NA_real_      # double
NA_character_ # character

    # Normally you don’t need to know about these different types because you can always use NA and it will be converted to the correct type using the implicit coercion rules described next. However, there are some functions that are strict about their inputs, so it’s useful to have this knowledge sitting in your back pocket so you can be specific when needed.

  # 3.5 Exercises
  #~~~~~~~~~~~~~~~~

    # > 1. Describe the difference between is.finite(x) and !is.infinite(x).

x <- c(0, NA, NaN, Inf, -Inf)
is.finite(x) # [1]  TRUE FALSE FALSE FALSE FALSE
!is.infinite(x) # [1]  TRUE  TRUE  TRUE FALSE FALSE

      # The is.finite() function considers non-missing numeric values to be finite, and missing (NA), not a number (NaN), and positive (Inf) and negative infinity (-Inf) to not be finite. The is.infinite() behaves slightly differently. It considers Inf and -Inf to be infinite, and everything else, including non-missing numbers, NA, and NaN to not be infinite. 

    # > 2. Read the source code for dplyr::near() (Hint: to see the source code, drop the ()). How does it work?

dplyr::near
# function (x, y, tol = .Machine$double.eps^0.5) 
# {
#   abs(x - y) < tol
# }
# <bytecode: 0x000002c238b8d7d0>
# <environment: namespace:dplyr>

      # Instead of checking for exact equality, it checks that two numbers are within a certain tolerance, tol. By default the tolerance is set to the square root of .Machine$double.eps, which is the smallest floating point number that the computer can represent.

    # > 3. A logical vector can take 3 possible values. How many possible values can an integer vector take? How many possible values can a double take? Use google to do some research.

      # For integers vectors, R uses a 32-bit representation. This means that it can represent up to 2^32 (4.294.967.296) different values with integers. One of these values is set aside for NA_integer_. From the help for integer: 
      # > Note that current implementations of R use 32-bit integers for integer vectors, so the range of representable integers is restricted to about +/-2*10^9: doubles can hold much larger integers exactly.
      
      # The range of integers values that R can represent in an integer vector is +/- (2^31-1).

.Machine$integer.max # [1] 2147483647

      # The maximum integer is 2^31-1 rather than 2^32 because 1 bit is used to represent the sign (+,-) (My words are 2^32 is the total number the half is positif and the other is negatif so 2^32/2 = 2147483648 = 2^31) and one value used to represent NA_integer_.

      # If you try to represent an integer greater than that value, R will return NA values.

.Machine$integer.max + 1L # [1] NA
# Warning message:
# In .Machine$integer.max + 1L :
#   NA produit par débordement d'entier par le haut

      # However, you can represent that value (exactly) with a numeric vector at the cost of about two times the memory.

as.numeric(.Machine$integer.max) + 1 # [1] 2147483648

      # The same is true for the negative of the integer max.

-.Machine$integer.max - 1L # [1] NA
# Warning message:
# In -.Machine$integer.max - 1L :
#   NA produit par débordement d'entier par le haut

      # For double vectors, R uses a 64-bit representation. This means that they can hold up to 2^64 values exactly. However, some of those values are allocated to special values such as -Inf, Inf, NA_real_, and NaN. From the help for double:
      # > All R platforms are required to work with values conforming to the IEC 60559 (also known as IEEE 754) standard. This basically works with a precision of 53 bits, and represents to that precision a range of absolute values from about 2e-308 to 2e+308. It also has special values NaN (many of them), plus and minus infinity and plus and minus zero (although R acts as if these are the same). There are also denormal(ized) (or subnormal) numbers with absolute values above or below the range given above but represented to less precision.

      # The details of floating point representation and arithmetic are complicated, beyond the scope of this question, and better discussed in the references provided below. The double can represent numbers in the range of about +/- 2*10^308, which is provided in:
.Machine$double.xmax # [1] 1.797693e+308

      # Many other details for the implementation of the double vectors are given in the .Machine variable (and its documentation). These include the base (radix) of doubles,
.Machine$double.base # [1] 2

      # the number of bits used for the significand (mantissa),
.Machine$double.digits # [1] 53

      # the number of bits used in the exponent,
.Machine$double.exponent # [1] 11

      # and the smallest positive and negative numbers not equal to zero,
.Machine$double.eps # [1] 2.220446e-16
.Machine$double.neg.eps # [1] 1.110223e-16

      # There's 11 reference in the page https://jrnold.github.io/r4ds-exercise-solutions/vectors.html to more lectures.


    # > 4. Brainstorm at least four functions that allow you to convert a double to an integer. How do they differ? Be precise.

      # The difference between to convert a double to an integer differ in how they deal with the fractional part of the double. There are are a variety of rules that could be used to do this.

        # * Round down, towards −∞ . This is also called taking the floor of a number. This is the method the floor() function uses.
        # * Round up, towards +∞ .  This is also called taking the ceiling. This is the method the ceiling() function uses.
        # * Round towards zero. This is the method that the trunc() and as.integer() functions use.
        # * Round away from zero.
        # * Round to the nearest integer. There several different methods for handling ties, which are numbers with a fractional part of 0.5.

          # * Round half down, towards −∞.
          # * Round half up, towards +∞ .
          # * Round half towards zero
          # * Round half away from zero
          # * Round half towards the even integer. This is the method that the round() function uses.
          # Round half towards the odd integer.

function(x, method) {
  if (method == "round down") {
    floor(x)
  } else if (method == "round up") {
    ceiling(x)
  } else if (method == "round towards zero") {
    trunc(x)
  } else if (method == "round away from zero") {
    sign(x) * ceiling(abs(x))
  } else if (method == "nearest, round half up") {
    floor(x + 0.5)
  } else if (method == "nearest, round half down") {
    ceiling(x - 0.5)
  } else if (method == "nearest, round half towards zero") {
    sign(x) * ceiling(abs(x) - 0.5)
  } else if (method == "nearest, round half away from zero") {
    sign(x) * floor(abs(x) + 0.5)
  } else if (method == "nearest, round half to even") {
    round(x, digits = 0)
  } else if (method == "nearest, round half to odd") {
    case_when(
      # smaller integer is odd - round half down
      floor(x) %% 2 ~ ceiling(x - 0.5),
      # otherwise, round half up
      TRUE ~ floor(x + 0.5)
    )
  } else if (method == "nearest, round half randomly") {
    round_half_up <- sample(c(TRUE, FALSE), length(x), replace = TRUE)
    y <- x
    y[round_half_up] <- ceiling(x[round_half_up] - 0.5)
    y[!round_half_up] <- floor(x[!round_half_up] + 0.5)
    y
  }
}

tibble::tibble(
  x = c(
    1.8, 1.5, 1.2, 0.8, 0.5, 0.2,
    -0.2, -0.5, -0.8, -1.2, -1.5, -1.8
  ),
  `Round down` = floor(x),
  `Round up` = ceiling(x),
  `Round towards zero` = trunc(x),
  `Nearest, round half to even` = round(x)
)

      # See the Wikipedia articles, [Rounding](https://en.wikipedia.org/wiki/Rounding) and [IEEE floating point](https://en.wikipedia.org/wiki/IEEE_floating_point) for more discussion of these rounding rules.

      # Rounding rules can have real world impacts. One notable example was that in 1983, the Vancouver stock exchange adjusted its index from 524.811 to 1098.892 to correct for accumulated error due to rounding to three decimal points (see [Vancouver Stock Exchange](https://en.wikipedia.org/wiki/Vancouver_Stock_Exchange)). This [site](https://web.ma.utexas.edu/users/arbogast/misc/disasters.html) lists several more examples of the dangers of rounding rules.
 

    # > 5. What functions from the readr package allow you to turn a string into logical, integer, and double vector?

      # The function parse_logical() parses logical values, which can appear as variations of TRUE/FALSE or 1/0.
readr::parse_logical(c("TRUE", "FALSE", "1", "0", "true", "t", "NA"))
# [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE    NA

      # The function parse_integer() parses integer values.
readr::parse_integer(c("1235", "0134", "NA"))
# [1] 1235  134   NA

      # However, if there are any non-numeric characters in the string, including currency symbols, commas, and decimals, parse_integer() will raise an error.
readr::parse_integer(c("1000", "$1,000", "10.00"))
# Warning: 2 parsing failures.
# row col               expected actual
#   2  -- an integer             $1,000
#   3  -- no trailing characters .00  
# [1] 1000   NA   NA
# attr(,"problems")
# # A tibble: 2 x 4
#     row   col expected               actual
#   <int> <int> <chr>                  <chr> 
# 1     2    NA an integer             $1,000
# 2     3    NA no trailing characters .00   

      # The function parse_number() parses integer values.
readr::parse_number(c("1.0", "3.5", "$1,000.00", "NA"))

      # Unlike parse_integer(), the function parse_number() is very forgiving about the format of the numbers. It ignores all non-numeric characters, as with "$1,000.00" in the example. This allows it to easily parse numeric fields that include currency symbols and comma separators in number strings without any intervention by the user.


# 4. Using atomic vectors
#~~~~~~~~~~~~~~~~~~~~~~~~~

  # 4.1 Coercion
  #~~~~~~~~~~~~~~

    # There are two ways to convert, or coerce, one type of vector to another:

      # 1. Explicit coercion happens when you call a function like as.logical(), as.integer(), as.double(), or as.character(). Whenever you find yourself using explicit coercion, you should always check whether you can make the fix upstream, so that the vector never had the wrong type in the first place. For example, you may need to tweak your readr col_types specification.

      # 2. Implicit coercion happens when you use a vector in a specific context that expects a certain type of vector. For example, when you use a logical vector with a numeric summary function, or when you use a double vector where an integer vector is expected.
  
    # Because explicit coercion is used relatively rarely, and is largely easy to understand, I’ll focus on implicit coercion here.

    # using a logical vector in a numeric context. In this case TRUE is converted to 1 and FALSE converted to 0. That means the sum of a logical vector is the number of trues, and the mean of a logical vector is the proportion of trues:

x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)  # how many are greater than 10?
mean(y) # what proportion are greater than 10?

    # You may see some code (typically older) that relies on implicit coercion in the opposite direction, from integer to logical:

if (length(x)) {
  # do something
}

    # In this case, 0 is converted to FALSE and everything else is converted to TRUE. I think this makes it harder to understand your code, and I don’t recommend it. Instead be explicit: length(x) > 0.

    # It’s also important to understand what happens when you try and create a vector containing multiple types with c(): the most complex type always wins.

typeof(c(TRUE, 1L)) # [1] "integer"
typeof(c(1L, 1.5)) # [1] "double"
typeof(c(1.5, "a")) # [1] "character"

  # 4.2 Test functions
  #~~~~~~~~~~~~~~~~~~~~

    # One option is to use typeof(). Another is to use a test function which returns a TRUE or FALSE. Base R provides many functions like is.vector() and is.atomic(), but they often return surprising results. Instead, it’s safer to use the is_* functions provided by purrr, which are summarised in the table below.
---------------------------------------------------------------------
#                        lgl     int     dbl     chr     list
---------------------------------------------------------------------
# is_logical()	         x
# is_integer()	                 x
# is_double()                             x
# is_numeric()	                 x        x
# is_character()	                                x
# is_atomic()	           x       x        x       x
# is_list()	                                              x
# is_vector()	           x       x        x       x       x  
----------------------------------------------------------------------
  
    # Each predicate also comes with a “scalar” version, like is_scalar_atomic(), which checks that the length is 1. This is useful, for example, if you want to check that an argument to your function is a single logical value.
  
  # 4.3 Scalars and recycling rules
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    #  R will also implicitly coerce the length of vectors. This is called vector recycling, because the shorter vector is repeated, or recycled, to the same length as the longer vector.
  
    # This is generally most useful when you are mixing vectors and “scalars”. I put scalars in quotes because R doesn’t actually have scalars: instead, a single number is a vector of length 1. Because there are no scalars, most built-in functions are vectorised, meaning that they will operate on a vector of numbers. That’s why, for example, this code works:
  
sample(10) + 100 # [1] 108 107 106 110 104 101 103 105 109 102
runif(10) > 0.5 # [1] FALSE FALSE FALSE TRUE TRUE TRUE FALSE FALSE TRUE TRUE

    # In R, basic mathematical operations work with vectors. That means that you should never need to perform explicit iteration when performing simple mathematical computations.

    # It’s intuitive what should happen if you add two vectors of the same length, or a vector and a “scalar”, but what happens if you add two vectors of different lengths?

1:10 + 1:2 #  [1]  2  4  4  6  6  8  8 10 10 12

    # Here, R will expand the shortest vector to the same length as the longest, so called recycling. This is silent except when the length of the longer is not an integer multiple of the length of the shorter:

1:10 + 1:3 #  [1]  2  4  6  5  7  9  8 10 12 11
# Warning message:
# In 1:10 + 1:3 :
#   la taille d'un objet plus long n'est pas multiple de la taille d'un objet plus court

    # While vector recycling can be used to create very succinct, clever code, it can also silently conceal problems. For this reason, the vectorised functions in tidyverse will throw errors when you recycle anything other than a scalar. If you do want to recycle, you’ll need to do it yourself with rep():

tibble::tibble(x = 1:4, y = 1:2)
# Erreur : Tibble columns must have compatible sizes.
# * Size 4: Existing data.
# * Size 2: Column `y`.
# i Only values of size one are recycled.
# Run `rlang::last_error()` to see where the error occurred.

tibble::tibble(x = 1:4, y = rep(1:2, 2))
# A tibble: 4 x 2
#      x     y
#   <int> <int>
# 1     1     1
# 2     2     2
# 3     3     1
# 4     4     2

tibble::tibble(x = 1:4, y = rep(1:2, each = 2))
# A tibble: 4 x 2
#      x     y
#   <int> <int>
# 1     1     1
# 2     2     1
# 3     3     2
# 4     4     2

  # 4.4 Naming vectors
  #~~~~~~~~~~~~~~~~~~~~

    # All types of vectors can be named. You can name them during creation with c():
c(x = 1, y = 2, z = 4)
# x y z 
# 1 2 4

    # Or after the fact with purrr::set_names():
purrr::set_names(1:3, c("a", "b", "c"))
# a b c 
# 1 2 3 

    #Named vectors are most useful for subsetting, described next.

  # 4.5 Subsetting
  #~~~~~~~~~~~~~~~~

    # So far we’ve used dplyr::filter() to filter the rows in a tibble. filter() only works with tibble, so we’ll need new tool for vectors: [. [ is the subsetting function, and is called like x[a]. There are four types of things that you can subset a vector with:

      # 1. A numeric vector containing only integers. The integers must either be all positive, all negative, or zero.

# Subsetting with positive integers keeps the elements at those positions:
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)] # [1] "three" "two"   "five" 

# By repeating a position, you can actually make a longer output than input:
x[c(1, 1, 5, 5, 5, 2)] # [1] "one"  "one"  "five" "five" "five" "two" 

# Negative values drop the elements at the specified positions:
x[c(-1, -3, -5)] # [1] "two"  "four"

# It’s an error to mix positive and negative values:
x[c(1, -1)] # Error in x[c(1, -1)]:les indices négatifs ne peuvent être mélangés qu'à des 0

# The error message mentions subsetting with zero, which returns no values:
x[0] # character(0)
# This is not useful very often, but it can be helpful if you want to create unusual data structures to test your functions with.

      # 2. Subsetting with a logical vector keeps all values corresponding to a TRUE value. This is most often useful in conjunction with the comparison functions.

x <- c(10, 3, NA, 5, 8, 1, NA)

# All non-missing values of x
x[!is.na(x)] #> [1] 10  3  5  8  1

# All even (or missing!) values of x
x[x %% 2 == 0] #> [1] 10 NA  8 NA

      # 3. If you have a named vector, you can subset it with a character vector:

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]
#> xyz def 
#>   5   2
# Like with positive integers, you can also use a character vector to duplicate individual entries.

      # 4. The simplest type of subsetting is nothing, x[], which returns the complete x. This is not useful for subsetting vectors, but it is useful when subsetting matrices (and other high dimensional structures) because it lets you select all the rows or all the columns, by leaving that index blank. For example, if x is 2d, x[1, ] selects the first row and all the columns, and x[, -1] selects all rows and all columns except the first.

      # To learn more about the applications of subsetting, reading the “Subsetting” chapter of Advanced R: http://adv-r.had.co.nz/Subsetting.html#applications.

      # There is an important variation of [ called [[. [[ only ever extracts a single element, and always drops names. It’s a good idea to use it whenever you want to make it clear that you’re extracting a single item, as in a for loop. 

  # 4.6 Exercises
  #~~~~~~~~~~~~~~~~

    # > 1. What does mean(is.na(x)) tell you about a vector x? What about sum(!is.finite(x))?

x <- c(10, 3, NA, 5, 8, 1, NA)
mean(is.na(x)) # [1] 0.2857143
# It's the proportion of NA's within a vector. Here it's 28% of the values.

sum(!is.finite(x)) # [1] 2
# It's the number of non finite values within a vector. That could be NA, NaN or +/- Inf.

    # > 2. Carefully read the documentation of is.vector(). What does it actually test for? Why does is.atomic() not agree with the definition of atomic vectors above?

      # is.vector() only checks whether the object has no attributes other than names. Thus a list is a vector:
is.vector(list(a = 1, b = 2)) # [1] TRUE

      # But any object that has an attribute (other than names) is not:
x <- 1:10
attr(x, "something") <- TRUE
is.vector(x) # [1] FALSE

      # The idea behind this is that object oriented classes will include attributes, including, but not limited to "class".

      # The function is.atomic() explicitly checks whether an object is one of the atomic types (“logical”, “integer”, “numeric”, “complex”, “character”, and “raw”) or NULL.
is.atomic(1:10) # [1] TRUE
is.atomic(list(a = 1)) # [1] FALSE

      # The function is.atomic() will consider objects to be atomic even if they have extra attributes.
is.atomic(x) # [1] TRUE

    # > 3. Compare and contrast setNames() with purrr::set_names().

    # The function set_names() has more ways to set the names than setNames().  
    # The names can be specified in the same manner as setNames().
purrr::set_names(1:4, c("a", "b", "c", "d"))

    # The names can also be specified as unnamed arguments,
purrr::set_names(1:4, "a", "b", "c", "d")

    # The function set_names() will name an object with itself if no nm argument is provided (the opposite of setNames() behavior).
purrr::set_names(c("a", "b", "c", "d"))

    # The biggest difference between set_names() and setNames() is that set_names() allows for using a function or formula to transform the existing names.
purrr::set_names(c(a = 1, b = 2, c = 3), toupper)
purrr::set_names(c(a = 1, b = 2, c = 3), ~ toupper(.))

    # also checks that the length of the names argument is the same length as the vector
purrr::set_names(1:4, c("a", "b"))

    # The setNames() function will allow the names to be shorter than the vector being named, and will set the missing names to NA.
setNames(1:4, c("a", "b"))

    # > 4. Create functions that take a vector as input and returns:

x <- c(1:10, NA, NaN, Inf, -Inf)

      # 1. The last value. Should you use [ or [[? We can use either [ or [[ the second one always drop names so if we want only the value the second one is prefered.
last_value <- function(x) {
  x[[length(x)]]
}
last_value(x)

      # 2. The elements at even numbered positions. I use the [ call because it allows subsetting multiple values.
pos_even <- function(x) {
  x[x %% 2 == 0]
}
pos_even(x)

      # 3. Every element except the last value.
all_except_last <- function(x) {
  x[-length(x)]
}
all_except_last(x)

      # 4. Only even numbers (and no missing values).
pos_even_fin <- function(x) {
  x[is.finite(x) & (x %% 2 == 0)]
}
pos_even_fin(x)

    # > 5. Why is x[-which(x > 0)] not the same as x[x <= 0]?

x[-which(x > 0)] # [1]   NA  NaN -Inf
# This function drop the values that are greater than 0, so it drops all integers greater than 0 and Inf, and return negative ones, Not a number and missing values.
x[x <= 0] # > [1]   NA   NA -Inf

      # Recall how the logical relational operators (<, <=, ==, !=, >, >=) treat NA values. Any relational operation that includes a NA returns an NA. Is NA <= 0? We don’t know because it depends on the unknown value of NA, so the answer is NA. This same argument applies to NaN. Asking whether NaN <= 0 does not make sense because you can’t compare a number to “Not a Number”.

      # Now consider the expression x[-which(x > 0)]. As before, to understand this expression we’ll work from the inside out. Consider x > 0.
x > 0 
#  [1]  TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE NA NA TRUE FALSE

      # As with x <= 0, it returns NA for comparisons involving NA and NaN.

      # What does which() do?
which(x > 0) # [1]  1  2  3  4  5  6  7  8  9 10 13
      # The which() function returns the indexes for which the argument is TRUE. This means that it is not including the indexes for which the argument is FALSE or NA.

      # There’s one other special case that we should consider. How do these two expressions work with an empty vector?
x <- numeric()
x[x <= 0] # numeric(0)
x[-which(x > 0)] # numeric(0)

      # This exercise is a reminder to always test your code. Even though these two expressions looked equivalent, they are not in practice. And when you do test code, consider both how it works on typical values as well as special values and edge cases, like a vector with NA or NaN or Inf values, or an empty vector. These are where unexpected behavior is most likely to occur.

    # > 6. What happens when you subset with a positive integer that’s bigger than the length of the vector? What happens when you subset with a name that doesn’t exist?

x <- c(a = 10, b = 20)

      # If we subset it by an integer larger than its length, it returns a vector of missing values.
x[3]
#> <NA> 
#>   NA

      # This also applies to ranges.
x[3:5]
#> <NA> <NA> <NA> 
#>   NA   NA   NA

      # If some indexes are larger than the length of the vector, those elements are NA.
x[1:5]
#>    a    b <NA> <NA> <NA> 
#>   10   20   NA   NA   NA

      # Likewise, when [ is provided names not in the vector’s names, it will return NA for those elements.
x["c"]
#> <NA> 
#>   NA
x[c("c", "d", "e")]
#> <NA> <NA> <NA> 
#>   NA   NA   NA
x[c("a", "b", "c")]
#>    a    b <NA> 
#>   10   20   NA

      # Though not yet discussed much in this chapter, the [[ behaves differently. With an atomic vector, if [[ is given an index outside the range of the vector or an invalid name, it raises an error.
x[["c"]]
#> Error in x[["c"]]: subscript out of bounds
x[[5]]
#> Error in x[[5]]: subscript out of bounds


# 5. Recursive vectors (lists)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Lists are a step up in complexity from atomic vectors, because lists can contain other lists. This makes them suitable for representing hierarchical or tree-like structures. You create a list with list():
x <- list(1, 2, 3)
x

  # A very useful tool for working with lists is str() because it focusses on the **str**ucture, not the contents.
str(x)
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

  # Unlike atomic vectors, list() can contain a mix of objects:
y <- list("a", 1L, 1.5, TRUE)
str(y)

  # Lists can even contain other lists!
z <- list(list(1, 2), list(3, 4))
str(z)

  # 5.1 Visualising lists
  #~~~~~~~~~~~~~~~~~~~~~~~~

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

    # There are three principles:

      # 1. Lists have rounded corners. Atomic vectors have square corners.
      # 2. Children are drawn inside their parent, and have a slightly darker background to make it easier to see the hierarchy.
      # 3. The orientation of the children (i.e. rows or columns) isn’t important, so I’ll pick a row or column orientation to either save space or illustrate an important property in the example.

    # 5.2 Subsetting
    #~~~~~~~~~~~~~~~~

      # There are three ways to subset a list, which I’ll illustrate with a list named a:

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

        # * [ extracts a sub-list. The result will always be a list.

str(a[1:2])
# List of 2
# $ a: int [1:3] 1 2 3
# $ b: chr "a string"
str(a[4])
# List of 1
# $ d:List of 2
# ..$ : num -1
# ..$ : num -5

          # Like with vectors, you can subset with a logical, integer, or character vector.

        # * [[ extracts a single component from a list. It removes a level of hierarchy from the list.

str(a[[1]])
#>  int [1:3] 1 2 3
str(a[[4]])
#> List of 2
#>  $ : num -1
#>  $ : num -5

        # * $ is a shorthand for extracting named elements of a list. It works similarly to [[ except that you don’t need to use quotes.

a$a
#> [1] 1 2 3
a[["a"]]
#> [1] 1 2 3

      # The distinction between [ and [[ is really important for lists, because [[ drills down into the list while [ returns a new, smaller list.

    # 5.3 Lists of condiments
    #~~~~~~~~~~~~~~~~~~~~~~~~~~

      # If this pepper shaker is your list x, then, x[1] is a pepper shaker containing a single pepper packet.
      # x[2] would look the same, but would contain the second packet. x[1:2] would be a pepper shaker containing two pepper packets.
      # x[[1]] is the pepper packet.
      # If you wanted to get the content of the pepper package, you’d need x[[1]][[1]]. (only the pepper)

    # 5.4 Exercises
    #~~~~~~~~~~~~~~~

      # > 1. Draw the following lists as nested sets:
        # 1. list(a, b, list(c, d), list(e, f))
        # 2. list(list(list(list(list(list(a))))))

      # > 2. What happens if you subset a tibble as if you’re subsetting a list? What are the key differences between a list and a tibble?

str(flights[[1]]) 
# int [1:336776] 2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
        # That return a vector.
str(flights[1]) 
# tibble [336,776 x 1] (S3: tbl_df/tbl/data.frame)
# $ year: int [1:336776] 2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
        # That return a one column tibble.

        # Subsetting a tibble works the same way as a list; a data frame can be thought of as a list of columns. The key difference between a list and a tibble is that all the elements (columns) of a tibble must have the same length (number of rows). Lists can have vectors with different lengths as elements.


# 6. Attributes
#~~~~~~~~~~~~~~~~

  # Any vector can contain arbitrary additional metadata through its **attributes**. You can think of attributes as named list of vectors that can be attached to any object. You can get and set individual attribute values with attr() or see them all at once with attributes().

x <- 1:10
attr(x, "greeting")
#> NULL
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)
#> $greeting
#> [1] "Hi!"
#> 
#> $farewell
#> [1] "Bye!"

  # There are three very important attributes that are used to implement fundamental parts of R:

    # 1. Names are used to name the elements of a vector.
    # 2. Dimensions (dims, for short) make a vector behave like a matrix or array.
    # 3. Class is used to implement the S3 object oriented system.

  # The class controls how **generic functions** work. Generic functions are key to object oriented programming in R, because they make functions behave differently for different classes of input. you can read more about object oriented programming in Advanced R at http://adv-r.had.co.nz/OO-essentials.html#s3.

  # Here’s what a typical generic function looks like:

as.Date
# function (x, ...) 
# UseMethod("as.Date")
# <bytecode: 0x000001cf0f93e330>
# <environment: namespace:base>

  # The call to “UseMethod” means that this is a generic function, and it will call a specific **method**, a function, based on the class of the first argument. (All methods are functions; not all functions are methods). You can list all the methods for a generic with methods():

methods("as.Date")
#[1] as.Date.character as.Date.default  as.Date.factor      as.Date.numeric 
#[5] as.Date.POSIXct  as.Date.POSIXlt  as.Date.vctrs_sclr* as.Date.vctrs_vctr*
# see '?methods' for accessing help and source code

  # For example, if x is a character vector, as.Date() will call as.Date.character(); if it’s a factor, it’ll call as.Date.factor().

  # You can see the specific implementation of a method with getS3method():

getS3method("as.Date", "default")
#> function (x, ...) 
#> {
#>     if (inherits(x, "Date")) 
#>         x
#>     else if (is.logical(x) && all(is.na(x))) 
#>         .Date(as.numeric(x))
#>     else stop(gettextf("do not know how to convert '%s' to class %s", 
#>         deparse1(substitute(x)), dQuote("Date")), domain = NA)
#> }
#> <bytecode: 0x53c6430>
#> <environment: namespace:base>

getS3method("as.Date", "numeric")
#> function (x, origin, ...) 
#> {
#>     if (missing(origin)) 
#>         stop("'origin' must be supplied")
#>     as.Date(origin, ...) + x
#> }
#> <bytecode: 0x7b4bcd0>
#> <environment: namespace:base>

  # The most important S3 generic is print(): it controls how the object is printed when you type its name at the console. Other important generics are the subsetting functions [, [[, and $.


# 7. Augmented vectors
#~~~~~~~~~~~~~~~~~~~~~~

  # Atomic vectors and lists are the building blocks for other important vector types like factors and dates. I call these augmented vectors, because they are vectors with additional attributes, including class. Because augmented vectors have a class, they behave differently to the atomic vector on which they are built. In this book, we make use of four important augmented vectors:

  # 7.1 Factors
  #~~~~~~~~~~~~~~

    # Factors are designed to represent categorical data that can take a fixed set of possible values. Factors are built on top of integers, and have a levels attribute:

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x) #> [1] "integer"
attributes(x)
#> $levels
#> [1] "ab" "cd" "ef"
#> 
#> $class
#> [1] "factor"

  # 7.2 Dates and date-times
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Dates in R are numeric vectors that represent the number of days since 1 January 1970.

x <- as.Date("1971-01-01")
unclass(x) #> [1] 365

typeof(x) #> [1] "double"
attributes(x)
#> $class
#> [1] "Date"

    # Date-times are numeric vectors with class POSIXct that represent the number of seconds since 1 January 1970. (In case you were wondering, “POSIXct” stands for “Portable Operating System Interface”, calendar time.)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
#> [1] 3600
#> attr(,"tzone")
#> [1] "UTC"

typeof(x) #> [1] "double"
attributes(x)
#> $class
#> [1] "POSIXct" "POSIXt" 
#> 
#> $tzone
#> [1] "UTC"

    # The tzone attribute is optional. It controls how the time is printed, not what absolute time it refers to.

attr(x, "tzone") <- "US/Pacific"
x #> [1] "1969-12-31 17:00:00 PST"

attr(x, "tzone") <- "US/Eastern"
x #> [1] "1969-12-31 20:00:00 EST"

    # There is another type of date-times called POSIXlt. These are built on top of named lists:

y <- as.POSIXlt(x)
typeof(y) #> [1] "list"

attributes(y)
#> $names
#>  [1] "sec"    "min"    "hour"   "mday"   "mon"    "year"   "wday"   "yday"  
#>  [9] "isdst"  "zone"   "gmtoff"
#> 
#> $class
#> [1] "POSIXlt" "POSIXt" 
#> 
#> $tzone
#> [1] "US/Eastern" "EST"        "EDT"

    # POSIXlts are rare inside the tidyverse. They do crop up in base R, because they are needed to extract specific components of a date, like the year or month. Since lubridate provides helpers for you to do this instead, you don’t need them. POSIXct’s are always easier to work with, so if you find you have a POSIXlt, you should always convert it to a regular data time lubridate::as_date_time().

  # 7.3 Tibbles
  #~~~~~~~~~~~~~

    # Tibbles are augmented lists: they have class “tbl_df” + “tbl” + “data.frame”, and names (column) and row.names attributes:

tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb) #> [1] "list"

attributes(tb)
#> $names
#> [1] "x" "y"
#> 
#> $row.names
#> [1] 1 2 3 4 5
#> 
#> $class
#> [1] "tbl_df"     "tbl"        "data.frame"

    # The difference between a tibble and a list is that all the elements of a data frame must be vectors with the same length. All functions that work with tibbles enforce this constraint.

    # Traditional data.frames have a very similar structure:

df <- data.frame(x = 1:5, y = 5:1)
typeof(df) #> [1] "list"

attributes(df)
#> $names
#> [1] "x" "y"
#> 
#> $class
#> [1] "data.frame"
#> 
#> $row.names
#> [1] 1 2 3 4 5

    # The main difference is the class. The class of tibble includes “data.frame” which means tibbles inherit the regular data frame behaviour by default.

  # 7.4 Excercises
  #~~~~~~~~~~~~~~~~

    # > 1. What does hms::hms(3600) return? How does it print? What primitive type is the augmented vector built on top of? What attributes does it use?

      # hms::hms returns an object of class, and prints the time in “%H:%M:%S” format.
x <- hms::hms(3600) # 01:00:00
class(x) # [1] "hms"      "difftime"
x # 01:00:00

      # The primitive type is a double
typeof(x) # [1] "double"

      # The attributes is uses are "units" and "class".
attributes(x)
# $units
# [1] "secs"
#
# $class
# [1] "hms"      "difftime"

    # < 2. Try and make a tibble that has columns with different lengths. What happens?

      # We've got an error message.
tb <- tibble::tibble(x = 1:5, y = 5:2)
# Erreur : Tibble columns must have compatible sizes.
# * Size 5: Existing data.
# * Size 4: Column `y`.
# i Only values of size one are recycled.
# Run `rlang::last_error()` to see where the error occurred.

      # If I try to create a tibble with a scalar and column of a different length there are no issues, and the scalar is repeated to the length of the longer vector.
tibble::tibble(x = 1, y = 1:5)
#> # A tibble: 5 x 2
#>       x     y
#>   <dbl> <int>
#> 1     1     1
#> 2     1     2
#> 3     1     3
#> 4     1     4
#> 5     1     5

    # > 3. Based on the definition above, is it ok to have a list as a column of a tibble?

      # From the above, the error message was about vectors having different lengths. But there is nothing that prevents a tibble from having vectors of different types: doubles, character, integers, logical, factor, date. The later are still atomic, but they have additional attributes. So, maybe there won’t be an issue with a list vector as long as it is the same length.

tibble::tibble(x = 1:3, y = list("a", 1, list(1:3)))
#> # A tibble: 3 x 2
#>       x y         
#>   <int> <list>    
#> 1     1 <chr [1]> 
#> 2     2 <dbl [1]> 
#> 3     3 <list [1]>
