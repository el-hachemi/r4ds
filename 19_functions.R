#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#
#                              19. FUNCTIONS                                #
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===# 

# 1. Introduction
#~~~~~~~~~~~~~~~~~~

  # Writing a function has three big advantages over using copy-and-paste:

    # 1. You can give a function an evocative name that makes your code easier to understand.
    # 2. As requirements change, you only need to update code in one place, instead of many.
    # 3. You eliminate the chance of making incidental mistakes when you copy and paste (i.e. updating a variable name in one place, but not in another).

  # 1.1 Prerequisities
  #~~~~~~~~~~~~~~~~~~~~~
    # The focus of this chapter is on writing functions in base R, so you won’t need any extra packages.

# 2. When should you write a function?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # You should consider writing a function whenever you’ve copied and pasted a block of code more than twice

df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

  # You might be able to puzzle out that this rescales each column to have a range from 0 to 1. But did you spot the mistake? I made an error when copying-and-pasting the code for df$b: I forgot to change an a to a b. Extracting repeated code out into a function is a good idea because it prevents you from making this type of mistake.

  # To write a function you need to first analyse the code. How many inputs does it have?

(df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

  # This code only has one input: df$a. To make the inputs more clear, it’s a good idea to rewrite the code using temporary variables with general names.

x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  # There is some duplication in this code. We’re computing the range of the data three times, so it makes sense to do it in one step:

rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

  # Pulling out intermediate calculations into named variables is a good practice because it makes it more clear what the code is doing. Now that I’ve simplified the code, and checked that it still works, I can turn it into a function:

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))

  # There are three key steps to creating a new function:

    # 1. You need to pick a **name** for the function. Here I’ve used rescale01 because this function rescales a vector to lie between 0 and 1.

    # 2. You list the inputs, or **arguments**, to the function inside function. Here we have just one argument. If we had more the call would look like function(x, y, z).

    # 3. You place the code you have developed in body of the function, a { block that immediately follows function(...).

  # Note the overall process: I only made the function after I’d figured out how to make it work with a simple input. It’s easier to start with working code and turn it into a function; it’s harder to create a function and then try to make it work.

  # At this point it’s a good idea to check your function with a few different inputs:

rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))

  # As you write more and more functions you’ll eventually want to convert these informal, interactive tests into formal, automated tests. That process is called unit testing. http://r-pkgs.had.co.nz/tests.html

  # We can simplify the original example now that we have a function:

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

  # Compared to the original, this code is easier to understand and we’ve eliminated one class of copy-and-paste errors. There is still quite a bit of duplication since we’re doing the same thing to multiple columns. We’ll learn how to eliminate that duplication in iteration, once you’ve learned more about R’s data structures in vectors.

  # Another advantage of functions is that if our requirements change, we only need to make the change in one place. For example, we might discover that some of our variables include infinite values, and rescale01() fails:

x <- c(1:10, Inf)
rescale01(x)

  # Because we’ve extracted the code into a function, we only need to make the fix in one place:

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

  # This is an important part of the “do not repeat yourself” (or DRY) principle. The more repetition you have in your code, the more places you need to remember to update when things change (and they always do!), and the more likely you are to create bugs over time.

  # 2.1 Exercises
  #~~~~~~~~~~~~~~~~

  # > 1. Why is TRUE not a parameter to rescale01()? What would happen if x contained a single missing value, and na.rm was FALSE?

rescale01_alt <- function(x, na.rm = FALSE) {
  rng <- range(x, na.rm = na.rm, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

    # If x contains a single missing value and na.rm = FALSE, then this function stills return a non-missing value.

rescale01_alt(c(NA, 1:5), na.rm = FALSE)
rescale01_alt(c(NA, 1:5), na.rm = TRUE)

    # The option finite = TRUE to range() will drop all non-finite elements, and NA is a non-finite element.
    # However, if both finite = FALSE and na.rm = FALSE, then this function will return a vector of NA values. Recall, arithmetic operations involving NA values return NA.

rescale01_alt2 <- function(x, na.rm = FALSE, finite = FALSE) {
  rng <- range(x, na.rm = na.rm, finite = finite)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01_alt2(c(NA, 1:5), na.rm = FALSE, finite = FALSE)

  # > 2. In the second variant of rescale01(), infinite values are left unchanged. Rewrite rescale01() so that -Inf is mapped to 0, and Inf is mapped to 1.

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == -Inf] <- 0
  y[y == Inf] <- 1
  y
}
rescale01(c(Inf, -Inf, 0:5, NA))

  # > 3. Practice turning the following code snippets into functions. Think about what each function does. What would you call it? How many arguments does it need? Can you rewrite it to be more expressive or less duplicative?

mean(is.na(x))
    # This code calculates the proportion of NA values in a vector.
prop_na <- function(x){
  mean(is.na(x))
}
prop_na(x)

x / sum(x, na.rm = TRUE)
    # This code calculate the proportion of each element of x, I've set the default of na.rm equal FALSE to be consistant with many of R functions like sum() ans mean() 
prop <- function(x, na.rm = FALSE){
  x / sum(x, na.rm = na.rm)
}
prop(x)

sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
    # This code calculate the coefficient of variation, (assuming that x can only take non-negative values)
coef_var <- function(x, na.rm = FALSE){
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

  # > 4. write your own functions to compute the variance and skewness of a numeric vector.

variance <- function(x, na.rm = FALSE){
  
  n_na <- length(x[is.na(x)])
  n <- length(x) - n_na
  s_mean <- sum(x, na.rm = na.rm) / n
  
  sum((x - s_mean) ^ 2, na.rm = na.rm) / (n - 1)
}
variance(x, na.rm = TRUE)

skew <- function(x, na.rm = FALSE){
  n_na <- length(x[is.na(x)])
  n <- length(x) - n_na
  s_mean <- sum(x, na.rm = na.rm) / n
  variance <- variance(x, na.rm = na.rm)
  
  1 / (n - 2) * sum(x - s_mean, na.rm = na.rm) ^ 3 / variance ^ (3 / 2)
}
skew(x, na.rm = TRUE)

  # > 5. Write both_na(), a function that takes two vectors of the same length and returns the number of positions that have an NA in both vectors.

both_na <- function(x, y){
  x_pos <- which(is.na(x))
  y_pos <- which(is.na(y))
  writeLines(c("First vector NA's positons are: ", x_pos, 
               "Second vector NA's positions are: ", y_pos))
}

  # > 6. What do the following functions do? Why are they useful even though they are so short?

is_directory <- function(x) file.info(x)$isdir
    # This function tell us if a file is a directory on not.
is_readable <- function(x) file.access(x, 4) == 0
    # This function tell if a file is readable or not.

  # > 7. Read the [complete lyrics](https://en.wikipedia.org/wiki/Little_Bunny_Foo_Foo) to “Little Bunny Foo Foo”. There’s a lot of duplication in this song. Extend the initial piping example to recreate the complete song, and use functions to reduce the duplication.

    # Little Bunny Foo Foo,
    # Hopping through the forest,
    # Scooping up the field mice,
    # And bopping them on the head.

    # (Spoken)
    # Down came the Good Fairy, and she said,

    # "Little Bunny Foo Foo,
    # I don't want to see you,
    # Scooping up the field mice
    # And bopping them on the head."

    # (Spoken)
    # "I'll give you three chances,
    # And if you don't behave,
    # I'm gonna turn you into a goonie!"

    # The next day... or That evening... or Later that night...

    # Little Bunny Foo Foo,
    # Hopping through the forest,
    # Scooping up the field mice,
    # And bopping them on the head.

    # (Spoken)
    # Down came the Good Fairy, and she said,

    # "Little Bunny Foo Foo,
    # I don't want to see you,
    # Scooping up the field mice
    # And bopping them on the head."

    # (Spoken)
    # "I'll give you two more chances,
    # And if you don't behave,
    # I'm gonna turn you into a goonie!"

    # The next day... or That evening... or Later that night...

    # Little Bunny Foo Foo,
    # Hopping through the forest,
    # Scooping up the field mice,
    # And bopping them on the head.

    # (Spoken)
    # Down came the Good Fairy, and she said,

    # "Little Bunny Foo Foo,
    # I don't want to see you,
    # Scooping up the field mice
    # And bopping them on the head."

    # (Spoken)
    # "I'll give you one more chance,
    # And if you don't behave,
    # I'm gonna turn you into a goonie!"

    # The next day... or That evening... or Later that night...

    # Little Bunny Foo Foo,
    # Hopping through the forest,
    # Scooping up the field mice,
    # And bopping them on the head.

    # (Spoken)
    # Down came the Good Fairy, and she said,

    # "Little Bunny Foo Foo,
    # I don't want to see you,
    # Scooping up the field mice
    # And bopping them on the head."

    # (Spoken)
    # "I gave you three chances,
    # And you didn't behave,
    # And now I'm gonna turn you into a goonie. POOF!"

foo_foo <- little_bunny()
fairy <- good_fairy()

behave <- foo_foo %>% 
  hop(through = forest) %>% 
  scoop(up = field_mice) %>% 
  bop(on = head)

chances <- fairy %>% 
  down_came() %>% 
  say(
    c(
      "Little Bunny Foo Foo,
      I don't want to see you,
      Scooping up the field mice
      And bopping them on the head."
    )
  ) 

n = 3
if (behave = behave & n != 0){
  n = n - 1
  behave
  chances
} else if (n = 0){
  fairy %>% 
    turn(foo_foo, into = goonie)
}

# 3. Functions are for humans and computers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # The name of a function is important. Ideally, the name of your function will be short, but clearly evoke what the function does.

  # Generally, function names should be verbs, and arguments should be nouns. There are some exceptions: nouns are ok if the function computes a very well known noun (i.e. mean() is better than compute_mean()), or accessing some property of an object (i.e. coef() is better than get_coefficients()).

f() # Too short

my_awesome_function() # Not a verb, or descriptive

impute_missing() # Long, but clear
collapse_years() # Long, but clear

  # If your function name is composed of multiple words, I recommend using “snake_case”, where each lowercase word is separated by an underscore. camelCase is a popular alternative. It doesn’t really matter which one you pick, the important thing is to be consistent:

col_mins <- function(x, y) {} # Never do this!
rowMaxes <- function(y, x) {} # Never do this!

  # If you have a family of functions that do similar things, make sure they have consistent names and arguments. Use a common prefix to indicate that they are connected. That’s better than a common suffix because autocomplete allows you to type the prefix and see all the members of the family.

# Good
input_select()
input_checkbox()
input_text()

# Not so good
select_input()
checkbox_input()
text_input()

  # A good example of this design is the stringr package: if you don’t remember exactly which function you need, you can type str_ and jog your memory.

  # Where possible, avoid overriding existing functions and variables. It’s impossible to do in general because so many good names are already taken by other packages, but avoiding the most common names from base R will avoid confusion.

# Don't do this!
T <- FALSE
c <- 10
mean <- function(x) sum(x)

  # Use comments, lines starting with #, to explain the “why” of your code. You generally should avoid comments that explain the “what” or the “how”. If you can’t understand what the code does from reading it, you should think about how to rewrite it to be more clear. Do you need to add some intermediate variables with useful names? Do you need to break out a subcomponent of a large function so you can name it? However, your code can never capture the reasoning behind your decisions: why did you choose this approach instead of an alternative? What else did you try that didn’t work? It’s a great idea to capture that sort of thinking in a comment.

  # Another important use of comments is to break up your file into easily readable chunks. Use long lines of - and = to make it easy to spot the breaks.

# Load data --------------------------------------

# Plot data --------------------------------------

  # RStudio provides a keyboard shortcut to create these headers (Cmd/Ctrl + Shift + R), and will display them in the code navigation drop-down at the bottom-left of the editor

  # 3.1 Exercises
  #~~~~~~~~~~~~~~~

    # > 1. Read the source code for each of the following three functions, puzzle out what they do, and then brainstorm better names.

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
      # Function that let us know if a string has a prefix or not; has_prefix()

f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
      # Function that render all the elements of a string except the last one; except_last()

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}
      # It render a replication of y where the length of the ouptput is equal to the length of x; rep_x_length.

    # > 2. Take a function that you’ve written recently and spend 5 minutes brainstorming a better name for it and its arguments.

      # for both_na I prefer na_pos()

    # > 3. Compare and contrast rnorm() and MASS::mvrnorm(). How could you make them more consistent?
?rnorm
MASS::mvrnorm
      # rnorm() returns values drawn from specified normal distribution. mvrnorm() returns values draw from specified multivariable normal distribution. The names are self explanatory, and the argument names are correct and already consistent with the methods.

    # > 4. Make a case for why norm_r(), norm_d() etc would be better than rnorm(), dnorm(). Make a case for the opposite.

      # If named norm_r() and norm_d(), the naming convention groups functions by their distribution.
      # If named rnorm(), and dnorm(), the naming convention groups functions by the action they perform.
          # r* functions always sample from distributions: for example, rnorm(), rbinom(), runif(), and rexp().
          # d* functions calculate the probability density or mass of a distribution: For example, dnorm(), dbinom(), dunif(), and dexp().
      # R distributions use this latter naming convention.

# 4. Conditional execution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (condition) {
  # code executed when condition is TRUE
} else {
  # code executed when condition is FALSE
}

  # To get help on if you need to surround it in backticks: ?`if`.

  # Here’s a simple function that uses an if statement. The goal of this function is to return a logical vector describing whether or not each element of a vector is named.

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
  # This function takes advantage of the standard return rule: a function returns the last value that it computed. Here that is either one of the two branches of the if statement.

  # 4.1 Conditions
  #~~~~~~~~~~~~~~~~~

    # The condition must evaluate to either TRUE or FALSE. If it’s a vector, you’ll get a warning message; if it’s an NA, you’ll get an error. 

if (c(TRUE, FALSE)) {}
if (NA) {}

    # You can use || (or) and && (and) to combine multiple logical expressions. These operators are “short-circuiting”: as soon as || sees the first TRUE it returns TRUE without computing anything else. As soon as && sees the first FALSE it returns FALSE. If you do have a logical vector, you can use any() or all() to collapse it to a single value.

    # Be careful when testing for equality. == is vectorised, which means that it’s easy to get more than one output. Either check the length is already 1, collapse with all() or any(), or use the non-vectorised identical(). identical() is very strict: it always returns either a single TRUE or a single FALSE, and doesn’t coerce types. This means that you need to be careful when comparing integers and doubles:
identical(0L, 0)

    # You also need to be wary of floating point numbers:
x <- sqrt(2) ^ 2
x
x == 2
x - 2

    # Instead use dplyr::near()
    # And remember, x == NA doesn’t do anything useful!

  # 4.2 Multiple conditions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~

    # You can chain multiple if statements together:

if (this) {
  # do that
} else if (that) {
  # do something else
} else {
  # 
}

    # But if you end up with a very long series of chained if statements, you should consider rewriting. One useful technique is the switch() function. It allows you to evaluate selected code based on position or name.

      #> function(x, y, op) {
      #>   switch(op,
      #>     plus = x + y,
      #>     minus = x - y,
      #>     times = x * y,
      #>     divide = x / y,
      #>     stop("Unknown op!")
      #>   )
      #> }

    # Another useful function that can often eliminate long chains of if statements is cut(). It’s used to discretise continuous variables.

  # 4.3 Code style
  #~~~~~~~~~~~~~~~~

    # Both if and function should (almost) always be followed by squiggly brackets ({}), and the contents should be indented by two spaces.
    # An opening curly brace should never go on its own line and should always be followed by a new line. A closing curly brace should always go on its own line, unless it’s followed by else. Always indent the code inside curly braces

# Good
if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad
if (y < 0 && debug)
  message("Y is negative")

if (y == 0) {
  log(x)
} 
else {
  y ^ x
}

    # It’s ok to drop the curly braces if you have a very short if statement that can fit on one line:
y <- 10
x <- if (y < 20) "Too low" else "Too high"
    # I recommend this only for very brief if statements. Otherwise, the full form is easier to read:
if (y < 20) {
  x <- "Too low" 
} else {
  x <- "Too high"
}

  # 4.4 Exercises
  #~~~~~~~~~~~~~~~~

    # > 1. What’s the difference between if and ifelse()? Carefully read the help and construct three examples that illustrate the key differences.
?'if'
?'ifelse'
      # From the help it's clearly mentionned that if is much more efficient and often much preferable to ifesle when length(test) == 1; So the keyworsd if tests a single condition, while ifelse tests each element.
x <- seq(as.Date("2000-02-29"), as.Date("2004-10-04"), by = "1 month")
y <- ifelse(as.POSIXlt(x)$mday == 29, x, NA)
if (as.POSIXlt(x)$mday == 29){
  x
} else {
  NA
} # In this example, I've had a warning that the condition has a length > 1 so only the first is used.

    # > 2. Write a greeting function that says “good morning”, “good afternoon”, or “good evening”, depending on the time of day. (Hint: use a time argument that defaults to lubridate::now(). That will make it easier to test your function.)
greeting <- function() { 
  h_now <- lubridate::hour(lubridate::now()) # I could make this in default.
  midnight <- lubridate::hour(lubridate::hm("00-00")) # I could do it as
  midday <- lubridate::hour(lubridate::hm("12-00"))   #  integer.
  height <- lubridate::hour(lubridate::hm("20-00"))
  if (h_now > midnight && h_now < midday) {
    "Good morning"
  } else if (h_now >= midday && h_now < height) {
    "Good afteroon"   # This is my code, and I miss to make lubridate::now()
  } else {            # as a default argument.
    "Good evening"
    }
}
      # from https://jrnold.github.io/r4ds-exercise-solutions/functions.html#conditional-execution
greet <- function(time = lubridate::now()) {
  hr <- lubridate::hour(time)
  # I don't know what to do about times after midnight,
  # are they evening or morning?
  if (hr < 12) {
    print("good morning")
  } else if (hr < 17) {
    print("good afternoon")
  } else {
    print("good evening")
  }

    # > 3. Implement a fizzbuzz function. It takes a single number as input. If the number is divisible by three, it returns “fizz”. If it’s divisible by five it returns “buzz”. If it’s divisible by three and five, it returns “fizzbuzz”. Otherwise, it returns the number. Make sure you first write working code before you create the function.

fizzbuzz <- function(x){
  if (x %% 3 == 0 && x %% 5 == 0) { # I learned here that if render the first 
    "fizzbuzz"                      # condition matched, so the order is  
  } else if (x %% 5 == 0) {         # important, the first one must be the
    "buzz"                          # one that have the narrowest range.
  } else if (x %% 3 == 0) {
    "fizz"
  } else {
    x
  }
}

      # from https://jrnold.github.io/r4ds-exercise-solutions/functions.html#conditional-execution, A more concise way of checking for divisibility is to note that the not operator will return TRUE for 0, and FALSE for all non-zero numbers. Thus, !(x %% y), will check whether y divides x.
!(1:10 %% 3) # So the code can be like this:
fizzbuzz <- function(x) {
  # these two lines check that x is a valid input
  stopifnot(length(x) == 1)
  stopifnot(is.numeric(x))
  if (!(x %% 3) && !(x %% 5)) {
    "fizzbuzz"
  } else if (!(x %% 3)) {
    "fizz"
  } else if (!(x %% 5)) {
    "buzz"
  } else {
    # ensure that the function returns a character vector
    as.character(x)
  }
}
      # This function can be slightly improved by combining the first two lines conditions so we only check whether x is divisible by 3 once.
fizzbuzz2 <- function(x) {
  # these two lines check that x is a valid input
  stopifnot(length(x) == 1)
  stopifnot(is.numeric(x))
  if (!(x %% 3)) {
    if (!(x %% 5)) {
      "fizzbuzz"
    } else {
      "fizz"
    }
  } else if (!(x %% 5)) {
    "buzz"
  } else {
    # ensure that the function returns a character vector
    as.character(x)
  }
}
      # Instead of only accepting one number as an input, we could a FizzBuzz function that works on a vector. The case_when() function vectorizes multiple if-else conditions, so is perfect for this task. In fact, fizz-buzz is used in the examples in the documentation of case_when().
fizzbuzz_vec <- function(x) {
  case_when(
    !(x %% 3) & !(x %% 5) ~ "fizzbuzz",
    !(x %% 3) ~ "fizz",
    !(x %% 5) ~ "buzz",
    TRUE ~ as.character(x)
  )
}
fizzbuzz_vec(c(0, 1, 2, 3, 5, 9, 10, 12, 15))

      # The following function is an example of a vectorized FizzBuzz function that only uses bracket assignment.
fizzbuzz_vec2 <- function(x) {
  y <- as.character(x)
  # put the individual cases first - any elements divisible by both 3 and 5
  # will be overwritten with fizzbuzz later
  y[!(x %% 3)] <- "fizz"
  y[!(x %% 3)] <- "buzz"
  y[!(x %% 3) & !(x %% 5)] <- "fizzbuzz"
  y
}
fizzbuzz_vec2

      # This question, called the [“Fizz Buzz”](https://en.wikipedia.org/wiki/Fizz_buzz) question, is a common programming interview question used for screening out programmers who can’t program.[^fizzbuzz]

    # > 4. How could you use cut() to simplify this set of nested if-else statements?
if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}  # How would you change the call to cut() if I’d used < instead of <=? What is the other chief advantage of cut() for this problem? (Hint: what happens if you have many values in temp?)

      # cut divides the range of x into intervals and codes the values in x according to which interval they fall.
temp <- seq(-10, 50, by = 5)
breaks <- c(-Inf, 0, 10, 20, 30, Inf)
cut(temp, breaks = breaks, 
    labels = c("freezing", "cold", "cool", "warm", "hot")
)

      # If we'd use < instead of <= the call will be with right = FALSE
cut(temp, breaks = breaks, labels = c("freezing", "cold", "cool", "warm", "hot"), right = FALSE, ordered_result = TRUE)

      # Two advantages of using cut is that it works on vectors, whereas if only works on a single value (I already demonstrated this above), and that to change comparisons I only needed to change the argument to right, but I would have had to change four operators in the if expression.

      # I don't know what's the chief advantage of cut() for this problem

    # > 5. What happens if you use switch() with numeric values?

      # In switch(n, ...), if n is numeric, it will return the nth argument from .... This means that if n = 1, switch() will return the first argument in ..., if n = 2, the second, and so on. For example,
switch(1, "apple", "banana", "cantaloupe")
switch(2, "apple", "banana", "cantaloupe")
      # If you use a non-integer number for the first argument of switch(), it will ignore the non-integer part.
switch(1.2, "apple", "banana", "cantaloupe")
switch(2.8, "apple", "banana", "cantaloupe")
      # Note that switch() truncates the numeric value, it does not round to the nearest integer. While it is possible to use non-integer numbers with switch(), you should avoid it

    # > 6. What does this switch() call do? What happens if x is “e”?
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)     # Experiment, then carefully read the documentation.

      # This switch call search for a matching value of x in the arguments, so if x is "e" the result will be NULL. I learned that I don't experiment by given values, I have such difficulty to assign by my own values to variables. I can't figure out how to do it. I struggle to choose a set of values to give to a variable. And in this case I could give to x the "e" value and see what happen with switch:
x <- "e"
      # And by doing this the function didn't give anything and from the documentation I remember that is a silent NULL, cause we haven't a an unnamed element whose will be return in the case of non match.
experiment_switch <- function(x){
  switch(x,
         a = ,
         b = "ab",
         c = ,
         d = "cd"
  )
}
experiment_switch("a") # "ab" <- "a" matched with non value, the next returned
experiment_switch("b") # "ab"
experiment_switch("c") # "cd"
experiment_switch("d") # "cd"
experiment_switch("e") # <- a silent NULL because "e" is non mtched
experiment_switch("f") # <- a silent NULL because "f" is non mtched

      # So to avoid this a non named element will be returned if non matched
experiment_switch_2 <- function(x){
  switch(x,
         a = ,
         b = "ab",
         c = ,
         d = "cd",
         NULL # value to return if not matched
  )
}
experiment_switch_2("e") # "NULL" 


# 5. Function arguments
#~~~~~~~~~~~~~~~~~~~~~~~~

  # The arguments to a function typically fall into two broad sets: one set supplies the data to compute on, and the other supplies arguments that control the details of the computation.

  # You specify a default value in the same way you call a function with a named argument:

mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
} # Compute confidence interval around mean using normal approximation

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)

  # The default value should almost always be the most common value. The few exceptions to this rule are to do with safety. For example, it makes sense for na.rm to default to FALSE because missing values are important. Even though na.rm = TRUE is what you usually put in your code, it’s a bad idea to silently ignore missing values by default.

  # When you call a function, you typically omit the names of the data arguments, because they are used so commonly. If you override the default value of a detail argument, you should use the full name:

mean(1:10, na.rm = TRUE)  # Good
mean(x = 1:10, , FALSE) # Bad
mean(, TRUE, x = c(1:10, NA)) # Bad

  # You can refer to an argument by its unique prefix (e.g. mean(x, n = TRUE)), but this is generally best avoided given the possibilities for confusion.

  # Notice that when you call a function, you should place a space around = in function calls, and always put a space after a comma, not before (just like in regular English).
average <- mean(feet / 12 + inches, na.rm = TRUE) # Good
average<-mean(feet/12+inches,na.rm=TRUE)  # Bad

  # 5.1 Choosing names
  #~~~~~~~~~~~~~~~~~~~~

    # Generally you should prefer longer, more descriptive names, but there are a handful of very common, very short names. It’s worth memorising these:
      # * x, y, z: vectors.
      # * w: a vector of weights.
      # * df: a data frame.
      # * i, j: numeric indices (typically rows and columns).
      # * n: length, or number of rows.
      # * p: number of columns.
    # Otherwise, consider matching names of arguments in existing R functions. For example, use na.rm to determine if missing values should be removed.

  # 5.2 Checking values
  #~~~~~~~~~~~~~~~~~~~~~~

    # As you start to write more functions, you’ll eventually get to the point where you don’t remember exactly how your function works. At this point it’s easy to call your function with invalid inputs. To avoid this problem, it’s often useful to make constraints explicit. For example, imagine you’ve written some functions for computing weighted summary statistics:

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

    # What happens if x and w are not the same length?
wt_mean(1:6, 1:3) # > [1] 7.76
    # In this case, because of R’s vector recycling rules, we don’t get an error.

    # It’s good practice to check important preconditions, and throw an error (with stop()), if they are not true:

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

    # Be careful not to take this too far. There’s a tradeoff between how much time you spend making your function robust, versus how long you spend writing it. For example, if you also added a na.rm argument, I probably wouldn’t check it carefully:

wt_mean <- function(x, w, na.rm = FALSE) {
  if (!is.logical(na.rm)) {
    stop("`na.rm` must be logical")
  }
  if (length(na.rm) != 1) {
    stop("`na.rm` must be length 1")
  }
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}

    # This is a lot of extra work for little additional gain. A useful compromise is the built-in stopifnot(): it checks that each argument is TRUE, and produces a generic error message if not.

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")
#> Error in wt_mean(1:6, 6:1, na.rm = "foo"): is.logical(na.rm) is not TRUE

    # Note that when using stopifnot() you assert what should be true rather than checking for what might be wrong.

  # 5.3 Dot-dot-dot (…)
  #~~~~~~~~~~~~~~~~~~~~~~

    # Many functions in R take an arbitrary number of inputs:
sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) #> [1] 55
stringr::str_c("a", "b", "c", "d", "e", "f") #> [1] "abcdef"

    # How do these functions work? They rely on a special argument: ... (pronounced dot-dot-dot). This special argument captures any number of arguments that aren’t otherwise matched.

    # It’s useful because you can then send those ... on to another function. This is a useful catch-all if your function primarily wraps another function. For example, I commonly create these helper functions that wrap around str_c():

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10]) #> [1] "a, b, c, d, e, f, g, h, i, j"

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")
#> Important output -----------------------------------------------------------

    # Here ... lets me forward on any arguments that I don’t want to deal with to str_c(). It’s a very convenient technique. But it does come at a price: any misspelled arguments will not raise an error. This makes it easy for typos to go unnoticed:

x <- c(1, 2)
sum(x, na.mr = TRUE) #> [1] 4

    # If you just want to capture the values of the ..., use list(...).

  # 5.4 Lazy evaluation
  #~~~~~~~~~~~~~~~~~~~~~

    # Arguments in R are lazily evaluated: they’re not computed until they’re needed. That means if they’re never used, they’re never called. This is an important property of R as a programming language, but is generally not important when you’re writing your own functions for data analysis. You can read more about lazy evaluation at http://adv-r.had.co.nz/Functions.html#lazy-evaluation.

  # 5.5 Exercises
  #~~~~~~~~~~~~~~~~

    # > 1. What does commas(letters, collapse = "-") do? Why?
commas(letters, collapse = "-")
# > Error in stringr::str_c(..., collapse = ", ") : 
#     argument formel "collapse" correspondant à plusieurs arguments fournis

      # This is because when the argument collapse is given to commas(), it is passed to str_c() as part of .... In other words, the previous code is equivalent to
str_c(letters, collapse = "-", collapse = ", ")

      # One way to allow the user to override the separator in commas() is to add a collapse argument to the function.
commas <- function(..., collapse = ", ") {
  str_c(..., collapse = collapse)
}

    # > 2. It’d be nice if you could supply multiple characters to the pad argument, e.g. rule("Title", pad = "-+"). Why doesn’t this currently work? How could you fix it?
rule("Title", pad = "-+") # Suprisingly that work for me, with a notice that the width is two times larger, somhow logical for me.

rule <- function(..., pad = "-") {
  title <- paste0(...)
   width <- (getOption("width") - nchar(title) - 5) / nchar(pad)
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

# I fix this problem by dividing the width formula by the nuber of characters of pad with the nchar(), before that I tryed the length() but it didn't work for me. And it work very well for me even with three and four charcters
rule("Title", pad = "-+")
rule("Title", pad = "-+*")
rule("Title", pad = "-+*=")
rule("Title", pad = "-+@@@")

    # > 3. What does the trim argument to mean() do? When might you use it?

      # From the help page: the fraction (0 to 0.5) of observations to be trimmed from each end of x before the mean is computed. If trim is non-zero, a symmetrically trimmed mean is computed with a fraction of trim observations deleted from each end before the mean is computed.

      # This is useful for calculating a measure of central tendency that is robust to outliers.

    # > 4. The default value for the method argument to cor() is c("pearson", "kendall", "spearman"). What does that mean? What value is used by default?

      # It means that the method argument can take one of those three values. The first value, "pearson", is used by default.


# 6. Return value
#~~~~~~~~~~~~~~~~~~

  # Figuring out what your function should return is usually straightforward: it’s why you created the function in the first place! There are two things you should consider when returning a value:

      # 1. Does returning early make your function easier to read?
      # 2. Can you make your function pipeable?

  # 6.1 Explicit return statements
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # The value returned by the function is usually the last statement it evaluates, but you can choose to return early by using return(). I think it’s best to save the use of return() to signal that you can return early with a simpler solution. A common reason to do this is because the inputs are empty:

complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  
  # Complicated code here
}

    # Another reason is because you have a if statement with one complex block and one simple block. For example, you might write an if statement like this:

f <- function() {
  if (x) {
    # Do 
    # something
    # that
    # takes
    # many
    # lines
    # to
    # express
  } else {
    # return something short
  }
}

    # But if the first block is very long, by the time you get to the else, you’ve forgotten the condition. One way to rewrite it is to use an early return for the simple case:

f <- function() {
  if (!x) {
    return(something_short)
  }
  
  # Do 
  # something
  # that
  # takes
  # many
  # lines
  # to
  # express
}
  
    # This tends to make the code easier to understand, because you don’t need quite so much context to understand it.

  # 6.2 Writing pipeable functions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # If you want to write your own pipeable functions, it’s important to think about the return value. Knowing the return value’s object type will mean that your pipeline will “just work”. For example, with dplyr and tidyr the object type is the data frame.

    # There are two basic types of pipeable functions: transformations and side-effects. With transformations, an object is passed to the function’s first argument and a modified object is returned. With side-effects, the passed object is not transformed. Instead, the function performs an action on the object, like drawing a plot or saving a file. Side-effects functions should “invisibly” return the first argument, so that while they’re not printed they can still be used in a pipeline. For example, this simple function prints the number of missing values in a data frame:

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

    # If we call it interactively, the invisible() means that the input df doesn’t get printed out:

show_missings(mtcars) #> Missing values: 0

    # But it’s still there, it’s just not printed by default:

x <- show_missings(mtcars)  #> Missing values: 0
class(x) #> [1] "data.frame"
dim(x) #> [1] 32 11

    # And we can still use it in a pipe:

mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() 
#> Missing values: 0
#> Missing values: 18

# 7. Environment
#~~~~~~~~~~~~~~~~

  # The last component of a function is its environment. This is not something you need to understand deeply when you first start writing functions. However, it’s important to know a little bit about environments because they are crucial to how functions work. The environment of a function controls how R finds the value associated with a name. For example, take this function:

f <- function(x) {
  x + y
} 

  # In many programming languages, this would be an error, because y is not defined inside the function. In R, this is valid code because R uses rules called **lexical scoping** to find the value associated with a name. Since y is not defined inside the function, R will look in the **environment** where the function was defined:

y <- 100
f(10) #> [1] 110

y <- 1000
f(10) #> [1] 1010

  # This behaviour seems like a recipe for bugs, and indeed you should avoid creating functions like this deliberately, but by and large it doesn’t cause too many problems (especially if you regularly restart R to get to a clean slate).

  # The advantage of this behaviour is that from a language standpoint it allows R to be very consistent. Every name is looked up using the same set of rules. For f() that includes the behaviour of two things that you might not expect: { and +. This allows you to do devious things like:

`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
table(replicate(1000, 1 + 2))
#> 
#>   3 3.3 
#> 100 900
rm(`+`)

  # This is a common phenomenon in R. R places few limits on your power. You can do many things that you can’t do in other programming languages. You can do many things that 99% of the time are extremely ill-advised (like overriding how addition works!). But this power and flexibility is what makes tools like ggplot2 and dplyr possible. Learning how to make best use of this flexibility is beyond the scope of this book, but you can read about in [Advanced R.](http://adv-r.had.co.nz/)


