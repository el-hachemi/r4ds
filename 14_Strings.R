
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#
#                                    STRINGS                                #
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===# 


# 1. Introduction
#~~~~~~~~~~~~~~~~~~

  # This chapter will focus on the stringr package for string manipulation, which is part of the core tidyverse.
  library(tidyverse)


# 2. String basics
#~~~~~~~~~~~~~~~~~~~
  
  # You can create strings with either single quotes or double quotes.
string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

  # To include a literal single or double quote in a string you can use \ to “escape” it:
double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

  # Beware that the printed representation of a string is not the same as string itself, because the printed representation shows the escapes. To see the raw contents of the string, use writeLines():
x <- c("\"", "\\")
x  
writeLines(x)  

  # There are a handful of other special characters. The most common are "\n", newline, and "\t", tab, but you can see the complete list by requesting help on ": ?'"', or ?"'". You’ll also sometimes see strings like "\u00b5", this is a way of writing non-English characters that works on all platforms:  
x <- "\u00b5"
x
  
  # Multiple strings are often stored in a character vector, which you can create with c():
c("one", "two", "three")

  # 2.1 String length
  #~~~~~~~~~~~~~~~~~~
str_length(c("a", "R for data science", NA))

  # 2.2 Combining strings
  #~~~~~~~~~~~~~~~~~~~~~~~
    
    # To combine two or more strings
str_c("x","y")
str_c("x", "y", "z")
str_c("x", "y", sep = ", ") # to control how they’re separated

    # If you want them to print as "NA"
x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|") 

    # automatically recycles shorter vectors to the same length as the longest
str_c("prefix-", c("a", "b", "c"), "-suffix")

    # Objects of length 0 are silently dropped. particularly useful in conjunction with if
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

    # To collapse a vector of strings into a single string
str_c(c("x", "y", "z"), collapse = ", ")

  # 2.3 Subsetting strings
  #~~~~~~~~~~~~~~~~~~~~~~~~

    # str_sub() takes start and end arguments
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)

    # str_sub() won’t fail if the string is too short
str_sub("a", 1, 5)

    # use the assignment form of str_sub() to modify strings
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

  # 2.4 Locales
  #~~~~~~~~~~~~
    
    # pick which set of rules to use by specifying a locale
str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")

    # The locale is specified as a ISO 639 language code, which is a two or three letter abbreviation. https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes 
x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")  # English
str_sort(x, locale = "haw") # Hawaiian

  # 2.5 Exercises
  #~~~~~~~~~~~~~~

    # > 1. In code that doesn’t use stringr, you’ll often see paste() and paste0(). What’s the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of NA?

      # The function paste() separates strings by spaces by default, while paste0() does not separate strings with spaces by default. So the paste0() is the equivalent for the str_c() except for handling NA values, paste0 convert NAs to character and treat them like any ather character, But the str_c deal with NAs like they are so for that we need use str_replace_na fonction instead.

    # > 2. In your own words, describe the difference between the sep and collapse arguments to str_c().

      # sep argument separate the arguments with the sep value; collapse make the output with a lenght of one and separate them with the collapse value.

    # > 3. Use str_length() and str_sub() to extract the middle character from a string. What will you do if the string has an even number of characters?

      # The following function extracts the middle character. If the string has an even number of characters the choice is arbitrary. We choose to select $n/2$ , because that case works even if the string is only of length one. A more general method would allow the user to select either the floor or ceiling for the middle character of an even string.

bae <- "Boukara Ahmed El-Hachemi"
L <- str_length(bae)
m <- ceiling(L / 2)
str_sub(bae, m, m)

    # > 4. What does str_wrap() do? When might you want to use it?

      # The function str_wrap() wraps text so that it fits within a certain width. This is useful for wrapping long strings of text to be typeset.

    # > 5. What does str_trim() do? What’s the opposite of str_trim()?

      # The function str_trim() trims the whitespace from a string.
str_trim(" abc ")
str_trim(" abc ", side = "left")
str_trim(" abc ", side = "right")

      # The opposite of str_trim() is str_pad() which adds characters to each side.
str_pad("abc", 5, side = "both")

    # > 6. Write a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c. Think carefully about what it should do if given a vector of length 0, 1, or 2.

my_fun <- function(x) {
  if (length(x) == 0) {
    x
  } else if (length(x) == 1) {
    x
  } else if (length(x) == 2) {
    str_c(x[[1]], x[[2]], sep = " and ")
  } else if (length(x) == 3) {
    first <- str_c(x[[1]], x[[2]], sep = ", ")
    last <- str_c(" and ", x[[3]])
    str_c(first, last)
  }
}

      # from https://jrnold.github.io/r4ds-exercise-solutions/strings.html
str_commasep <- function(x, delim = ",") {
  n <- length(x)
  if (n == 0) {
    ""
  } else if (n == 1) {
    x
  } else if (n == 2) {
    # no comma before and when n == 2
    str_c(x[[1]], "and", x[[2]], sep = " ")
  } else {
    # commas after all n - 1 elements
    not_last <- str_c(x[seq_len(n - 1)], delim)
    # prepend "and" to the last element
    last <- str_c("and", x[[n]], sep = " ")
    # combine parts with spaces
    str_c(c(not_last, last), collapse = " ")
  }
}


# 3. Matching patterns with regular expressions 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # To learn regular expressions, we’ll use str_view() and str_view_all().

  # 3.1 Basic matches
  #~~~~~~~~~~~~~~~~~~~~

x <- c("apple", "banana", "pear")
str_view(x, "an") # The simplest patterns match exact strings
str_view(x, ".a.") # `.` matches any character (except a newline)

dot <- "\\." # To create the regular expression, we need \\
writeLines(dot) # but the expression itself only contain one 
str_view(c("abc", "a.c", "bef"), "a\\.c") # Tell R to look for an explicit `.`

    # If \ is used as an escape character in regular expressions, how do you match a literal \? Well you need to escape it, creating the regular expression \\. To create that regular expression, you need to use a string, which also needs to escape \. That means to match a literal \ you need to write "\\\\" — you need four backslashes to match one!

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")

    # 3.1.1 Exercices

      # > 1. Explain why each of these strings don't match a \ : "\", "\\", "\\\" .
        
        # "\" : This escape the next character in the R string.
        # "\\": This will resolve to \ in the regular expression, which will escape the next character in the regular expression.
        # "\\\": The first two backslashes will resolve to a literal backslash in the regular expression, the third will escape the next character. So in the regular expression, this will escape some escaped character.


      # > 2. How would you match the sequence "'\ ?
str <- "\"'\\"
writeLines(str)
str_view(str, "\\\"\\'\\\\")



      # > 3. What patterns will the regular expression \..\..\.. match? How would you represent it as a string?
        
        # It will match any patterns that are a dot followed by any character, repeated three times.
str_view(c(".a.b.c", ".a.b", "....."), c("\\..\\..\\.."), match = TRUE)


  # 3.2 Anchors
  #~~~~~~~~~~~~~

    # * `^` to match the start of the string.
    # * `$` to match the end of the string.

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$") # force a reg expr to only match a complete string

    # You can also match the boundary between words with \b. For example, I’ll search for \bsum\b to avoid matching summarise, summary, rowsum and so on.

    # 3.2.1 Exercises
    #~~~~~~~~~~~~~~~~~~

      # > 1. How would match the literal string "$^$" ?
str_view("$^$", "$\\^$")

      # > 2. Given the corpus of common words in stringr::words, create regular expressions that find all words that:

        # Start with “y”.
str_view(words, "^y", match = TRUE)
        # Ends with "x"
str_view(words, "x$", match = TRUE)
        # Are exactly tree letters long. (Don't cheat by using str_length()!)
str_view(words, "^...$", match = TRUE)
        # Have seven letters or more.
str_view(words, "^.......", match = TRUE)

  # 3.3 Character classes and alternatives
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Special patterns that match more than one character:
      # . :  matches any character apart from newline.
      # \d : matches any digit.
      # \s : matches any whitespaces (e.g. space, tab, newline).
      # [abc]: matches a, b or c.
      # [^abc]: matches aything except a,b or c.

    # A character class containing a single character is a nice alternative to backslash escapes
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")
    # This works for most (but not all) regex metacharacters: $ . | ? * + ( ) [ {. Unfortunately, a few characters have special meaning even inside a character class and must be handled with backslash escapes: ] \ ^ and -.

    # You can use alternation to pick between one or more alternative patterns.  if precedence ever gets confusing, use parentheses to make it clear what you want:
str_view(c("grey", "gray"), "gr(e|a)y")

    # 3.3.1 Exercises 
    #~~~~~~~~~~~~~~~~~~

      # > 1. Create regular expressions to find all words that:
        # Start with vowel.
str_view(words, "^(a|e|i|o|u)", match = TRUE) # my regexp
str_subset(words, "^[aeiou]") # from https://jrnold.github.io/r4ds-exercise-solutions/strings.html#matching-patterns-with-regular-expressions

        # That only contain consonants. (Hint: thinking about matching “not”-vowels.)
str_view(words, "[^aeiou]", match = TRUE) # need to be improved 
        # End with ed, but not with eed.
str_view(words, "[^e]ed$", match = TRUE)
        # End with ing or ise.
str_view(words, "(ing|ise)$", match = TRUE)

      # > 2. Emprically verify the rule "i before e expect after c".
str_view(words, "cie|[^c]ei", match = TRUE)
        #  It seemes there is 3 exeptions science, society and weigh

      # > 3. Is “q” always followed by a “u”?
str_view(words, "q[^u]", match = TRUE)
        # In the words dataset yes.

      # > 4. Write a regular expression that matches a word if it's probably written in British English, not American English
        # In the general case, this is hard, and could require a dictionary. But, there are a few heuristics to consider that would account for some common cases: British English tends to use the following:
          # * “ou” instead of “o”
          # * use of “ae” and “oe” instead of “a” and “o”
          # * ends in ise instead of ize
          # * ends in yse
        # The regex ou|ise$|ae|oe|yse$ would match these.
        # There are other spelling differences between American and British English https://en.wikipedia.org/wiki/American_and_British_English_spelling_differences but they are not patterns amenable to regular expressions. It would require a dictionary with differences in spellings for different words.

      # > 5. Create a regular expression that will match telephone numbers as commonly written in your country.
        # the common written form of the telephone numbers in Algeria is "\d\d\d\d\s\d\d\s\d\d\s\d\d"
x <- c("123-456-7890", "1235-2351", "0123 45 67 89", "012 34 56 78")
str_view(x, "\\d\\d\\d\\d\\s\\d\\d\\s\\d\\d\\s\\d\\d")
str_view(x, "\\d{4}\\s\\d{2}\\s\\d{2}\\s\\d{2}")
str_view(x, "\\d{4} \\d{2} \\d{2} \\d{2}|\\d{3} \\d{2} \\d{2} \\d{2}")
        # this Stack Overflow question https://stackoverflow.com/questions/123559/a-comprehensive-regex-for-phone-number-validation for a discussion of using a regex for phone number validation.

  # 3.4 Repetition
  #~~~~~~~~~~~~~~~~

    # how many times a pattern matches:
      #  ?: 0 or 1
      #  +: 1 or more
      #  *: 0 or more

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')

    # Note that the precedence of these operators is high, so you can write: colou?r to match either American or British spellings. That means most uses will need parentheses, like bana(na)+.

    # You can also specify the number of matches precisely: 
      # {n}: exactly n
      # {n,}: n or more
      # {,m}: at most m
      # {n,m}: between n and m

str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")

    # By default these matches are “greedy”: they will match the longest string possible. You can make them “lazy”, matching the shortest string possible by putting a ? after them. 

str_view(x, 'C{2,3}?')
str_view(x, 'C[LX]+?')

    # 3.4.1 Exercices
    #~~~~~~~~~~~~~~~~~~

      # > 1. Describe the equivalents of ?, +, * in {m,n} form.
        # ? is {0,1} Match at most 1
        # + is {1, } Match 1 or more
        # * is {0, } Match 0 or more

      # > 2. Describe in words what these regular expressions match: (read carefully to see if I’m using a regular expression or a string that defines a regular expression.)
        # 1. ^.*$ This is a regular expression that matches every thing.
        # 2. "\\{.+\\}" This is a string that define a regular expression That matches a word with any character with a length of 1 or more in curly braces.
        # 3. \d{4}-\d{2}-\d{2} This is a regular expression taht matches four digits, two digits then two digits separated with hyphen, this can be date formatted "YYYY-MM-DD" ("%Y-%m-%d").
        # 4. "\\\\{4}" This is a string that define a regular expression That matches four backslashes.

      # > 3. Create regular expressions to find all words that:
        # 1. Start with three consonants.
str_view(words, "^[^aeiou]{3}", match = TRUE)
        # 2. Have three or more vowels in a row.
str_view(words, "[aeiou]{3,}", match = TRUE)
        # 3. Have two or more vowel-consonant pairs in a row.
str_view(words, "[^aeoui][aeoui]{2,}", match = TRUE)

      # > 4. Solve the beginner regexp crosswords at https://regexcrossword.com/challenges/beginner.

  # 3.5 Grouping and backreferences
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Parantheses can disanbiguate complex expressions, they also create a numbered capturing group. A capturing group stores  the part of the string matched by the part of the regular expression inside the parentheses. You can refer to the same text as previously matched by a capturing group with backreferences, like \1, \2 etc.

str_view(fruit, "(..)\\1", match = TRUE)

    # 3.5.1 Exercises
    #~~~~~~~~~~~~~~~~~~

      # > 1. Describe, in words, what these expressions will match:
        # 1. (.)\1\1 any character repeated three times
        # 2. "(.)(.)\\2\\1" any character fellowed by any character twice then the first character
        # 3. (..)\1 any two character repeated
        # 4. "(.).\\1.\\1" any charcter fellowed by any other character then the first character then any other character then the first one.
        # 5. "(.)(.)(.).*\\3\\2\\1" any character then any character the any other character then may be any character or more then the third character then the second then the first.

      # > 2. Construct regular expressions to match words that:
        # 1. Start and end with the same character.
str_view_all(words, "(.).+\\1")
        # 2. Contain a repeated pair of letters (e.g. “church” contains “ch” repeated twice.)
str_view(words, "(..).+\\1")
        # 3. Contain one letter repeated in at least three places (e.g. “eleven” contains three “e”s.)
str_view(words, "(.).*\\1.*\\1.*")


# 4. Tools
#~~~~~~~~~~

  # Now that you’ve learned the basics, it’s time to learn how to apply them to real problems. There's a wide array of stringr functions that let you:
    # * Determine which strings match a pattern
    # * Find the positions of matches
    # * Extract the content of matches
    # * Replace matches with new values
    # * Split a string based on a match

  # Caution 
    # > Some people, when confronted with a problem, think “I know, I’ll use regular expressions.” Now they have two problems. http://stackoverflow.com/a/201378 This link lead to an example. So Instead of creating one complex regular expression, it’s often easier to write a series of simpler regexps.

  # 4.1 Detect matches
  #~~~~~~~~~~~~~~~~~~~~

    # To determine if a character vector matches a pattern
x <- c("apple", "banana", "pear")
str_detect(x, "e")

    # when you use a logical vector in a numeric context, FALSE becomes 0 and TRUE becomes 1. That makes sum() and mean() useful
sum(str_detect(words, "^t")) # How many common words start with t?
mean(str_detect(words, "[aeiou]$")) # proportion of words ends with a vowel?

    # When you have complex logical conditions (e.g. match a or b but not c unless d) it’s often easier to combine multiple str_detect() calls with logical operators, rather than trying to create a single regular expression.

      # Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
      # Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeoui]+$")
identical(no_vowels_1, no_vowels_2)

    # The results are identical, but I think the first approach is significantly easier to understand. If your regular expression gets overly complicated, try breaking it up into smaller pieces, giving each piece a name, and then combining the pieces with logical operations.

    # To select the elements that match a pattern.
words[str_detect(words, "x$")] # logical subsetting
str_subset(words, "x$") # str_subset() wrapper

    # Typically, however, your strings will be one column of a data frame, and you’ll want to use filter instead:
df <- tibble(
  word = words,
  i = seq_along(word)
)
df %>% 
  filter(str_detect(word, "x$"))

    # str_count() tells you how many matches there are in a string
x <- c("apple", "banana", "pear")
str_count(x, "a")
mean(str_count(words, "[aeiou]")) # On average, how many vowels per word?

    # It's natural to use str_count() with mutate()
df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

    # Note that matches never overlap.
str_count("abababa", "aba")
str_view_all("abababa", "aba") 
    # many stringr functions come in pairs: one function works with a single match, and the other works with all matches.

      # 4.1.1 Exercises
      #~~~~~~~~~~~~~~~~~~

        # > 1. For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str_detect() calls.
          # 1. Find all words that start or end with x.
str_subset(words, "^x|x$")
start_with_x <- str_detect(words, "^x")
end_with_x <- str_detect(words, "x$")
words[start_with_x | end_with_x]
          # 2. Find all words that start with a vowel and end with a consonant.
str_subset(words, "^[aeiou].*[^aeiou]$")
start_with_vowel <- str_detect(words, "^[aeiou]")
end_with_consonant <- str_detect(words, "[^aeiou]$")
words[start_with_vowel & end_with_consonant]
          # 3. Are there any words that contain at least one of each different vowel?
            # I've can't figure out how to write a regexp for that problem!
matching_word <- c("aseiouds", "kjklhf")
words[
  str_detect(words, "a") &
  str_detect(words, "e") &
  str_detect(words, "i") &
  str_detect(words, "o") &
  str_detect(words, "u") 
]
matching_word[
  str_detect(matching_word, "a") &
  str_detect(matching_word, "e") &
  str_detect(matching_word, "i") &
  str_detect(matching_word, "o") &
  str_detect(matching_word, "u") 
  ]

        # > 2. What word has the highest number of vowels? What word has the highest proportion of vowels? (Hint: what is the denominator?)
df <- tibble(
  word = words,
  i = seq_along(word),
  n_vowels = str_count(word, "[aeiou]"),
  n_consonant = str_count(word, "[^aeiou]")
)
            # word with the highest number of vowels
max_vowels <- df %>% filter(n_vowels == max(n_vowels)) # My solution
vowels <- str_count(words, "[aeiou]") # the solution from the jrnolds
words[which(vowels == max(vowels))] # https://jrnold.github.io/r4ds-exercise-solutions/strings.html#tools

            # word with the highest proportion of vowels
max_p_vowels <- df %>% 
  mutate(p_vowels = n_vowels / str_length(word)) %>%
  filter(str_length(word) > 1) %>% 
  filter(p_vowels == max(p_vowels)) # My solution
prop_vowels <- str_count(words, "[aeiou]") / str_length(words)#from the jrnolds
words[which(prop_vowels == max(prop_vowels))] # https://jrnold.github.io/r4ds-exercise-solutions/strings.html#tools


  # 4.2 Extract matches
  #~~~~~~~~~~~~~~~~~~~~~~

    # To extract the actual text of a match, use str_extract()

    # I’m going to use the Harvard sentences provided in stringr::sentences
length(sentences)
    # https://en.wikipedia.org/wiki/Harvard_sentences
head(sentences)

    # Imagine we want to find all sentences that contain a colour.

    # We first create a vector of colour names,
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
    # and then turn it into a single regular expression
colour_match <- str_c(colours, collapse = "|")  
colour_match  

    # Now we can select the sentences that contain a colour, 
has_colour <- str_subset(sentences, colour_match)
    # and then extract the colour to figure out which one it is:
matches <- str_extract(has_colour, colour_match)
head(matches)    

    # Note that str_extract() only extracts the first match.  We can see that most easily by first selecting all the sentences that have more than 1 match:
more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)
str_extract(more, colour_match)

    # To get all matches, use str_extract_all(). It returns a list:
str_extract_all(more, colour_match)

    # If you use simplify = TRUE, str_extract_all() will return a matrix with short matches expanded to the same length as the longest:
str_extract_all(more, colour_match, simplify = TRUE)
x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

    # 4.2.1 Exercises
    #~~~~~~~~~~~~~~~~~~

      # > 1. In the previous example, you might have noticed that the regular expression matched “flickered”, which is not a colour. Modify the regex to fix the problem.
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")   

colour_match2 <- str_c("\\b(", str_c(colours, collapse = "|"),")\\b")
colour_match2 # After adding boundaries in both sides

more <- sentences[str_count(sentences, colour_match) > 1]
# If we made more2 object with the colour_match2 we will not get the wanted sentences with the word flickred, so I choose to view the matched sentences with the first one.
str_view_all(more, colour_match2, match = TRUE)

      # > 2. From the Harvard sentences data, extract:
        # 1. The first word from each sentence.
          # consider a word any contiguous set of letters. Since str_extract() will extract the first match, if it is provided a regular expression for words, it will return the first word.
str_extract(sentences, "[A-ZAa-z]+") %>% head()
        # 2. All words ending in ing.
str_extract_all(sentences, "\\b[A-Za-z]+ing\\b") %>% unlist()
        # 3. All plurals.
# Finding plural words would at least require morphological information about words in the language. See WordNet https://cran.r-project.org/web/packages/wordnet/index.html for a resource that would do that. However, identifying words that end in an “s” and with more than three characters, in order to remove “as”, “is”, “gas”, etc., is a reasonable heuristic.
str_extract_all(sentences, "\\b[A-Za-z]{3,}s\\b", simplify = TRUE)

  # 4.3 Grouped matches
  #~~~~~~~~~~~~~~~~~~~~~~

    # Imagine we want to extract nouns from the sentences. As a heuristic, we’ll look for any word that comes after “a” or “the”. Defining a “word” in a regular expression is a little tricky, so here I use a simple approximation: a sequence of at least one character that isn’t a space.
noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>% 
  str_subset(noun) %>% 
  head(10)
has_noun %>% 
  str_extract(noun) # str_extract() gives us the complete match

    # str_match() gives each individual component. Instead of a character vector, it returns a matrix, with one column for the complete match followed by one column for each group
has_noun %>% 
  str_match(noun) 

    # If your data is in a tibble, it’s often easier to use tidyr::extract(). It works like str_match() but requires you to name the matches, which are then placed in new columns:
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )

    # 4.3.1 Exercises
      # > 1. Find all words that come after a "number" like "one", "two", "three" etc. Pull out both the number and the word.
num_word <- str_c(c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"), collapse = "|")
num_pattern <- str_c("\\b(", num_word, ") +(\\w+)")
tibble(sentences) %>%  
  extract(
    sentences, c("number", "word"), num_pattern,
    remove = FALSE
  ) %>% 
  filter(!is.na(number)) # With the tibble method

num_word <- str_c(c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"), collapse = "|")
num_pattern <- str_c("\\b(", num_word, ") +(\\w+)")
has_numbers <- sentences %>% 
  str_subset(num_pattern) # subsetting only the matched sentences
has_numbers %>% 
  str_extract(num_pattern) # with str_extract()
has_numbers %>% 
  str_match(num_pattern) # with str_match

      # > 2. Find all contractions. Separate out the pieces before and after the apostrophe.
contractions <- "([A-Za-z]+)'([A-Za-z]+)"
has_contractions <- sentences %>% 
  str_subset(contractions)
has_contractions %>% 
  str_extract(contractions) # with str_subset
sentences[str_detect(sentences, contractions)] %>% 
  str_extract(contractions) # extracting after subsetting using the str_detect
has_contractions %>% 
  str_match(contractions) # Using the str_match method

  # 4.4 Replacing matches
  #~~~~~~~~~~~~~~~~~~~~~~~~

    # The simplest use is to replace a pattern with a fixed string
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")

    # str_replace_all() can perform multiple replacements by supplying a named vector
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

    # Instead of replacing with a fixed string you can use backreferences to insert components of the match.
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5) #  I flip the order of the second and third words.

    # 4.4.1 Exercices
    #~~~~~~~~~~~~~~~~~~

      # > 1. Replace all forward slashes in a string with backslashes.
piece_of_code <- '+ str_replace(\"([^ ]+) ([^ ]+) ([^ ]+)\", \"/1 /3 /2\")'
str_replace_all(piece_of_code, c("/" = "\\\\")) # named vector
str_replace_all(piece_of_code, "/", "\\\\") # replace pattern with fixed string

      # > 2. Implement a simple version of str_to_lower() using replace_all().
replacements <- c(
  "A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e",
  "F" = "f", "G" = "g", "H" = "h", "I" = "i", "J" = "j",
  "K" = "k", "L" = "l", "M" = "m", "N" = "n", "O" = "o",
  "P" = "p", "Q" = "q", "R" = "r", "S" = "s", "T" = "t",
  "U" = "u", "V" = "v", "W" = "w", "X" = "x", "Y" = "y",
  "Z" = "z"
)
str_replace_all(sentences, pattern = replacements)

      # > 3. Switch the first and last letters in words. Which of those strings are still words?
str_replace_all(words, "^([A-Za-z])(.*)([A-Za-z])$", "\\3\\2\\1")

  # 4.5 Splitting
  #~~~~~~~~~~~~~~~~

    # Split sentences into words
sentences %>% 
  head(5) %>% 
  str_split(" ") # returns a list.

    # If you’re working with a length-1 vector, the easiest thing is to just extract the first element of the list
"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]

    # Otherwise, use simplify = TRUE to return a matrix
sentences %>% 
  head(5) %>% 
  str_split(" ", simplify = TRUE)

    # You can also request a maximum number of pieces
fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)

    # Split up by character, line, sentence and word boundary()s
x <- "This is a sentence.  This is another sentence."
str_split(x, " ")[[1]]
str_split(x, boundary("word"))[[1]]

    # 4.5.1 Exercises
    #~~~~~~~~~~~~~~~~~~

    # > 1. Split up a string like "apples, pears, and bananas" into individual components.
x <- c("apples, pears, and bananas")
str_split(x, boundary("word"))[[1]] # Using boundary function
str_split(x, ", +(and +)?")[[1]] # Using regexp pattern

    # > 2. Why is it better to split up by boundary("word") than " "?
      # Splitting by boundary("word") is a more sophisticated method to split a string into words. It recognizes non-space punctuation that splits words, and also removes punctuation while retaining internal non-letter characters that are parts of the word, e.g., “can’t” See the ICU website (http://userguide.icu-project.org/boundaryanalysis) for a description of the set of rules that are used to determine word boundaries.
      # Consider this sentence from the official Unicode Report on word boundaries (http://www.unicode.org/reports/tr29/#Word_Boundaries),
sentence <- "The quick (“brown”) fox can’t jump 32.3 feet, right?"
str_split(sentence, " ") # group the punctuation with the words
str_split(sentence, boundary("word")) # correctly removes punctuation, while not separating “32.2” and “can’t”

    # > 3. What does splitting with an empty string ("") do? Experiment, and then read the documentation.
str_split(sentence, "")
?str_split
      # An empty pattern, "", is equivalent to boundary("character").

  # 4.6 Find matches
  #~~~~~~~~~~~~~~~~~~

    #str_locate() and str_locate_all() give you the starting and ending positions of each match. These are particularly useful when none of the other functions does exactly what you want. You can use str_locate() to find the matching pattern, str_sub() to extract and/or modify them.


# 5. Other types of pattern
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # When you use a pattern that’s a string, it’s automatically wrapped into a call to regex()
str_view(fruit, "nana") # The regular call
str_view(fruit, regex("nana")) # Is shorthand for

  # You can use the other arguments of regex() to control details of the match

    # ignore_case = TRUE allows characters to match either their uppercase or lowercase forms. This always uses the current locale.
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE)) 

    # multiline = TRUE allows ^ and $ to match the start and end of each line rather than the start and end of the complete string.
x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

    # comments = TRUE allows you to use comments and white space to make complex regular expressions more understandable. Spaces are ignored, as is everything after #. To match a literal space, you’ll need to escape it: "\\ ".
phone <- regex("
  \\(?     # optional opening parens
  (\\d{3}) # area code
  [) -]?   # optional closing parens, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3}) # three more numbers
  ", comments = TRUE)

str_match("514-791-8141", phone)

    # dotall = TRUE allows . to match everything, including \n.

  # There are three other functions you can use instead of regex():

    # fixed(): matches exactly the specified sequence of bytes. It ignores all special regular expressions and operates at a very low level. Beware using fixed() with non-English data. It is problematic because there are often multiple ways of representing the same character.
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

    # coll(): compare strings using standard collation rules. This is useful for doing case insensitive matching. Note that coll() takes a locale parameter that controls which rules are used for comparing characters
i <- c("I", "İ", "i", "ı")
i
str_subset(i, coll("i", ignore_case = TRUE))
str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))

      # Both fixed() and regex() have ignore_case arguments, but they do not allow you to pick the locale: they always use the default locale. You can see what that is with the following code; more on stringi later.
stringi::stri_locale_info()
      # The downside of coll() is speed; because the rules for recognising which characters are the same are complicated, coll() is relatively slow compared to regex() and fixed().

    # boundary() to match boundaries. You can also use it with the other functions:
x <- "This is a sentence."
str_view_all(x, boundary("word"))
str_extract_all(x, boundary("word"))

  # 5.1 Exercises
  #~~~~~~~~~~~~~~~
  
    # > 1. How would you find all strings containing \  with regex() vs. with fixed() ?
str_subset(c("a\\b", "ab"), "\\\\")
str_subset(c("a\\b", "ab"), fixed("\\"))

    # > 2. What are the five most common words in sentences?
sentences %>% 
  str_extract_all(boundary("word")) %>% 
  unlist() %>% 
  as_tibble() %>% 
  transmute(word = str_to_lower(value)) %>% 
  count(word, sort = TRUE) %>% 
  head(5)


# 6. Other uses of regular expressions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # There are two useful function in base R that also use regular expressions

    # apropos() searches all objects available from the global environment. This is useful if you can’t quite remember the name of the function.
apropos("replace") 

    # dir() lists all the files in a directory. The pattern argument takes a regular expression and only returns file names that match the pattern. For example, you can find all the R Markdown files in the current directory with
dir(pattern = "\\.Rmd$") 
        # We can convert "globs" to regular expressions with glob2rx()

# 7. stringi
#~~~~~~~~~~~~~

  # stringr is built on top of the stringi package. It contains almost every function you might ever need: stringi has 244 functions to stringr’s 49.

  # The packages work very similarly, The main difference is the prefix: str_ vs. stri_.

  # 7.1 Exercices
  #~~~~~~~~~~~~~~~~

  # > 1. Find the stringi functions that:
    # 1. Count the number of words.
library(stringi)
stri_count_words(sentences)
    # 2. Find duplicated strings.
stri_duplicated(c(
  "the", "brown", "cow", "jumped", "over",
  "the", "lazy", "fox"
))
    # 3. Generate random text.
stri_rand_strings(4, 5) # generates random strings
stri_rand_shuffle("The brown fox jumped over the lazy cow.") #randomly shuffles the characters in the text.
stri_rand_lipsum(1) # generates lorem ipsum text. (https://en.wikipedia.org/wiki/Lorem_ipsum)

  # > 2. How do you control the language that stri_sort() uses for sorting?
    # we control the language by seting locale either by the locale argument directely or by the opts_collator = stri_ots_collator(locale = ...) argument.
stri_sort(c("hladny", "chladny"), locale = "pl_PL")
stri_sort(c("hladny", "chladny"), locale = "sk_SK")

# The stri_opts_collator() provides finer grained control over how strings are sorted. In addition to setting the locale, it has options to customize how cases, unicode, accents, and numeric values are handled when comparing strings.
stri_sort(c("hladny", "chladny"), opts_collator = stri_opts_collator(locale = "pl_PL"))
stri_sort(c("hladny", "chladny"), opts_collator = stri_opts_collator(locale = "sk_SK"))

stri_sort(c("number100", "number2"))
stri_sort(c("number100", "number2"), opts_collator = stri_opts_collator(numeric = TRUE))

