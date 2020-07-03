library(tidyverse)


ggplot(diamonds, aes(carat, price)) +
  geom_hex()
ggsave("diamonds.pdf")


write_csv(diamonds, "diamonds.csv")

##########################################################################

# Wrangle


# 10. Tibbles 


# 10.1 Introduction
# to learn more about vignette("tibble")


# 10.2 Creating tibbles 

as.tibble(iris) #From data frame


tibble(         #From individual victors 
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)


tb <- tibble(       #Non-syntactic names 
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)
tb


tribble(           #short for transposed tibble
  ~x, ~y, ~z,
  #--/--/----
  "a", 2, 3.6,
  "b", 1, 8.5
)


# 10.3 Tibbles vs. data.frame


# 10.3.1 Printing

nycflights13::flights %>%      #print 10 rows and all columns
  print(n = 10, width = Inf)

#if more than n rows, print only m
options(tibble.print_max = n, tibble.print_min = m)

#to always shows all rows
options(tibble.width = Inf)

#the complete list of options 
package?tibble


# 10.3.2 Subsetting


df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

#Extract by name
df$x  
df[["x"]]

#Extract by position
 df[[1]]

#The special placeholder `.` to use in pipe
 df %>% .$x
 df %>% .[["x"]]
 

# 10.4 Interacting with older code
 
 class(as.data.frame(tb)) #some older functions don't worl with tibbles
 
 
# 10.5 Exercices
 
# 1. How can you tell if an object is a tibble? (Hint: try printing mtcars, which is a regular data frame).
 mtcars
 class(mtcars)
 is_tibble(mtcars)
 
# 2. Compare and contrast the following operations on a data.frame and equivalent tibble. What is different? Why might the default data frame behaviours cause you frustration? 
 
 df <- data.frame(abc = 1, xyz = "a")
df$x # It shows the column it partially match it
df[, "xyz"] # shows the column as factor only the value with a level.
df[, c("abc", "wyz")]  #shows an error message "undefined columns selected"

tb <- tibble(abc = 1, xyz = "a") 
tb$x  # the output is "NULL" with a warning "Unknown or uninitialised column: `x`"
tb[, "xyz"] # shows the column with her name and her type and the value.
tb[, c("abc", "wyz")] # shows an error message with the specific name that dosen't exist.

# 3. If you have the name of a variable stored in an object, e.g. var <- "mpg", how can you extract the reference variable from a tibble?
 # by using the double bracket df[[var]] not the dollar sign df$var

# 4. Practice referring to non-syntactic names in the following data frame by:
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
  # Extracting the variable called `1`.
annoying$`1`
annoying[[1]]
annoying[["1"]] 
 
  # Plotting scatterplot of `1` vs `2`.
ggplot(annoying, aes(`1`, `2`)) +
  geom_point()

  # Creating a new column called `3` which is `2` divided by `1`.
annoying <- annoying %>% 
  mutate(`3` = `2` / `1`)

  # Renaming the columns to `one`, `two` and `three`.
annoying <- annoying %>% 
  rename("one" = `1`, "two" = `2`, "three" = `3`)

# 5. What does tibble::enframe() do? When might you use it?
  # enframe() converts named atomic vectors or lists to one- or two-column data frames. 

# 6. What option controls how many additional column names are printed at the footer of a tibble?
  #The help page for the `print()` method of tibble objects is discussed in `?print.tbl`. The `n_extra` argument determines the number of extra columns to print information for.



# 11. Data import
# === === === === ===

# 11.1 Introduction 
library(tidyverse)

# 11.2 Getting started
# === === === === === === 

class_glob <- read_csv("D:/data/Classement GLOBAL.csv")

# We can supply an inline csv file
read_csv("a,b,c
         1,2,3
         4,5,6")

# In both cases `read_csv()` uses the first line for the column name except for:
    # 1. When are a few lines of metadata at the top of the file. `skip = n` or `comment = #`
read_csv("The first line of metadata
         the second line of metadata
         x,y,z
         1,2,3", skip = 2)

read_csv("# A commen tI want to skip
         x,y,z
         1,2,3", comment = "#")

    # 2. The data don't have column names. col_names = FALSE
read_csv("1,2,3\n4,5,6", col_names = FALSE)  # "\n" add new line.

# specifie the values for `na`
read_csv("a,b,c\n1,2,.", na = ".")

# 11.2.1 Compared to base R `read.csv()`

# * much faster (~10x)
# * produce tibbles, they don’t convert character vectors to factors, use row names, or munge the column names.
# * more reproducible

# 11.2.2 Exercices
#== == == == == == == 

  # > 1. What function would you use to read a file where fields were separated with “|”?
    # read_delim(file, delim = "|")

  # > 2. Apart from file, skip, and comment, what other arguments do `read_csv()` and `read_tsv()` have in common?
    
    union(names(formals(read_csv)), names(formals(read_tsv)))
    # col_names, col_types, locale, na , quoted_na, trim_ws, n_max, guess_max, progress, skip_empty_rows
    
  # > 3. What are the most important arguments to `read_fwf()`?
    # The most important argument to `read_fwf()` which reads “fixed-width formats”, is `col_positions` which tells the function where data columns begin and end.
    
  # > 4. Sometimes strings in a CSV file contain commas. To prevent them from causing problems they need to be surrounded by a quoting character, like " or '. By default, read_csv() assumes that the quoting character will be ". What argument to read_csv() do you need to specify to read the following text into a data frame?  `"x,y\n1,'a,b'"`
    z <- "x,y\n1,'a,b'"
    read_csv(z, quote = "'")
    read_delim(z, ",", quote = "'")
  
  # > 5. Identify what is wrong with each of the following inline CSV files. What happens when you run the code?
    read_csv("a,b\n1,2,3\n4,5,6")
      # Only two columns are specified in the header “a” and “b”, but the rows have three columns, so the last column is dropped.
      # the ouput:  
                    #Warning: 2 parsing failures.
                    #row col  expected    actual         file
                    #1  -- 2 columns 3 columns literal data
                    #2  -- 2 columns 3 columns literal data
    read_csv("a,b,c\n1,2\n1,2,3,4")
      #The numbers of columns in the data do not match the number of columns in the header (three). In row one, there are only two values, so column c is set to missing. In row two, there is an extra value, and that value is dropped.
      # the ouput:
                    #Warning: 2 parsing failures.
                    #row col  expected    actual         file
                    #1  -- 3 columns 2 columns literal data
                    #2  -- 3 columns 4 columns literal data
    read_csv("a,b\n\"1")
      #It’s not clear what the intent was here. The opening quote "1 is dropped because it is not closed, and a is treated as an integer.
        #Warning: 2 parsing failures.
        #row col                     expected    actual         file
        #1  a  closing quote at end of file           literal data
        #1  -- 2 columns                    1 columns literal data
    read_csv("a,b\n1,2\na,b")
      #Both “a” and “b” are treated as character vectors since they contain non-numeric strings.
    read_csv("a;b\n1;3")
      #a anb b are treated as one variable name a;b and the values as one value 1;3.
    
    
# 11.3 Parsing a vector 
#=== === === === === === 
  
  # `parse_*()` functions take a caracter vector and return a more specialised vector like a logical, integer, or date:
    
  
    # na argument specifie which strings should be treated as missing
    parse_integer(c("1", "231", ".", "456"), na = ".") 
      
    # If parsing fails, you'll get a warning
    x <- parse_integer(c("123", "345", "abc", "123.45" ))
    
    # And failures will be missing in the output
    x

    # If there are many parsing failures, `problems()` returns a tibble, which you can manipulate with dplyr.     
    problems(x)
    
  # 11.3.1 Numbers
  # == == == == == == 
    
    # decimal_mark
    parse_double("1.23")
    parse_double("1,23", locale = locale(decimal_mark = ","))    

    # Extrat numbers
    parse_number("$100")
    parse_number("20%")
    parse_number("It cost $123.45")

    # grouping mark
    parse_number("$123,456,789") 
                        # Used in America
    parse_number("123.456.789", locale = locale(grouping_mark = "."))
                        # Used in many parts of Europe
    parse_number("123'456'789", locale = locale(grouping_mark = "'"))
                        # Used in Switzerland
    
  # 11.3.2 Strings
  # == == == == == ==   
    
    #The mapping from hexadecimal number to character is called the encoding.
    charToRaw("Ahmed") 
      # 41 68 6d 65 64

    x1 <- "El Ni\xf1o was particularly bad this year"
      #> [1] "El Ni\xf1o was particularly bad this year"
    
    x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
      #> [1] "\x82\xb1\x82\xf1\x82ɂ\xbf\x82\xcd"
    
    #To fix the problem you need to specify the encoding in `parse_character()`
    parse_character(x1, locale = locale(encoding = "Latin1"))
      #> [1] "El Niño was particularly bad this year"
    parse_character(x2, locale = locale(encoding = "Shift-JIS"))
      #> [1] "こんにちは"
    
    #the correct encoding will be included somewhere in the data documentation. If not use `guess_encoding()`
    
    guess_encoding(charToRaw(x1))
    
    guess_encoding(charToRaw(x2))
    
    
  # 11.3.3 Factors
  # == == == == == 
    
    # Give `parse_factor()` a vector of known levels to generate a warning whenever an unexpected value is present:
    
    fruit <- c("apple", "banana")
    parse_factor(c("apple", "banana", "bananana"), levels = fruit)
       

  
  # 11.3.4 Dates, date-times, and times
  # == == == == == == == == == == == == 

    # * `parse_datetime()` expects an ISO8601 date-time. 
         
      parse_datetime("2010-10-01T2010")
      parse_datetime("20101010")
            
    # * `parse_date()` expects a four digit with a `-` or `/` separator
            
      parse_date("2010-10-01")

    # * `parse_time()` expects the hour, `:`, minutes, optionally `:` and seconds, and an optional am/pm specifier:  
        
      library(hms) #Base R doesn’t have a great built in class for time data
      parse_time("01:10 am")
      parse_time("20:10:01")

    # If these default don't work, you can supply your own format:
          
      # Year
            # %Y (4 digit)
            # %y (2 digit)
        
      # Month
            # %m (2 digits).
            # %b (abbreviated name, like “Jan”).
            # %B (full name, “January”).

      # Day
            # %d (2 digits).
            # %e (optional leading space).
        
      # Time
            # %H 0-23 hour.
            # %I 0-12, must be used with %p.
            # %p AM/PM indicator.
            # %M minutes.
            # %S integer seconds.
            # %OS real seconds.
            # %Z Time zone (as name, e.g. America/Chicago).
            # %z (as offset from UTC, e.g. +0800)
        
      # Non-digits
            # %. skips one non-digit character.
            # %* skips any number of non-digits.
        
       # If you’re using %b or %B with non-English month names, you’ll need to set the lang argument to locale().
        
  # 11.3.5 Exercises
  #== == == == == ==     
      
    # > 1. What are the most important arguments to `locale()`?
      # The locale object has arguments to set the following:
          # *date and time formats: date_names, date_format, and time_format
          # *time zone: tz
          # *numbers: decimal_mark, grouping_mark
          # *encoding: encoding
      
    # > 2. What happens if you try and set decimal_mark and grouping_mark to the same character? What happens to the default value of grouping_mark when you set decimal_mark to “,”? What happens to the default value of decimal_mark when you set the grouping_mark to “.”?  
      
      parse_number("123,456.789", locale = locale(decimal_mark = ".", grouping_mark = "."))
      # We got an error message: Erreur : `decimal_mark` and `grouping_mark` must be different
    locale(decimal_mark = ",")
      # If the decimal_mark is set to the comma ",", then the grouping mark is set to the period "."
    locale(grouping_mark = ".")
    # If the rouping_mark is set to the period ".", then the decimal_mark is set to the period ","
    
    # > 3. I didn’t discuss the date_format and time_format options to locale(). What do they do? Construct an example that shows when they might be useful.
    
    #Examples from the readr vignette of parsing French dates
    parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
    parse_date("14 oct. 1979", "%d %b %Y", locale = locale("fr"))
    # Apparently the time format is not used for anything, but the date format is used for guessing column types.
    
    # > 4. If you live outside the US, create a new locale object that encapsulates the settings for the types of file you read most commonly.
    my_locale <-  locale(date_names = "fr", date_format = "%d/%m/%Y", decimal_mark = ",", grouping_mark = " ")

    # > 5. What’s the difference between `read_csv()` and `read_csv2()`?
      # read_csv2() uses ; for the field separator and , for decimal_mark
    
    # > 6. What are the most common encodings used in Europe? What are the most common encodings used in Asia? Do some googling to find out.
      # Windows 1252 and 7 bit ASCII were the most widely used encoding schemes until 2008 when UTF-8 Became the most common. uses one byte for the first 128 code points, and up to 4 bytes for other characters.
      # The next-most popular multi-byte encodings are the Chinese GB 2312 and GBK  with a combined 0.7% share, and the Japanese Shift JIS with 0.2%.
      # The list in the documentation for stringi::stri_enc_detect() is a good list of encodings since it supports the most common encodings.
    
        # *Western European Latin script languages: ISO-8859-1, Windows-1250 (also CP-1250 for code-point)
        # *Eastern European Latin script languages: ISO-8859-2, Windows-1252
        # *Greek: ISO-8859-7
        # *Turkish: ISO-8859-9, Windows-1254
        # *Hebrew: ISO-8859-8, IBM424, Windows 1255
        # *Russian: Windows 1251
        # *Japanese: Shift JIS, ISO-2022-JP, EUC-JP
        # *Korean: ISO-2022-KR, EUC-KR
        # *Chinese: GB18030, ISO-2022-CN (Simplified), Big5 (Traditional)
        # *Arabic: ISO-8859-6, IBM420, Windows 1256
    
      # “What Every Programmer Absolutely, Positively Needs To Know About Encodings And Character Sets To Work With Text”, http://kunststube.net/encoding/
      # Programs that identify the encoding of text include:
        # readr::guess_encoding()
        # stringi::str_enc_detect()
        # iconv (https://en.wikipedia.org/wiki/Iconv)
        # chardet (Python) (https://github.com/chardet/chardet)
    
    
    # > 7. Generate the correct format string to parse each of the following dates and times:
    
    d1 <- "January 1, 2010"
    parse_date(d1, "%B %d, %Y")
    
    d2 <- "2015-Mar-07"    
    parse_date(d2, "%Y-%b-%d")      

    d3 <- "06-Jun-2017"    
    parse_date(d3, "%d-%b-%Y") 
    
    d4 <- c("August 19 (2015)", "July 1 (2015)")    
    parse_date(d4, "%B %d (%Y)")  
    
    d5 <- "12/30/14" # Dec 30, 2014
    parse_date(d5, "%m/%d/%y")

    t1 <- "1705"
    parse_time(t1, "%H%M")    

    t2 <- "11:15:10.12 PM"    
    parse_time(t2, "%H:%M:%OS %p")

    
# 11.4 Parsing a file
# === === === === ===   
    
  # 11.4.1 Strategy
  #== == == == == ==
    
    # readr reads the first 1000 rows and uses some heuristics to figure out the type of each column. We can emulate this process using guess_parser(), which returns readr’s best guess, and parse_guess() which uses that guess to parse the column:
    
    guess_parser("2010-10-01")
    str(parse_guess("2010-10-01"))
    
  # 11.4.2 Problems
  #== == == == == ==  
    
    # There are two basic problems:
    # 1. The first thousand rows might be a special case, and readr guesses a type that is not sufficiently general.
    # 2. The column might contain a lot of missing values.
    
    challenge <- read_csv(readr_example("challenge.csv"))
    
    # It’s always a good idea to explicitly pull out the problems()
    # we can see that there are a lot of parsing problems with the y column
    problems(challenge)
    
    
    # A good strategy is to work column by column
    # In the last few rows we see that they’re dates stored in a character vector.
    tail(challenge)
    
    # That suggests we need to use a date parser instead.
    # To fix the call, start by copying and pasting the column specification into your original call:
    # Then fix the type of the y column
    challenge <- read_csv(
      readr_example("challenge.csv"),
      col_types = cols(
        x = col_double(),
        y = col_date()
      )
    )
    
    # If you want to be really strict, use stop_for_problems(): that will throw an error and stop your script if there are any parsing problems.
    
    
  #11.4.3 Other strategies
  #== == == == == == == ==
    
    # * If we look at more rows may be wwe can parse it in one shot
    challenge <- read_csv(readr_example("challenge.csv"), guess_max = 1001)

    # * Sometimes it’s easier to diagnose problems if you just read in all the columns as character vectors
    challenge2 <- read_csv(readr_example("challenge.csv"),
      col_types = cols(.default = col_character())
    )
    
      # This is particularly useful in conjunction with type_convert()
    type_convert(challenge2)
    
    # * If you’re reading a very large file, you might want to set n_max to a smallish number like 10,000 or 100,000. 
    
    # * If you’re having major parsing problems, sometimes it’s easier to just read into a character vector of lines with `read_lines()`, or even a character vector of length 1 with read_file(). Then you can use the string parsing skills you’ll learn later to parse more exotic formats.
    
    
  #11.5 Writing to a file
  #=== === === === === ===
    
  # `write_csv()` and `write_tsv()` increasethe chances of the output file being read back correctly by:
    
    # * Always encoding strings in UTF-8.
    # * Saving dates and date-times in ISO8601 format.
    
  # `write_excel_csv()`  
    
    write_csv(challenge, "challenge-2.csv")
    # Note that the type information is lost when you save to csv:
    read_csv("challenge-2.csv")
    # This makes CSVs a little unreliable for caching interim results
    # There are two alternatives:
    
      # 1. write_rds() and read_rds() are uniform wrappers around the base functions readRDS() and saveRDS(). R’s custom binary format called RDS:
      write_rds(challenge, "challenge.rds")
      read_rds("challenge.rds")    

      # 2. The feather package implements a fast binary file format that can be shared across programming languages:
      library(feather)
      write_feather(challenge, "challenge.feather")    
      read_feather("challenge.feather")    
        #Feather tends to be faster than RDS and is usable outside of R. 
        #RDS supports list-columns; feather currently does not.
        
        
  # Other types of data
  #=== === === === === ===
        
    # To get other types of data into R, recommended packages
      
      # **haven** reads SPSS, Stata and SAS files
      # **readxl** reads excel files
      # **DBI** along with a databases specefic packend (RMySQL, RSQLite, RPostgreSQL etc)
      
    # jsonlite for json, xml2 for xml (https://jennybc.github.io/purrr-tutorial/).
    # For other file types, try the R data import/export manual (https://cran.r-project.org/doc/manuals/r-release/R-data.html)
    # and the rio package (https://github.com/leeper/rio)
        
      
      
      
      
      
      
      
      