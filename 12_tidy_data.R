
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#
#                                 TIDY DATA                                 #
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#       

# 1. Introduction
#~~~~~~~~~~~~~~~~~

  # > “Tidy datasets are all alike, but every messy dataset is messy in its own way.” –– Hadley Wickham

  #  To learn more about the underlying theory, you might enjoy the Tidy Data paper published in the Journal of Statistical Software, http://www.jstatsoft.org/v59/i10/paper.

  library(tidyverse)
  

# 2. Tidy data
#~~~~~~~~~~~~~~
  
  # The same data organised in four different ways.
  table1
  table2
  table3
  table4a
  table4b

  # Three (interrelated) rules make a dataset tidy:
    # 1. Each variable must have its own column.
    # 2. Each observation must have its own row.
    # 3. Each value must have its own cell.
  
  # That interrelationship leads a set of practical instructions:
    # 1. Put each dataset in a tibble.
    # 2. Put each variable in a column.
  
  # Why ensure that your data is tidy?
    # 1. The general advantage, learning the tools become easier.
    # 2. The specific advantage, make the transforming feel natural.

  
  # Example
  # compute rate per 10,000
  table1 %>% 
    mutate(rate = cases / population * 10000)
  
  # Compute cases per year
  table1 %>% 
    count(year, wt = cases)
  
  # Visualise changes over time 
  ggplot(table1, aes(year, cases)) +
    geom_line(aes(group = country), color = "grey50") +
    geom_point(aes(color = country))
  
# 2.1 Exercises
#~~~~~~~~~~~~~~~~
  # > 1. Using prose, describe how the variables and observations are organised in each of the sample tables.
  
  # > 2. Compute the `rate` for `table2`, and `table4a` + `table4b.` You will need to perform four operations:
      # 1. Extract the number of TB cases per country per year.
      # 2. Extract the matching population per country per year.
      # 3. Divide cases by population, and multiply by 10000.
      # 4. Store back in the appropriate place.
    # Which representation is easiest to work with? Which is hardest? Why?
  
  cases <- table2 %>% 
    filter(type == "cases") %>% 
    mutate(cases = count) %>% 
    select(country, year, cases)
  
  population <- table2 %>% 
    filter(type == "population") %>% 
    mutate(population = count) %>% 
    select(population)
  
  rate_table2 <- tibble(
    cases,
    population,
    rate = cases / population * 10000
  )
  
  
  table_4 <- tibble(
    country = c(table4a$country, table4b$country),
    year = rep(1999:2000, each = 3),
    cases = c(table4a$`1999`, table4a$`2000`),
    population = c(table4b$`1999`, table4b$`2000`)
  ) %>% 
    arrange(country) %>% 
    mutate(rate = cases / population * 10000)
  
  
  # > 3. Recreate the plot showing change in cases over time using table2 instead of `table1`. What do you need to do first?
  
  # I had to filter the dataeset to only include rws represation cases.
  
  table2 %>% 
    filter(type == "cases") %>% 
    ggplot(aes(year, count)) +
    geom_line(aes(group = country), color = "grey50") +
    geom_point(aes(color = country)) +
    scale_x_continuous(breaks = unique(table2$year)) +
    ylab("cases")
  
  
# 3. Pivoting
#~~~~~~~~~~~~~
  
  # The first step is always to figure out what the variables and observations are. The second step is to resolve one of two common problems:
    # 1. One variable might be spread across multiple columns.
    # 2. One observation might be scattered across multiple rows.
  
  # To fix these problems, you’ll need `pivot_longer()` and `pivot_wider()`
  
  # 3.1 Longer
  #~~~~~~~~~~~~
  
    # A common problem is a dataset where some of the column names are not names of variables, but values of a variable. common problem is a dataset where some of the column names are not names of variables, but values of a variable.
  
    table4a
    
    # To tidy it we need to **pivot** the offending columns into a new pair of variables. We need three parameters:
      # * The set of columns whose name are values (1999 and 2000)
      # * The name of the variable to move the column names to. (year)
      # * The name of the variable to move the column values to. (cases)
  
  tidy4a <- table4a %>% 
    pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

  tidy4b <- table4b %>% 
    pivot_longer(c(`1999`,`2000`), names_to = "year", values_to = "population")

  left_join(tidy4a, tidy4b)    
  
    
  # 3.2 Wider
  #~~~~~~~~~~
  
    # `pivot_wider()` is the opposite of `pivot_longer()`. You use it when an observation is scattered across multiple rows.
    
    table2

    # We only need two parameters:
      # * The column to take variable names from
      # * The column to take values from
    
  table2 %>% 
    pivot_wider(names_from = type, values_from = count)
    
    
  # 3.2 Exercises
  #~~~~~~~~~~~~~~~
  
    # > 1. Why are `pivot_longer()` and `pivot_wider()` not perfectly symmetrical? Carefully consider the following example:
    stocks <- tibble(
      year   = c(2015, 2015, 2016, 2016),
      half  = c(   1,    2,     1,    2),
      return = c(1.88, 0.59, 0.92, 0.17) 
    )
    stocks %>% 
      pivot_wider(names_from = year, values_from = return) %>% 
      pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return", names_ptypes = list(year = double()))
    # (Hint: look at the variable types and think about column names.)
    # pivot_longer() has a names_ptype argument, e.g. names_ptype = list(year = double()). What does it do?pivot
    
      # The functions pivot_wider() and pivot_longer() are not perfectly symmetrical because column type information is lost. and by default it specifies the the type of the columns generated from names_to will be character. And by setting the argument names_ptypes = list(year = double()) then it defines the type of the variable name to double.
    
  # > 2.Why does this code fail?
    table4a %>% 
      pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")
    # the error message says: Erreur : Can't subset columns that don't exist. x The locations 1999 and 2000 don't exist. i There are only 3 columns. 
    # The column names 1999 and 2000 are non syntactics and we must suround the names by backticks or double quotes to be identified. 
    table4a %>% 
      pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
    
  # > 3. What would happen if you widen this table? Why? How could you add a new column to uniquely identify each value?
    people <- tribble(
      ~name,             ~names,  ~values,
      #-----------------|--------|------
      "Phillip Woods",   "age",       45,
      "Phillip Woods",   "height",   186,
      "Phillip Woods",   "age",       50,
      "Jessica Cordero", "age",       37,
      "Jessica Cordero", "height",   156
    )
    
    
    people %>% 
      pivot_wider(names_from = names, values_from = values)
  
    # So we got a warning message indicates that values from the variable values are not uniquely identified her is the warning message:
    # Warning message:
    # Values in `values` are not uniquely identified; output will contain list-cols.
    # * Use `values_fn = list(values = list)` to suppress this warning.
    # * Use `values_fn = list(values = length)` to identify where the duplicates arise
    # * Use `values_fn = list(values = summary_fun)` to summarise duplicates 
    
    # For example we will have two observations with the combination of name "Phillip Woods" age.
    
    people %>% 
      pivot_wider(names_from = names, values_from = values, names_repair = "unique")
    
    # We could solve this problem by adding a row with a distinct observation count for each combination of name and names.
    
    people2 <- people %>% 
      group_by(name, names) %>% 
      mutate(obs = row_number())
    
    pivot_wider(people2, names_from = names, values_from = values)
    
    # Another way to solve this problem is by keeping only distinct rows of the name and key values, and dropping duplicate rows.
    
    people %>% 
      distinct(name, names, .keep_all = TRUE) %>%
      pivot_wider(names_from = names, values_from = values)
    
    # However, before doing this you would want to understand why there are duplicates in the data to begin with. This is usually not merely a nuisance, but indicates deeper problems with the data.
    
  # > 4. Tidy the simple tibble below. Do you need to make it wider or longer? What are the variables?
    preg <- tribble(
      ~pregnant, ~male, ~female,
          "yes",    NA,      10,
           "no",    20,      12,
    )

    # We have two variables that are actually values, so male and female are values of sex variable, for this I need to make it longer. 
    preg %>%
      pivot_longer(male:female ,names_to = "sex", values_to = "count")
    
    
# 4. Separating and uniting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # `table3` has a different problem: we have one column (`rate`) that contains two variables (`cases` and `population`).  
    
  # 4.1 Separate
  #~~~~~~~~~~~~~~  
    # separate() pulls apart one column into multiple columns, by splitting wherever a separator character appears.
    table3
    
    table3 %>% 
      separate(rate, into = c("cases", "population"))
      # Or to be more precise
    table3 %>% 
      separate(rate, into = c("cases", "population"), sep = "/")
    
    # The default behaviour in `separate()` leaves the type of the column as is. We can try to convert using `convert = TRUE` 
    table3 %>% 
      separate(rate, into = c("cases", "population"), convert = TRUE)
    
    # We can also pass a vector of integers to `sep`. Positive values start at 1 on the far-left of the strings; negative value start at -1 on the far-right of the strings.
    table5 <- table3 %>% 
      separate(year, into = c("century", "year"), sep = 2)
    
  # 4.2 Unite
  #~~~~~~~~~~~~
    # unite() is the inverse of separate(): it combines multiple columns into a single column. 
    table5 %>% 
      unite(new, century, year)
    # In this case we also need to use the `sep` argument. The default will place an underscore (`_`) between the values from different columns.
    table5 %>% 
      unite(new, century, year, sep = "")

  # 4.3 Exercises
  #~~~~~~~~~~~~~~~~  
    
    # > 1. What do the `extra` and `fill` arguments do in `separate()`? Experiment with the various options for the following two toy datasets.
    tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
      separate(x, c("one", "two", "three"))
    
    tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
      separate(x, c("one", "two", "three"))
  
    # The extra argument tells separate() what to do if there are too many pieces, and the fill argument tells it what to do if there aren’t enough. By default, separate() drops extra values with a warning.
    # extra = "drop", the extra will be droped without warning:
    tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
      separate(x, c("one", "two", "three"), extra = "drop")
    
    # extra = "merge", then the extra values are not split, so "f,g" appears in column three.
    tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
      separate(x, c("one", "two", "three"), extra = "merge")

    # The default for `fill` fills columns with missing values but emits a warning.
    # `fill = right`, will fill `NA` but without a warning
    tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
      separate(x, c("one", "two", "three"), fill = "right")
    
    # `fill = left`, will fill `NA` but without a warning
    tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
      separate(x, c("one", "two", "three"), fill = "left")
  
    # > 2. Both `unite()` and `separate()` have a remove argument. What does it do? Why would you set it to `FALSE`?
    
      # remove = TRUE (the default) remove input column when creating the new variables, to preserve it in the new data set, we need to set it to FALSE.
    
    # > 3. Compare and contrast separate() and extract(). Why are there three variations of separation (by position, by separator, and with groups), but only one unite?
    
      # The function separate(), splits a column into multiple columns by separator, if the sep argument is a character vector, or by character positions, if sep is numeric.
    
      # The function extract() uses a regular expression to specify groups in character vector and split that single character vector into multiple columns.
    
      tibble(x = c("X_1", "X_2", "AA_1", "AA_2")) %>%
        separate(x, c("variable", "into"), sep = "_")
      
      # example with separators
      tibble(x = c("X_1", "X_2", "AA_1", "AA_2")) %>%
        extract(x, c("variable", "id"), regex = "([A-Z])_([0-9])")
      
      # example with position
      tibble(x = c("X1", "X2", "Y1", "Y2")) %>%
        extract(x, c("variable", "id"), regex = "([A-Z])([0-9])")
      
      # example that separate could not parse
      tibble(x = c("X1", "X20", "AA11", "AA2")) %>%
        extract(x, c("variable", "id"), regex = "([A-Z]+)([0-9]+)")
  
      #In other words, with extract() and separate() only one column can be chosen, but there are many choices how to split that single column into different columns. With unite(), there are many choices as to which columns to include, but only one choice as to how to combine their contents into a single vector.
      
      
# 5. Missing values
#~~~~~~~~~~~~~~~~~~~
      
  # A value can be missing in one of two possible ways:
      
      # * Explicitly, i.e. flagged with NA.
      # * Implicitly, i.e. simply not present in the data. 
      
  stocks <- tibble(
    year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
    qtr    = c(   1,    2,    3,    4,    2,    3,    4),
    return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
  )

      # * The return for the fourth quarter of 2015 is explicitly missing, because the cell where its value should be instead contains NA.
      # * The return for the first quarter of 2016 is implicitly missing, because it simply does not appear in the dataset.
  
  # > One way to think about the difference is with this Zen-like koan: An explicit missing value is the presence of an absence; an implicit missing value is the absence of a presence. 
  
  # The way that a dataset is represented can make implicit values explicit. For example, we can make the implicit missing value explicit by putting years in the columns:
  
  stocks %>% 
    pivot_wider(names_from = year, values_from = return)

  # Because these explicit missing values may not be important in other representations of the data, you can set values_drop_na = TRUE in pivot_longer() to turn explicit missing values implicit:
  
  stocks %>% 
    pivot_wider(names_from = year, values_from = return) %>% 
    pivot_longer(
      cols = c(`2015`, `2016`),
      names_to = "year",
      values_to = "return",
      values_drop_na = TRUE
    )
  
  # Another important tool for making missing values explicit in tidy data is complete():
   
  stocks %>% 
    complete(year, qtr)
  
  # complete() takes a set of columns, and finds all unique combinations. It then ensures the original dataset contains all those values, filling in explicit NAs where necessary.
  
  # Sometimes when a data source has primarily been used for data entry, missing values indicate that the previous value should be carried forward:
  
  treatment <- tribble(
    ~ person,           ~ treatment, ~response,
    "Derrick Whitmore", 1,           7,
    NA,                 2,           10,
    NA,                 3,           9,
    "Katherine Burke",  1,           4
  )
  
  # You can fill in these missing values with fill(). It takes a set of columns where you want missing values to be replaced by the most recent non-missing value (sometimes called last observation carried forward).
  
  treatment %>% 
    fill(person)
  
  # 5.1 Exercises
  #~~~~~~~~~~~~~~
  
    # > 1. compare and contrast the fill arguments to pivot_wider() and complete().
  
      # for complete() sets a named list that for each variable supplies a single value to use instead of NA. for the pivot_wider the argument is named values_fill and it's a named list that have the same role.
  
    # > 2. What does the direction argument to fill() do?
  
      # Direction in which to fill missing values. Currently either "down" (the default), "up", "downup" (i.e. first down and then up) or "updown" (first up and then down).
  

# 6. Case Study
#~~~~~~~~~~~~~~~
  
  # The tidyr::who dataset contains tuberculosis (TB) cases broken down by year, country, age, gender, and diagnosis method. The data comes from the 2014 World Health Organization Global Tuberculosis Report, available at http://www.who.int/tb/country/data/download/en/.
  
  who
  
  # The best place to start is almost always to gather together the columns that are not variables. Let’s have a look at what we’ve got:
  
    # * It looks like country, iso2, and iso3 are three variables that redundantly specify the country.
  
    # * year is clearly also a variable.
  
    # * We don’t know what all the other columns are yet, but given the structure in the variable names (e.g. new_sp_m014, new_ep_m014, new_ep_f014) these are likely to be values, not variables.
  
  # So we need to gather together all the columns from new_sp_m014 to newrel_f65. We don’t know what those values represent yet, so we’ll give them the generic name "key". We know the cells represent the count of cases, so we’ll use the variable cases. There are a lot of missing values in the current representation, so for now we’ll use na.rm just so we can focus on the values that are present.
  
  who1 <- who %>% 
    pivot_longer(
      cols = new_sp_m014:newrel_f65,
      names_to = "key",
      values_to = "cases",
      values_drop_na = TRUE
    )

  who1  
  
  # We can get some hint of the structure of the values in the new key column by counting them:
  
  who1 %>% 
    count(key)
  
  # You might be able to parse this out by yourself with a little thought and some experimentation, but luckily we have the data dictionary handy. It tells us:
  
    # 1. The first three letters of each column denote whether the column contains new or old cases of TB. In this dataset, each column contains new cases.
  
    # 2. The next two letters describe the type of TB:
      # * `rel` stands for cases of relapse
      # * `ep` stands for cases of extrapulmonary TB
      # * `sn` stands for cases of pulmonary TB that could not be diagnosed by a pulmonary smear (smear negative)
      # * `sp` stands for cases of pulmonary TB that could be diagnosed be a pulmonary smear (smear positive) 
  
    # 3. The sixth letter gives the sex of TB patients. The dataset groups cases by males (m) and females (f).
  
    # 4. The remaining numbers gives the age group. The dataset groups cases into seven age groups:
      # * 014 = 0 – 14 years old
      # * 1524 = 15 – 24 years old
      # * 2534 = 25 – 34 years old
      # * 3544 = 35 – 44 years old
      # * 4554 = 45 – 54 years old
      # * 5564 = 55 – 64 years old
      # * 65 = 65 or older
    
  # We need to make a minor fix to the format of the column names, we'll replace the characters “newrel” with “new_rel”.  
  
  who2 <- who1 %>% 
    mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

  who2  
  
  # We can separate the values in each code with two passes of separate(). The first pass will split the codes at each underscore.
  
  who3 <- who2 %>% 
    separate(key, c("new", "type", "sexage"), sep = "_")
  
  who3
  
  # Then we might as well drop the new column because it’s constant in this dataset. While we’re dropping columns, let’s also drop iso2 and iso3 since they’re redundant.
  
  who3 %>% 
    count(new)
  
  who4 <-  who3 %>% 
    select(-new, -iso2, -iso3)
  
  # Next we’ll separate sexage into sex and age by splitting after the first character:
    
  who5 <- who4 %>% 
    separate(sexage, c("sex", "age"), sep = 1)

  who5  
  
  # The who dataset is now tidy!
  
  # I’ve shown you the code a piece at a time, assigning each interim result to a new variable. This typically isn’t how you’d work interactively. Instead, you’d gradually build up a complex pipe:
  
  who %>% 
    pivot_longer(
      cols = new_sp_m014:newrel_f65,
      names_to = "key",
      values_to = "cases",
      values_drop_na = TRUE
    ) %>% 
    mutate(
      key = stringr::str_replace(key, "newrel", "new_rel")
    ) %>% 
    separate(key, c("new", "var", "sexage")) %>% 
    select(-new, -iso2, -iso3) %>% 
    separate(sexage, c("sex", "age"), sep = 1)
  
  # 6.1 Exercices
  #~~~~~~~~~~~~~~~
  
    # > 1. In this case study I set values_drop_na = TRUE just to make it easier to check that we had the correct values. Is this reasonable? Think about how missing values are represented in this dataset. Are there implicit missing values? What’s the difference between an NA and zero?
  
      # If there are no 0 values in the data, then missing values may be used to indicate no cases.
  
      # If there are both explicit and implicit missing values, then it suggests that missing values are being used differently. In that case, it is likely that explicit missing values would mean no cases, and implicit missing values would mean no data on the number of cases.
  
      # First, I’ll check for the presence of zeros in the data.
      who1 %>% 
        filter(cases == 0) %>% 
        nrow()
      # There are zeros in the data, so it appears that cases of zero TB are explicitly indicated, and the value ofNA is used to indicate missing data.
      
      # Second, I should check whether all values for a (country, year) are missing or whether it is possible for only some columns to be missing.
      who %>% 
        pivot_longer(
          cols = new_sp_m014:newrel_f65,
          names_to = "key",
          values_to = "cases"
        ) %>% 
        group_by(country, year) %>% 
        mutate(prop_missing = sum(is.na(cases)) / n()) %>% 
        filter(prop_missing > 0, prop_missing < 1)
      # From the results above, it looks like it is possible for a (country, year) row to contain some, but not all, missing values in its columns.
      
      # Finally, I will check for implicit missing values. Implicit missing values are (year, country) combinations that do not appear in the data.
      nrow(who)
      
      who %>% 
        complete(country, year) %>% 
        nrow()
      # Since the number of complete cases of (country, year) is greater than the number of rows in who, there are some implicit values. But that doesn’t tell us what those implicit missing values are. 
      
      # To do this, I will use the anti_join() function introduced in the later Relational Data chapter.
      anti_join(complete(who, country, year), who, by = c("country", "year")) %>% 
        select(country, year) %>% 
        group_by(country) %>% 
        # so I can make better sense of the years
        summarise(min_years = min(year), max_year = max(year))
      # All of these refer to (country, year) combinations for years prior to the existence of the country. For example, Timor-Leste achieved independence in 2002, so years prior to that are not included in the data.
      
      # To summarize:
        # * 0 is used to represent no cases of TB.    
        # * Explicit missing values (NAs) are used to represent missing data for (country, year) combinations in which the country existed in that year.
        # * Implicit missing values are used to represent missing data because a country did not exist in that year.
      
    
    # > 2. What happens if you neglect the mutate() step? (mutate(names_from = stringr::str_replace(key, "newrel", "new_rel")))    
      
      who3a <- who1 %>% 
        separate(key, c("new", "type", "sexage"), sep = "_")
      
      # The separate() function emits the warning “too few values”. If we check the rows for keys beginning with "newrel_", we see that sexage is missing, and type = m014
      
      who3a %>% 
        filter(new == "newrel")
      
    # > 3. I claimed that iso2 and iso3 were redundant with country. Confirm this claim. 
      
      who %>% 
        select(country, iso2, iso3) %>% 
        distinct() %>% 
        group_by(country) %>%
        filter(n() > 1)
      
    # > 4. For each country, year, and sex compute the total number of cases of TB. Make an informative visualisation of the data.   
      
      who5 %>% 
        group_by(country, year, sex) %>% 
        filter(year > 1995) %>% 
        summarise(cases = sum(cases)) %>% 
        unite(country_sex, country, sex, remove = FALSE) %>% 
        ggplot(aes(x = year, y = cases, group = country_sex, colour = sex)) +
        geom_line()

      
      
# 7. Non-tidy data
#~~~~~~~~~~~~~~~~~~~
      
    # There are two main reasons to use other data structures: 
      # * Alternative representations may have substantial performance or space advantages.
      # * Specialised fields have evolved their own conventions for storing data that may be quite different to the conventions of tidy data.
      
      
              
      