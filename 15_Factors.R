
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#
#                               15.FACTORS                                  #
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===# 


# 1. Introduction
#~~~~~~~~~~~~~~~~~~

  # In R, factors are used to work with categorical variables, variables that have a fixed and known set of possible values. They are also useful when you want to display character vectors in a non-alphabetical order.

  # 1.1 Prerequisites
  #~~~~~~~~~~~~~~~~~~~~

    # We’ll use the **forcats** package, which is part of the core tidyverse.
library(tidyverse)

  # 1.2 Learning more
  #~~~~~~~~~~~~~~~~~~~~

    #  recommend reading Amelia McNamara and Nicholas Horton’s paper, [Wrangling categorical data in R](https://peerj.com/preprints/3163/).


# 2. Creating factors
#~~~~~~~~~~~~~~~~~~~~~~

x1 <- c("Dec", "Apr", "Jan", "Mar") # variable that records month
x2 <- c("Dec", "Apr", "Jam", "Mar") # nothing saving you from typos
sort(x1) # It doesn’t sort in a useful way

  # You can fix both of these problems with a factor.

  # To create a factor you must start by creating a list of the valid **levels**
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

  # Now you can create a factor
y1 <- factor(x1, levels = month_levels)
y1
sort(y1)  

  # And any values not in the set will be silently converted to NA
y2 <- factor(x2, levels = month_levels)
y2

  # If you want a warning, you can use readr::parse_factor()
y2 <- parse_factor(x2, levels = month_levels)

  # If you omit the levels, they’ll be taken from the data in alphabetical order
factor(x1)

  # If you prefer that the order of the levels match the order of the first appearance in the data. You can do that when creating the factor by
f1 <- factor(x1, levels = unique(x1)) # setting levels to unique(x)
f1
f2 <- x1 %>% factor() %>% fct_inorder() # or after the fact, with fct_inorder()
f2

  # to access the set of valid levels directly
levels(f2)


# 3. General Social Survey
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

gss_cat # a sample of data from the General Social Survey
?gss_cat # more information about the variables

  # One way to see levels in tibble is with count()
gss_cat %>% 
  count(race)

  # or with bar chart
ggplot(gss_cat, aes(race)) +
  geom_bar()

  # By default, ggplot2 will drop levels that don’t have any values. You can force them to display
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

  # When working with factors, the two most common operations are changing the order of the levels, and changing the values of the levels.

  # 3.1 Exercises
  #~~~~~~~~~~~~~~~

    # > 1. Explore the distribution of rincome (reported income). What makes the default bar chart hard to understand? How could you improve the plot?
ggplot(gss_cat, aes(rincome)) +
  geom_bar()
      # The default bar chart is difficult to read because we could not read the values of the reported income.
levels(gss_cat$rincome)
      # We can change the coordinate scale 
ggplot(gss_cat, aes(rincome)) +
  geom_bar() +
  coord_flip()
      # And it'll be better if we reoder the levels

    # > 2. What is the most common relig in this survey? What’s the most common partyid?
gss_cat %>% 
  count(relig, sort = TRUE)

ggplot(gss_cat, aes(relig)) +
  geom_bar() +
  coord_flip()
      # the most common relig in this survey is Protestant followed by catholic then none.
gss_cat %>% 
  count(partyid, sort = TRUE)

ggplot(gss_cat, aes(partyid)) +
  geom_bar() +
  coord_flip()
      # the most common partyid is Independent then Not str democrat then strong democrat

    # > 3. Which relig does denom (denomination) apply to? How can you find out with a table? How can you find out with a visualisation?
levels(gss_cat$denom)

gss_cat %>% 
  filter(!denom %in% c("No answer", "Other", "Don't know", "Not applicable", "No denomination")) %>% 
  count(relig)
      # the only one that still is Protestant.

gss_cat %>% 
  count(relig, denom)

ggplot(gss_cat, aes(relig, denom)) +
  geom_count()

gss_cat %>% 
  count(relig, denom) %>% 
  ggplot(aes(relig, denom, fill = n)) +
  geom_tile()


# 4. Modifying factor order
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # It’s often useful to change the order of the factor levels in a visualisation. imagine you want to explore the average number of hours spent watching TV per day across religions
relig_summary <- gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

  # It is difficult to interpret this plot because there’s no overall pattern. We can improve it by reordering the levels of relig using fct_reorder().
  # fct_reorder() takes three arguments:
    # * f, the factor whose levels you want to modify.
    # * x, a numeric vector that you want to use to reorder the levels.
    # * Optionally, fun, a function that’s used if there are multiple values of x for each value of f. The default value is median.

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) + geom_point()

  # Reordering religion makes it much easier to see that people in the “Don’t know” category watch much more TV, and Hinduism & Other Eastern religions watch much less.

  # As you start making more complicated transformations, I’d recommend moving them out of aes() and into a separate mutate() step. For example, you could rewrite the plot above as:

relig_summary %>% 
  mutate(relig = fct_reorder(relig, tvhours)) %>% 
  ggplot(aes(tvhours, relig)) +
  geom_point()

  # What if we create a similar plot looking at how average age varies across reported income level?

rincome_summary <- gss_cat %>% 
  group_by(rincome) %>% 
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()
    # Here, arbitrarily reordering the levels isn’t a good idea! That’s because rincome already has a principled order that we shouldn’t mess with. Reserve fct_reorder() for factors whose levels are arbitrarily ordered.

  # However, it does make sense to pull “Not applicable” to the front with the other special levels. You can use fct_relevel(). It takes a factor, f, and then any number of levels that you want to move to the front of the line.
ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) + 
  geom_point()

  # Another type of reordering is useful when you are colouring the lines on a plot. fct_reorder2() reorders the factor by the y values associated with the largest x values. This makes the plot easier to read because the line colours line up with the legend.
by_age <- gss_cat %>% 
  filter(!is.na(age)) %>% 
  count(age, marital) %>% 
  group_by(age) %>% 
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

  # Finally, for bar plots, you can use fct_infreq() to order levels in increasing frequency: this is the simplest type of reordering because it doesn’t need any extra variables. You may want to combine with fct_rev().
gss_cat %>% 
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>% 
  ggplot(aes(marital)) +
  geom_bar()

  # 4.1 Exercices
  #~~~~~~~~~~~~~~~~

    # > 1. There are some suspiciously high numbers in tvhours. Is the mean a good summary?
summary(gss_cat$tvhours)

ggplot(aes(tvhours)) +
  geom_histogram(binwidth = 1)

    # > 2. For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.
levels(gss_cat[["marital"]]) # Somewhat principled
levels(gss_cat[["race"]]) # Arbitrary
levels(gss_cat[["rincome"]]) # Principled
levels(gss_cat[["partyid"]]) # Principled
levels(gss_cat[["relig"]]) # Arbitrary
levels(gss_cat[["denom"]]) # Arbitrary?

    # > 3. Why did moving “Not applicable” to the front of the levels move it to the bottom of the plot?

levels(fct_relevel(gss_cat[["rincome"]]))
?fct_relevel
# Because that gives the level “Not applicable” an integer value of 1.


# 5. Modifying factor levels
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # The most general and powerful tool is fct_recode(). It allows you to recode, or change, the value of each level.
gss_cat %>% count(partyid)

  # The levels are terse and inconsistent. Let’s tweak them to be longer and use a parallel construction.
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
    "Republican, strong"    = "Strong republican",
    "Republican, weak"      = "Not str republican",
    "Independent, near rep" = "Ind,near rep",
    "Independent, near dem" = "Ind,near dem",
    "Democrat, weak"        = "Not str democrat",
    "Democrat, strong"      = "Strong democrat"
  )) %>%
 count(partyid)
  # fct_recode() will leave levels that aren’t explicitly mentioned as is, and will warn you if you accidentally refer to a level that doesn’t exist.

  # To combine groups, you can assign multiple old levels to the same new level:
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
    "Republican, strong"    = "Strong republican",
    "Republican, weak"      = "Not str republican",
    "Independent, near rep" = "Ind,near rep",
    "Independent, near dem" = "Ind,near dem",
    "Democrat, weak"        = "Not str democrat",
    "Democrat, strong"      = "Strong democrat",
    "Other"                 = "No answer",
    "Other"                 = "Don't know",
    "Other"                 = "Other party"
  )) %>%
  count(partyid)
  #You must use this technique with care: if you group together categories that are truly different you will end up with misleading results

  # If you want to collapse a lot of levels, fct_collapse() is a useful variant of fct_recode(). For each new variable, you can provide a vector of old levels:
gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
    other = c("No answer", "Don't know", "Other party"),
    rep = c("Strong republican", "Not str republican"),
    ind = c("Ind,near rep", "Independent", "Ind,near dem"),
    dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

  # Sometimes you just want to lump together all the small groups to make a plot or table simpler. That’s the job of fct_lump():
gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)
  # The default behaviour is to progressively lump together the smallest groups, ensuring that the aggregate is still the smallest group. In this case it’s not very helpful: it is true that the majority of Americans in this survey are Protestant, but we’ve probably over collapsed.

  # Instead, we can use the n parameter to specify how many groups (excluding other) we want to keep:
gss_cat %>% 
  mutate(relig = fct_lump(relig, n = 10)) %>% 
  count(relig, sort = TRUE) %>% 
  print(n = Inf)

  # 5.1 Exercises
  #~~~~~~~~~~~~~~~~

    # > 1. How have the proportions of people identifying as Democrat, Republican, and Independent changed over time?
gss_cat %>% 
  count(partyid, year) %>% 
  group_by(year) %>% 
  mutate(prop = n / sum(n),
         partyid = fct_reorder2(partyid, year, prop)
         ) %>% 
  ggplot(aes(year, prop, colour = partyid)) +
  geom_line() +
  labs(colour = "Party ID")

    # > 2. How could you collapse rincome into a small set of categories?
gss_cat %>% count(rincome)

gss_cat %>% 
  mutate(rincome = fct_collapse(rincome,
    Unknown = c("No answer", "Don't know", "Refused", "Not applicable"),
    'Lt $5000' = c("Lt $1000", str_c("$", c("1000", "3000", "4000"), " to ", c("2999", "3999", "4999")
    )),
    '$5000 - 9999' = c(str_c("$", c("5000", "6000", "7000", "8000"), " to ", c("5999", "6999", "7999", "9999")
    )),
  )) %>% 
  ggplot(aes(rincome)) +
  geom_bar() +
  coord_flip()
