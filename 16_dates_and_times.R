#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#
#                           15.DATES and TIMES                              #
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===# 


# 1. Introduction
#~~~~~~~~~~~~~~~~~~
  # I’m sure you know that not every year has 365 days, but do you know the full rule for determining if a year is a leap year? (It has three parts.) You might have remembered that many parts of the world use daylight savings time (DST), so that some days have 23 hours, and others have 25. You might not have known that some minutes have 61 seconds because every now and then leap seconds are added because the Earth’s rotation is gradually slowing down.

  # 1.1 Prerequisites
  #~~~~~~~~~~~~~~~~~~~~
    # This chapter will focus on the lubridate package, which makes it easier to work with dates and times in R. lubridate is not part of core tidyverse.
library(lubridate)


# 2. Creating date/times
#~~~~~~~~~~~~~~~~~~~~~~~~
  # There are three types of date/time data:
    # * A date. Tibbles print this as <date>.
    # * A time within a day. Tibbles print this as <time>.
    # * A date-time is a date plus a time: it uniquely identifies an instant in time (typically to the nearest second). Tibbles print this as <dttm>. Elsewhere in R these are called POSIXct.

  # R doesn’t have a native class for storing times. If you need one, you can use the hms package.

  # You should always use the simplest possible data type that works for your needs. 

  # To get the current date or date-time
today()
now()

  # Otherwise, there are three ways you’re likely to create a date/time:
    # * From a string.
    # * From individual date-time components.
    # * From an existing date/time object.

  # 2.1 From strings
  #~~~~~~~~~~~~~~~~~~
    # To use the helpers provided by lubridate, identify the order in which year, month, and day appear in your dates, then arrange “y”, “m”, and “d” in the same order. That gives you the name of the lubridate function that will parse your date.
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

    # These functions also take unquoted numbers.
ymd(20170131)
    # ymd() and friends create dates.

    # To create a date-time, add an underscore and one or more of “h”, “m”, and “s” to the name of the parsing function
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

    # You can also force the creation of a date-time from a date by supplying a timezone
ymd(20170131, tz = "UTC")

  # 2.2 From individual components
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Sometimes you’ll have the individual components of the date-time spread across multiple columns.
flights %>% 
  select(year, month, day, hour, minute)

    # To create a date/time from this sort of input, use make_date() for dates, or make_datetime() for date-times:
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

    # Let’s do the same thing for each of the four time columns in flights. The times are represented in a slightly odd format, so we use modulus arithmetic to pull out the hour and minute components. Once I’ve created the date-time variables, I focus in on the variables we’ll explore in the rest of the chapter.
make_datetime_100 <- function(year, month, day, time){
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
  
flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time), 
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

    # With this data, I can visualise the distribution of departure times across the year
flights_dt %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

    # Or within a single day
flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes
    # Note that when you use date-times in a numeric context (like in a histogram), 1 means 1 second, so a binwidth of 86400 means one day. For dates, 1 means 1 day.

  # 2.3 From other types
  #~~~~~~~~~~~~~~~~~~~~~~
    #  To switch between a date-time and a date. That’s the job of as_datetime() and as_date()
as_datetime(today())
as_date(now())

  # 2.4 Exercises
  #~~~~~~~~~~~~~~~

    # > 1. What happens if you parse a string that contains invalid dates?
ymd(c("2010-10-10", "bananas"))
      # It produces an NA and a warning message.

    # > 2. What does the tzone argument to today() do? Why is it important?
?today
      # It determines the time-zone of the date. Since different time-zones can have different dates, the value of today() can vary depending on the time-zone specified.

    # > 3. Use the appropriate lubridate function to parse each of the following dates:
d1 <- "January 1, 2010"
mdy(d1)
d2 <- "2015-Mar-07"
ymd(d2)
d3 <- "06-Jun-2017"
dmy(d3)
d4 <- c("August 19 (2015)", "July 1 (2015)")
mdy(d4)
d5 <- "12/30/14" # Dec 30, 2014
mdy(d5)

# 3. Date-time components
#~~~~~~~~~~~~~~~~~~~~~~~~~~

  # 3.1 Getting components
  #~~~~~~~~~~~~~~~~~~~~~~~~
    # You can pull out individual parts of the date with the accessor functions year(), month(), mday() (day of the month), yday() (day of the year), wday() (day of the week), hour(), minute(), and second().
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)

yday(datetime)
wday(datetime)

    # For month() and wday() you can set label = TRUE to return the abbreviated name of the month or day of the week. Set abbr = FALSE to return the full name.
month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

    # We can use wday() to see that more flights depart during the week than on the weekend:
flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

    # There’s an interesting pattern if we look at the average departure delay by minute within the hour. It looks like flights leaving in minutes 20-30 and 50-60 have much lower delays than the rest of the hour!
flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

    # Interestingly, if we look at the scheduled departure time we don’t see such a strong pattern:
sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

    # So why do we see that pattern with the actual departure times? Well, like much data collected by humans, there’s a strong bias towards flights leaving at “nice” departure times. Always be alert for this sort of pattern whenever you work with data that involves human judgement!
ggplot(sched_dep, aes(minute, n)) +
  geom_line()

  # 3.2 Rounding
  #~~~~~~~~~~~~~~
    # An alternative approach to plotting individual components is to round the date to a nearby unit of time, with floor_date(), round_date(), and ceiling_date(). Each function takes a vector of dates to adjust and then the name of the unit round down (floor), round up (ceiling), or round to. This, for example, allows us to plot the number of flights per week
flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

  # 3.3 Setting components
  #~~~~~~~~~~~~~~~~~~~~~~~~
    # You can also use each accessor function to set the components of a date/time:
(datetime <- ymd_hms("2016-07-08 12:34:56"))

year(datetime) <- 2020
datetime
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime

    # Alternatively, rather than modifying in place, you can create a new date-time with update(). This also allows you to set multiple values at once.
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

    # If values are too big, they will roll-over:
ymd("2015-02-01") %>% 
  update(mday = 30)
ymd("2015-02-01") %>% 
  update(hour = 400)

    # You can use update() to show the distribution of flights across the course of the day for every day of the year:
flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

    #Setting larger components of a date to a constant is a powerful technique that allows you to explore patterns in the smaller components.

  # 3. Exercises
  #~~~~~~~~~~~~~~
    # > 1. How does the distribution of flight times within a day change over the course of the year?
flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  mutate(month = factor(month(dep_time))) %>% 
  ggplot(aes(dep_hour, color = month)) +
  geom_freqpoly(binwidth = 3600)

      #This will look better if everything is normalized within groups. The reason that February is lower is that there are fewer days and thus fewer flights.
flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  mutate(month = factor(month(dep_time))) %>% 
  ggplot(aes(dep_hour, y = ..density.., color = month)) +
  geom_freqpoly(binwidth = 3600)

    # > 2. Compare dep_time, sched_dep_time and dep_delay. Are they consistent? Explain your findings.
      # dep_time = sched_dep_time + dep_delay
flights_dt %>% 
  select(dep_time, sched_dep_time, dep_delay) %>% 
  mutate(diff_dep_time = as.double.difftime(dep_time - sched_dep_time) / 60) %>%
  filter(diff_dep_time != dep_delay)
      # There exist discrepancies. It looks like there are mistakes in the dates. These are flights in which the actual departure time is on the next day relative to the scheduled departure time. The code would have had to check if the departure time is less than the scheduled departure time plus departure delay (in minutes). Alternatively, simply adding the departure delay to the scheduled departure time is a more robust way to construct the departure time because it will automatically account for crossing into the next day.

    # > 3. Compare air_time with the duration between the departure and arrival. Explain your findings. (Hint: consider the location of the airport.)
flights_dt %>% 
  select(arr_time, dep_time, air_time) %>% 
  mutate(diff_time = arr_time - dep_time) %>% 
  filter(as.double.difftime(diff_time) == air_time)
# There are only 196 flights where the air_time aqual the difference between dep_time and arr_time. I think that may be the arrival time was recorded with the time zone of the destination airport.

    # > 4. How does the average delay time change over the course of a day? Should you use dep_time or sched_dep_time? Why?
flights_dt %>% 
  mutate(sched_dep_hour = hour(sched_dep_time)) %>% 
  group_by(sched_dep_hour) %>% 
  summarise(avg_dep_delay = mean(dep_delay)) %>% 
  ggplot(aes(sched_dep_hour, avg_dep_delay)) +
  geom_line()
  
    # > 5. On what day of the week should you leave if you want to minimise the chance of a delay?
flights_dt %>% 
  mutate(sched_dep_week = wday(sched_dep_time, label = TRUE)) %>% 
  group_by(sched_dep_week) %>% 
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(sched_dep_week, avg_dep_delay)) +
  geom_bar(stat = "identity")
      # Saturday has the lower average delay.

flights_dt %>% 
  mutate(sched_dep_week = wday(sched_dep_time, label = TRUE)) %>% 
  group_by(sched_dep_week) %>% 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% 
  ggplot(aes(sched_dep_week, avg_arr_delay)) +
  geom_bar(stat = "identity")

    # > 6. What makes the distribution of diamonds$carat and flights$sched_dep_time similar?
diamonds %>% 
  ggplot(aes(carat)) + 
  geom_histogram(binwidth = .1)

flights_dt %>% 
  ggplot(aes(minute(sched_dep_time))) +
  geom_histogram(binwidth = 1)

      # In both carat and sched_dep_time there are abnormally large numbers of values are at nice “human” numbers. In sched_dep_time it is at 00 and 30 minutes. In carats, it is at 0, 1/3, 1/2, 2/3, 
      # In scheduled departure times it is 00 and 30 minutes, and minutes ending in 0 and 5.

    # > 7. Confirm my hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early. Hint: create a binary variable that tells you whether or not a flight was delayed.

      # First, I create a binary variable early that is equal to 1 if a flight leaves early, and 0 if it does not. Then, I group flights by the minute of departure. This shows that the proportion of flights that are early departures is highest between minutes 20–30 and 50–60.
flights_dt %>% 
  mutate(
    minute = minute(dep_time),
    early = dep_delay < 0
  ) %>% 
  group_by(minute) %>% 
  summarise(
    early = mean(early, na.rm = TRUE),
    n = n()
  ) %>% 
  ggplot(aes(minute, early)) +
  geom_line()

# 4. Time spans
#~~~~~~~~~~~~~~~~

  # you’ll learn about three important classes that represent time spans:
    # * durations, which represent an exact number of seconds.
    # * periods, which represent human units like weeks and months.
    # * intervals, which represent a starting and ending point.

  # 4.1 Durations
  #~~~~~~~~~~~~~~~~
    # In R, when you subtract two dates, you get a difftime object:
my_age <- today() - ymd(19790405)
my_age

    # A difftime class object records a time span of seconds, minutes, hours, days, or weeks. This ambiguity can make difftimes a little painful to work with, so lubridate provides an alternative which always uses seconds: the duration.
as.duration(my_age)

    # Durations come with a bunch of convenient constructors:
dseconds(15)
dminutes(10)
dhours(c(12,24))
ddays(0:5)
dweeks(3)
dyears(1)

    # Durations always record the time span in seconds. Larger units are created by converting minutes, hours, days, weeks, and years to seconds at the standard rate (60 seconds in a minute, 60 minutes in an hour, 24 hours in day, 7 days in a week, 365 days in a year).

    # You can add and multiply durations:
2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)

    # You can add and subtract durations to and from days:
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)

    # However, because durations represent an exact number of seconds, sometimes you might get an unexpected result:
one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm
one_pm + ddays(1)

    # Why is one day after 1pm on March 12, 2pm on March 13?! If you look carefully at the date you might also notice that the time zones have changed. Because of DST, March 12 only has 23 hours, so if we add a full days worth of seconds we end up with a different time.

  # 4.2 Periods
  #~~~~~~~~~~~~~~

    # To solve this problem, lubridate provides periods. Periods are time spans but don’t have a fixed length in seconds, instead they work with “human” times, like days and months. That allows them work in a more intuitive way:
one_pm
one_pm + days(1)

    # Like durations, periods can be created with a number of friendly constructor functions.
seconds(15)
minutes(10)
hours(c(12,24))
days(7)
months(1:6)
weeks(3)
years(1)

    # You can add and multiply periods:
10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

    # And of course, add them to dates. Compared to durations, periods are more likely to do what you expect:

# A leap year
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

# Daylight Savings Time
one_pm + ddays(1)
one_pm + days(1)

    # Let’s use periods to fix an oddity related to our flight dates. Some planes appear to have arrived at their destination before they departed from New York City.
flights_dt %>% 
  filter(arr_time < dep_time)

    # These are overnight flights. We used the same date information for both the departure and the arrival times, but these flights arrived on the following day. We can fix this by adding days(1) to the arrival time of each overnight flight.
flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )

    # Now all of our flights obey the laws of physics
flights_dt %>% 
  filter(overnight, arr_time < dep_time)

  # 4.3 Intervals
  #~~~~~~~~~~~~~~~

    # It’s obvious what dyears(1) / ddays(365) should return: one, because durations are always represented by a number of seconds, and a duration of a year is defined as 365 days worth of seconds.

    # What should years(1) / days(1) return? Well, if the year was 2015 it should return 365, but if it was 2016, it should return 366! There’s not quite enough information for lubridate to give a single clear answer. What it does instead is give an estimate, with a warning:
years(1) / days(1) # it gives 365.25 without warning!!!

    # If you want a more accurate measurement, you’ll have to use an interval. An interval is a duration with a starting point: that makes it precise so you can determine exactly how long it is
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)

    # To find out how many periods fall into an interval, you need to use integer division:
(today() %--% next_year) %/% days(1)

  # 4.4 Summary
  #~~~~~~~~~~~~~~

    # How do you pick between duration, periods, and intervals? As always, pick the simplest data structure that solves your problem. If you only care about physical time, use a duration; if you need to add human times, use a period; if you need to figure out how long a span is in human units, use an interval.

  # 4.5 Exercises
  #~~~~~~~~~~~~~~~

    # > 1. Why is there months() but no dmonths()?
      # There is no direct unambiguous value of months in seconds since months have differing numbers of days. 31, 30, 28 or 29 days.

    # > 2. Explain days(overnight * 1) to someone who has just started learning R. How does it work?
      # The variable overnight is equal to TRUE or FALSE. 1 is the value of TRUE in R and 0 is the value of False.

    # > 3. Create a vector of dates giving the first day of every month in 2015. Create a vector of dates giving the first day of every month in the current year.
ymd("2015-01-01") + months(0:11)
floor_date(today(), unit = "year") + months(0:11)

    # > 4. Write a function that given your birthday (as a date), returns how old you are in years.
age <- function(bday){
  (bday %--% today()) %/% years(1)
}
age(ymd("1979-04-05"))

    # > 5. Why can’t (today() %--% (today() + years(1)) / months(1) work?
(today() %--% (today() + years(1))) / months(1)
      # The code in the question is missing a parentheses.
      #  Months can be 28, 29, 30, or 31 days, so it is not clear what months(1) divide by? The code does not produce a warning message, but it will not always produce the correct result.
      # To find the number of months within an interval use %/% instead of /,
      # Alternatively, we could define a “month” as 30 days, and run
(today() %--% (today() + years(1))) / days(30)
      # This approach will not work with today() + years(1), which is not defined for February 29th on leap years:
as.Date("2016-02-29") + years(1)

# 5. Time zones
#~~~~~~~~~~~~~~~~

  # Time zones are an enormously complicated. Fortunately we don’t need to dig into all the details as they’re not all important for data analysis.

  # R uses the international standard IANA time zones. These use a consistent naming scheme “/”, typically in the form “<continent>/<city>” (there are a few exceptions because not every country lies on a continent).  It’s worth reading the raw time zone database (available at http://www.iana.org/time-zones) just to read some of these stories!

  # You can find out what R thinks your current time zone is with
Sys.timezone() # (If R doesn’t know, you’ll get an NA.)

  # And see the complete list of all time zone names with OlsonNames()
length(OlsonNames())
head(OlsonNames())

  # In R, the time zone is an attribute of the date-time that only controls printing. For example, these three objects represent the same instant in time:
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

  # You can verify that they’re the same time using subtraction:
x1 - x2
x1 - x3

  # Unless otherwise specified, lubridate always uses UTC. UTC (Coordinated Universal Time) is the standard time zone used by the scientific community and roughly equivalent to its predecessor GMT (Greenwich Mean Time). It does not have DST, which makes a convenient representation for computation. Operations that combine date-times, like c(), will often drop the time zone. In that case, the date-times will display in your local time zone:
x4 <- c(x1, x2, x3)
x4

  # You can change the time zone in two ways:
    # Keep the instant in time the same, and change how it’s displayed. Use this when the instant is correct, but you want a more natural display
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a # (This also illustrates another challenge of times zones: they’re not all integer hour offsets!)
x4a - x4

    # Change the underlying instant in time. Use this when you have an instant that has been labelled with the incorrect time zone, and you need to fix it
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4










