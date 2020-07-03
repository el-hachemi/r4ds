
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#
#                             RELATIONAL DATA                               #
#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===# 



# 1. Introduction
#~~~~~~~~~~~~~~~~~

 # To work with relational data you need verbs that work with pairs of tables. There are three families of verbs designed to work with relational data:
  
    # * Mutating joins, which add new variables to one data frame from matching observations in another.

    # * Filtering joins, which filter observations from one data frame based on whether or not they match an observation in the other table.

    # * Set operations, which treat observations as if they were set elements.

  library(tidyverse)
  library(nycflights13)


# 2. nycflights13
#~~~~~~~~~~~~~~~~~
  
  # nycflights13 contains four tibbles that are related to the `flights` table.
  
  airlines # full carrier name from its abbreviated code
  airports # each airport, identified by the faa airport code
  planes # each plane, identified by its tailnum
  weather # the weather at each NYC airport for each hour
  
  # One way to show the relationships between the different tables is with a drawing (diagram)
  
  # 2.1 Exercices
  #~~~~~~~~~~~~~~
  
    # > 1. Imagine you wanted to draw (approximately) the route each plane flies from its origin to its destination. What variables would you need? What tables would you need to combine?
  
      # I would need the variables that set the localisation of the airports `lat` and `lon` and `alt` from the `airports` table and the variables `origin` and `dest` from the `filghts` table
  
      flights_latlon <- flights %>% 
        inner_join(select(airports, origin = faa, origin_lat = lat, origin_lon = lon), 
                   by = "origin"
        ) %>% 
        inner_join(select(airports, dest = faa, dest_lat = lat, dest_lon = lon),
                   by = "dest"
        )
      
      flights_latlon %>% 
        slice(1:100) %>% 
        ggplot(aes(
          x = origin_lon, xend = dest_lon,
          y = origin_lat, yend = dest_lat
        )) +
        borders("state") +
        geom_segment(arrow = arrow(length = unit(0.1, "cm"))) +
        coord_quickmap() +
        labs(y = "Latitude", x = "Longitude")
  
  # > 2. I forgot to draw the relationship between weather and airports. What is the relationship and how should it appear in the diagram?
      
    # The column `airport$faa` is a foreingn key of `weather$origin`. 
      
  # > 3. `weather` only contains information for the origin (NYC) airports. If it contained weather records for all airports in the USA, what additional relation would it define with `flights`?   
      
    # It will be a relation with flights$dest.
      
  # > 4. We know that some days of the year are “special”, and fewer people than usual fly on them. How might you represent that data as a data frame? What would be the primary keys of that table? How would it connect to the existing tables?
      
    special_days <- tribble(
      ~year, ~month, ~day, ~holiday,
      2013, 01, 01, "New Years Day",
      2013, 07, 04, "Indepence Day",
      2013, 11, 29, "Thanks giving",
      2013, 12, 25, "Christmas Day"
    )
  
    # It would be connecting to other tables with the year, month and day columns.
    
  
# 3. Keys
#~~~~~~~~~
    
  # A key is a variable (or set of variables) that uniquely identifies an observation. There are two types of keys:
    
    # * A **primary key** uniquely identifies an observation in its own table.
    # * A **foreign key** uniquely identifies an observation in another table.
    
  # Once you’ve identified the primary keys in your tables, it’s good practice to verify that they do indeed uniquely identify each observation.
    
    planes %>% 
      count(tailnum) %>% 
      filter(n > 1)
    
  # Sometimes a table doesn’t have an explicit primary key: each row is an observation, but no combination of variables reliably identifies it.  
    
    flights %>% 
      count(year, month, day, flight) %>% 
      filter(n > 1) # This is not unique
    
    flights %>% 
      count(year, month, day, tailnum) %>% 
      filter(n > 1) # This is not unique
    
  # If a table lacks a primary key, it’s sometimes useful to add one with mutate() and row_number(). That makes it easier to match observations if you’ve done some filtering and want to check back in with the original data. This is called a **surrogate key**.
    
  # A primary key and the corresponding foreign key in another table form a relation. Relations are typically one-to-many. For example, each flight has one plane, but each plane has many flights. In other data, you’ll occasionally see a 1-to-1 relationship. You can think of this as a special case of 1-to-many. You can model many-to-many relations with a many-to-1 relation plus a 1-to-many relation. For example, in this data there’s a many-to-many relationship between airlines and airports: each airline flies to many airports; each airport hosts many airlines.
    
  # 3.1 Exercises
  #~~~~~~~~~~~~~~~~
    
    # > 1. Add a surrogate key to `flights`.
    
      flights %>% 
        mutate(num = row_number())
      
    # > 2. Identify the keys in the following datasets
      # (You might need to install some packages and read some documentation.)   
      Lahman::Batting %>% 
        count(playerID, yearID, stint) %>% 
        filter(n > 1)
      # (playerID, yearID, stint) are the primary key.
      
      babynames::babynames %>% 
        count(year, name, sex) %>% 
        filter(n > 1)
      # (year, name, sex) variables are the primary key.
      
      nasaweather::atmos %>% 
        count(lat, long, year, month) %>% 
        filter(n > 1)
      # (lat, long, year, month) variables form the primary key.

      fueleconomy::vehicles %>% 
        count(id) %>% 
        filter(n > 1)
      # `id` The unique EPA identifier is the primary key.
      
      ggplot2::diamonds %>% 
        count(x, y, z, price) %>% 
        filter(n > 1)
      ggplot2::diamonds %>% 
        distinct() %>% 
        nrow()
      nrow(ggplot2::diamonds)    
      # There is no primary key for ggplot2::diamonds since there is no combination of variables that uniquely identifies each observation. This is implied by the fact that the number of distinct rows in the dataset is less than the total number of rows, meaning that there are some duplicate rows.
      diamonds <- mutate(ggplot2::diamonds, id = row_number())
      #If we need a unique identifier for our analysis, we could add a surrogate key.
      
      # 3. Draw a diagram illustrating the connections between the Batting, Master, and Salaries tables in the Lahman package. Draw another diagram that shows the relationship between Master, Managers, AwardsManagers.
      # How would you characterise the relationship between the Batting, Pitching, and Fielding tables?
      
        # Let's see the primary keys for the different tables
      
        glimpse(Batting)
        Batting %>% 
          count(playerID, yearID, stint) %>% 
          filter(n > 1)
        
        glimpse(Master)
        Master %>% 
          count(playerID) %>% 
          filter(n > 1)
        
        glimpse(Salaries) 
        Salaries %>% 
          count(yearID, playerID, teamID) %>% 
          filter(n > 1)
        
        library(datamodelr)
      dm1 <- dm_from_data_frames(list(
        Batting = Lahman::Batting,
        Master = Lahman::Master,
        Salaries = Lahman::Salaries
      )) %>% 
        dm_set_key("Batting", c("playerID", "yearID", "stint")) %>% 
        dm_set_key("Master", "playerID") %>% 
        dm_set_key("Salaries", c("yearID", "teamID", "playerID")) %>% 
        dm_add_references(
          Batting$playerID == Master$playerID,
          Salaries$playerID == Master$playerID
        )
        
        dm_create_graph(dm1, rankdir = "LR", columnArrows = TRUE) %>% 
          dm_render_graph()
        
        
      
      
      
# 4. Mutating joins
#~~~~~~~~~~~~~~~~~~~~
    
  # A mutating join allows you to combine variables from two tables.
      
    flights2 <- flights %>% 
      select(year:day, hour, origin,dest, tailnum,carrier)
  
  # To add the full airline name to the flights2.
    
    flights2 %>% 
      select(-origin, -dest) %>% 
      left_join(airlines, by = "carrier")
  
  # The result is an additional variable: name. This is why Hadley called it mutating join. And we could have the same place with the following:
    
    flights2 %>% 
      select(-origin, -dest) %>% 
      mutate(name = airlines$name[match(carrier, airlines$carrier)])
    
  # But this is hard to generalise when you need to match multiple variables, and takes close reading to figure out the overall intent. 
    
  # 4.1. Understanding joins  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    x <- tribble(
      ~key, ~val_x,
      1, "x1",
      2, "x2",
      3, "x3"
    )
    y <- tribble(
      ~key, ~val_y,
      1, "y1",
      2, "y2",
      4, "y3"
    )
    
  # 4.2 Inner join
  #~~~~~~~~~~~~~~~~
    
    # The simplest type of join is the inner join. An inner join matches pairs of observations whenever their keys are equal:
    
    # (To be precise, this is an inner equijoin because the keys are matched using the equality operator. Since most joins are equijoins we usually drop that specification.)
    
    x %>% 
      inner_join(y, by = "key")
    
    # The most important property of an inner join is that unmatched rows are not included in the result. This means that generally inner joins are usually not appropriate for use in analysis because it’s too easy to lose observations.
    
  # 4.3 Outer joins 
  #~~~~~~~~~~~~~~~~~~  
    
    # An inner join keeps observations that appear in both tables. An outer join keeps observations that appear in at least one of the tables.
    
      # * A left join keeps all observations in x.
      # * A right join keeps all observations in y.
      # * A full join keeps all observations in x and y.
    
  # 4.4 Duplicate keys
  #~~~~~~~~~~~~~~~~~~~~
    
    # When the keys are not unique there are two possibilities:
    
      # 1. One table has duplicate keys. This is useful when you want to add in additional information as there is typically a one-to-many relationship.
    
        # Note that I’ve put the key column in a slightly different position in the output. This reflects that the key is a primary key in y and a foreign key in x.
    
      x <- tribble(
        ~key, ~val_x,
           1, "x1",
           2, "x2",
           2, "x3",
           1, "x4",
      )
      y <- tribble(
        ~key, ~val_y,
        1, "y1",
        2, "y2"
      )
      left_join(x, y, by = "key")
      
      # 2. Both tables have duplicate keys. This is usually an error because in neither table do the keys uniquely identify an observation. When you join duplicated keys, you get all possible combinations, the Cartesian product:
      
      x <- tribble(
        ~key, ~val_x,
        1, "x1",
        2, "x2",
        2, "x3",
        3, "x4"
      )
      y <- tribble(
        ~key, ~val_y,
        1, "y1",
        2, "y2",
        2, "y3",
        3, "y4"
      )
      left_join(x, y, by = "key")
 
  # 4.5 Defining the key columns
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    # The pairs of tables have always been joined by a single variable that was encoded by by = "key". You can use other values for by to connect the tables in other ways:
      
      # * The default, by = NULL, uses all variables that appear in both tables, the so called **natural** join.
        
        flights2 %>% 
          left_join(weather)
        
      # * A character vector, by = "x". This is like a natural join, but uses only some of the common variables. 
        
        flights2 %>% 
          left_join(planes, by = "tailnum")
        # Note that the year variables (which appear in both input data frames, but are not constrained to be equal) are disambiguated in the output with a suffix.
        
      # * A named character vector: by = c("a" = "b"). This will match variable a in table x to variable b in table y. The variables from x will be used in the output.
        
        flights2 %>% 
          left_join(airports, c("dest" = "faa"))

        flights2 %>%
          left_join(airports, c("origin" = "faa"))

  # 4.6 Exercises
  #~~~~~~~~~~~~~~~      
        
    # 1. Compute the average delay by destination, then join on the airports data frame so you can show the spatial distribution of delays. Here’s an easy way to draw a map of the United States:
      airports %>% 
        semi_join(flights, c("faa" = "dest")) %>% 
        ggplot(aes(lon, lat)) +
        borders("state") +
        geom_point() +
        coord_quickmap()
    # (Don’t worry if you don’t understand what semi_join() does — you’ll learn about it next.)
    # You might want to use the size or colour of the points to display the average delay for each airport.  
       
      avr_dest_delay <- 
        flights %>% 
        group_by(dest) %>% 
        summarise(delay = mean(arr_delay, na.rm = TRUE)) %>% 
        inner_join(airports, by = c(dest = "faa"))
      
      avr_dest_delay %>% 
        ggplot(aes(lon, lat, color = delay)) +
        borders("state") +
        geom_point() +
        coord_quickmap()
      
    # > 2. Add the location of the origin and destination (i.e. the lat and lon) to flights.
      
      airports_loc <- airports %>% 
        select(faa, lat, lon)
      
      flights %>% 
        select(year:day, hour, origin, dest) %>% 
        left_join(airports_loc, by = c("origin" = "faa")) %>% 
        left_join(airports_loc, by = c("dest" = "faa"), suffix = c("_origin", "_dest"))

    # > 3. Is there a relationship between the age of a plane and its delays?
      
      plane_cohorts <- inner_join(
        flights,
        select(planes, tailnum, plane_year = year),
        by = "tailnum"
      ) %>% 
        mutate(age = year - plane_year) %>% 
        filter(!is.na(age)) %>% 
        mutate(age = if_else(age > 25, 25L, age)) %>% # Since there are few planes older than 25 years, so I truncate age at 25 years.
        group_by(age) %>% 
        summarise(
          dep_delay_mean = mean(dep_delay, na.rm = TRUE),
          dep_delay_sd = sd(dep_delay, na.rm = TRUE), 
          arr_delay_mean = mean(arr_delay, na.rm = TRUE),
          arr_delay_sd = sd(arr_delay, na.rm = TRUE),
          n_arr_delay = sum(!is.na(arr_delay)),
          n_dep_delay = sum(!is.na(dep_delay))
        )
      
      ggplot(plane_cohorts, aes(x = age, y = dep_delay_mean)) +
        geom_point() +
        scale_x_continuous("Age of plane (years)", breaks = seq(0,30, by = 10)) +
        scale_y_continuous("Mean departure delay (minutes)")

      ggplot(plane_cohorts, aes(x = age, y = arr_delay_mean)) +
        geom_point() +
        scale_x_continuous("Age of Plane (years)", breaks = seq(0, 30, by = 10)) +
        scale_y_continuous("Mean Arrival Delay (minutes)")

      
    # > 4. What weather conditions make it more likely to see a delay?
      
      flights_weather <- 
        flights %>% 
        inner_join(weather, by = c(
          "origin" = "origin",
          "year" = "year",
          "month" = "month", 
          "day" = "day",
          "hour" = "hour"
        ))

      flights_weather %>% 
        group_by(precip) %>% 
        summarise(delay = mean(dep_delay, na.rm = TRUE)) %>% 
        ggplot(aes(x = precip, y = delay)) +
        geom_line() + geom_point()
      
    # > 5. What happened on June 13 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.
      
        # There was a large series of storms (derechos) in the southeastern US. The following plot show that the largest delays were in Tennessee (Nashville), the Southeast, and the Midwest, which were the locations of the derechos.
      
      flights %>% 
        filter(year == 2013, month == 6, day == 13) %>% 
        group_by(dest) %>% 
        summarise(delay = mean(arr_delay, na.rm = TRUE)) %>% 
        inner_join(airports, by = c("dest" = "faa")) %>% 
        ggplot(aes(y = lat, x = lon, size = delay, colour = delay)) +
        borders("state") +
        geom_point() +
        coord_quickmap() +
        scale_color_viridis_c()
      
  # 4.7 Other immplementations
    
    # base::merge()
      
      #          dplyr         |           merge
      #-------------------------------------------------------------------
            #inner_join(x,y)   | merge(x, y)
            #left_join(x,y)    | merge(x, y, all.x = TRUE)
            #right_join(x,y)   | merge(x, y, all.y = TRUE),
            #full_join(x,y)    | merge(x, y, all.x = TRUE, all.y = TRUE)

      
      #          dplyr           |           merge
      #-------------------------------------------------------------------
      # inner_join(x,y, by = "z")| SELECT * FROM x INNER JOIN y USING (z)
      # left_join(x,y, by = "z") | SELECT * FROM x LEFT OUTER JOIN y USING (z)
      # right_join(x,y, by = "z")| SELECT * FROM x RIGHT OUTER JOIN y USING (z)
      # full_join(x,y, by = "z") | SELECT * FROM x FULL OUTER JOIN y USING (z)
      
    # oining different variables between the tables, e.g. `inner_join(x, y, by = c("a" = "b"))` uses a slightly different syntax in SQL: `SELECT * FROM x INNER JOIN y ON x.a = y.b` .
      
      
# 5. Filtering joins
#~~~~~~~~~~~~~~~~~~~~
  
    # Affect the observations, not the variables. There are two types: 
      
      # * semi_join(x, y) keeps all observations in x that have a match in y.
      # * anti_join(x, y) drops all observations in x that have a match in y.
      
    # Semi-joins are useful for matching filtered summary tables back to the original rows. For example, imagine you’ve found the top ten most popular destinations:
      
      top_dest <- flights %>% 
        count(dest, sort = TRUE) %>% 
        head(10)
      top_dest
      
    # to find each flight that went to one of those destinations. You could construct a filter yourself:
      
      flights %>% 
        filter(dest %in% top_dest$dest)
      
    # But it’s difficult to extend that approach to multiple variables. For example, imagine that you’d found the 10 days with highest average delays.
      
      flights %>% 
        semi_join(top_dest)

    # Anti-joins are useful for diagnosing join mismatches. For example, when connecting flights and planes, you might be interested to know that there are many flights that don’t have a match in planes
      
      flights %>% 
        anti_join(planes, by = "tailnum") %>% 
        count(tailnum, sort = TRUE)

      
  # 5.1 Exercises  
      
    # > 1. What does it mean for a flight to have a missing `tailnum`? What do the tail numbers that don’t have a matching record in `planes` have in common? (Hint: one variable explains ~90% of the problems.)  
      
      # Flights that have a missing tailnum all have missing values of arr_time, meaning that the flight was canceled.
      
      flights %>% 
        filter(is.na(tailnum), !is.na(arr_time)) %>% 
        nrow()
      
      # Many of the tail numbers that don’t have a matching value in planes are registered to American Airlines (AA) or Envoy Airlines (MQ). The documentation for planes states
      
      flights %>% 
        anti_join(planes, by = "tailnum") %>% 
        count(carrier, sort = TRUE) %>% 
        mutate(p = n / sum(n))

      # However, not all tail numbers appearing inflights from these carriers are missing from the planes table. I don’t know how to reconcile this discrepancy.
      flights %>% 
        distinct(carrier, tailnum) %>% 
        left_join(planes, by = "tailnum") %>% 
        group_by(carrier) %>% 
        summarise(
          total_planes = n(),
          not_in_planes = sum(is.na(model))
        ) %>% 
        mutate(missing_pct = not_in_planes / total_planes) %>% 
        arrange(desc(missing_pct))
      
    # > 2. Filter flights to only show flights with planes that have flown at least 100 flights.  
      
      # I need to filter flights that are missing a tail number otherwise all flights missing a tail number will be treated as a single plane.
      
      planes_gte100 <- flights %>%
        filter(!is.na(tailnum)) %>%
        group_by(tailnum) %>%
        count() %>%
        filter(n >= 100)
      
      # Now, I will semi join the data frame of planes that have flown at least 100 flights to the data frame of flights to select the flights by those planes
      
      flights %>% 
        semi_join(planes_gte100, by = "tailnum")
      
      # This can also be answered with a grouped mutate.
      
      flights %>% 
        filter(!is.na(tailnum)) %>% 
        group_by(tailnum) %>% 
        mutate(n = n()) %>% 
        filter(n >= 100)
     
    # > 3. Combine fueleconomy::vehicles and fueleconomy::common to find only the records for the most common models.
      
      library(fueleconomy)
      vehicles %>% 
        semi_join(common, by = c("make", "model"))
      
      # Why does the above code join on make and model and not just model? It is possible for two car brands (make) to produce a car with the same name (model). In both the vehicles and common data we can find some examples. For example, “Truck 4WD” is produced by many different brands.
      
      vehicles %>% 
        distinct(model, make) %>% 
        group_by(model) %>% 
        filter(n() > 1) %>% 
        arrange(model)
   
      common %>% 
        distinct(model, make) %>% 
        group_by(model) %>% 
        filter(n() > 1) %>% 
        arrange(model)
      
      # If we were to merge these data on the model column alone, there would be incorrect matches.
      
    # > 4. Find the 48 hours (over the course of the whole year) that have the worst delays. Cross-reference it with the weather data. Can you see any patterns?
      
      # There are three concepts that need to be defined more precisely.
        # * hat is meant by “delay”? I will use departure delay. Since the weather data only contains data for the New York City airports, and departure delays will be more sensitive to New York City weather conditions than arrival delays.
        # * What is meant by “worst”? I define worst delay as the average departure delay per flight for flights scheduled to depart in that hour. For hour, I will use the scheduled departure time rather than the actual departure time. If planes are delayed due to weather conditions, the weather conditions during the scheduled time are more important than the actual departure time, at which point, the weather could have improved.
        # * What is meant by “48 hours over the course of the year”? This could mean two days, a span of 48 contiguous hours, or 48 hours that are not necessarily contiguous hours. I will find 48 not-necessarily contiguous hours. That definition makes better use of the methods introduced in this section and chapter.
        # * What is the unit of analysis? Although the question mentions only hours, I will use airport hours. The weather dataset has an observation for each airport for each hour. Since all the departure airports are in the vicinity of New York City, their weather should be similar, it will not be the same.
      
      worst_hours <- flights %>% 
        mutate(hour = sched_dep_time %/% 100) %>% 
        group_by(origin, year, month, day, hour) %>% 
        summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(desc(dep_delay)) %>% 
        slice(1:48)
      
      # Then I can use semi_join() to get the weather for these hours.
      
      weather_most_delayed <- semi_join(weather, worst_hours,
        by = c(
        "origin", "year",
        "month", "day", "hour"
        )
      )
      
      # For weather, I’ll focus on precipitation, wind speed, and temperature. I will display these in both a table and a plot.
      # Many of these observations have a higher than average wind speed (10 mph) or some precipitation. However, I would have expected the weather for the hours with the worst delays to be much worse.
      
      select(weather_most_delayed, temp, wind_speed, precip) %>% 
        print(n = 48)
      
      ggplot(weather_most_delayed, aes(x = precip, y = wind_speed, color = temp)) +
               geom_point()
    
    # > 5. What does anti_join(flights, airports, by = c("dest" = "faa")) tell you? What does anti_join(airports, flights, by = c("faa" = "dest")) tell you? 
      
      anti_join(flights, airports, by = c("dest" = "faa"))
      # returns the flights that went to an airport that is not in the FAA list of destinations. Since the FAA list only contains domestic airports, these are likely foreign flights.
      anti_join(airports, flights, by = c("faa" = "dest"))
      # returns the US airports that were not the destination of any flight in the data. Since the data contains all flights from New York City airports, this is also the list of US airports that did not have a nonstop flight from New York City in 2013.
      
    # > 6. You might expect that there’s an implicit relationship between plane and airline, because each plane is flown by a single airline. Confirm or reject this hypothesis using the tools you’ve learned above.
      
      # let’s check to see if there are any planes in the data flew for multiple airlines.
      
      # First, find all distinct airline, plane combinations.
      planes_carriers <- 
        flights %>% 
        filter(!is.na(tailnum)) %>% 
        distinct(tailnum, carrier)
      
      # The number of planes that have flown for more than one airline are those tailnum that appear more than once in the planes_carriers data.
      planes_carriers %>% 
        count(tailnum) %>% 
        filter(n > 1) %>% 
        nrow()

      # The names of airlines are easier to understand than the two-letter carrier codes. The airlines data frame contains the names of the airlines.
      carrier_transfer_tbl <- planes_carriers %>% 
        # keep only planes which have flown for more than one airlines
        group_by(tailnum) %>% 
        filter(n() > 1) %>% 
        # joinwith airlines to get airline names 
        left_join(airlines, by = "carrier") %>% 
        arrange(tailnum, carrier)

      carrier_transfer_tbl   
      
  
# 6. Join problems
#~~~~~~~~~~~~~~~~~~
      
  # Your own data is unlikely to be so nice, so there are a few things that you should do with your own data to make your joins go smoothly.
      
    # 1. Start by identifying the variables that form the primary key in each table. You should usually do this based on your understanding of the data, not empirically by looking for a combination of variables that give a unique identifier.  
      
    # 2. Check that none of the variables in the primary key are missing. If a value is missing then it can’t identify an observation!
      
    # 3. Check that your foreign keys match primary keys in another table. The best way to do this is with an `anti_join()`. It’s common for keys not to match because of data entry errors. Fixing these is often a lot of work. 
      
      # If you do have missing keys, you’ll need to be thoughtful about your use of inner vs. outer joins, carefully considering whether or not you want to drop rows that don’t have a match.
      
  # Be aware that simply checking the number of rows before and after the join is not sufficient to ensure that your join has gone smoothly. If you have an inner join with duplicate keys in both tables, you might get unlucky as the number of dropped rows might exactly equal the number of duplicated rows!
      
      
# 7. Set operations
#~~~~~~~~~~~~~~~~~~~~
      
  # Generally, I use these the least frequently, but they are occasionally useful when you want to break a single complex filter into simpler pieces. All these operations work with a complete row, comparing the values of every variable. These expect the x and y inputs to have the same variables, and treat the observations like sets:
    
    # * intersect(x, y): return only observations in both x and y.
    # * union(x, y): return unique observations in x and y.
    # * setdiff(x, y): return observations in x, but not in y.
      
      df1 <- tribble(
        ~x, ~y,
        1,  1,
        2,  1
      )
      df2 <- tribble(
        ~x, ~y,
        1,  1,
        1,  2
      )

      intersect(df1, df2)    
      union(df1, df2)      
      setdiff(df1, df2)      
      setdiff(df2, df1)      
      
      