# the github page https://github.com/bergant/datamodelr#model-diagram-of-interconnected-data-frames

library(datamodelr)

# Usage
#~~~~~~~~~~


      


      
      
# Model diagram of interconnected data frames
library(nycflights13)      
dm_f <- dm_from_data_frames(flights, airlines, weather, airports, planes)  
# Create plot
graph <- dm_create_graph(dm_f, rankdir = "BT", col_attr = c("column", "type"))
dm_render_graph(graph)

# Add referencesand primary keys:
dm_f <- dm_add_references(
  dm_f,
  
  flights$carrier == airlines$carrier,
  flights$origin == airports$faa,
  flights$dest == airports$faa,
  flights$tailnum == planes$tailnum,
  weather$origin == airports$faa
)
graph <- dm_create_graph(dm_f, rankdir = "BT", col_attr = c("column", "type"))
dm_render_graph(graph)