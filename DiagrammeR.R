
# The github of the package https://github.com/rich-iannone/DiagrammeR

library(DiagrammeR)

# Graph basics
#~~~~~~~~~~~~~~

a_graph <- 
  create_graph() %>% 
  add_node() %>% 
  add_node() %>% 
  add_edge(from = 1, to = 2)

render_graph(a_graph)

# We can take away an edge
b_graph <- a_graph %>% delete_edge(from = 1, to = 2)
render_graph(b_graph) 

# Add a node to the graph while, at the same time, defining edges to or from existing nodes
c_graph <- b_graph %>% add_node(from = 1, to = 2)
render_graph(c_graph)

#Viewing the graph object in the console will provide some basic information about the graph and some pointers on where to get additional information.
c_graph
