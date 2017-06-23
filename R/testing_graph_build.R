
library(tidyverse)
library(igraph)


## Simulation starts with a cluster
g <- make_empty_graph(n = 3, directed = F) %>%
  add_edges(c(1,2, 2,3, 3,1))


tot_nodes <- 500
edges_to_attach <- 2
alpha <- 2
init_graph_length <- length(V(g))
graph_list <- vector(mode = "list", length = (tot_nodes - init_graph_length))
for(node_index in (init_graph_length+1):tot_nodes){
  ## Cycles through desired number of nodes, starting with index of current graph + 1
  ## This index should always start at 4, because graph should initialize with 3, but
  ## Just in case changes, made it flexible
  
  ## First sample which edges the new node should be attached to
  new_edges <- sample(x = V(g), size = edges_to_attach, prob = transitivity(g, type = "local")^alpha, replace=FALSE)
  
  ## Add vertex and then add the number of edges
  
  g <- g %>% add_vertices(nv = 1) %>%
    add_edges(c(rbind(rep(node_index, length(new_edges)), new_edges)))
  graph_list[[node_index-init_graph_length]] <- g
}


get_modularity <- function(graph){
  ## Returns the modularity for a single graph
  modularity(x = graph, membership = membership(cluster_louvain(graph)))
}

get_clustering <- function(graph){
  ## Returns the modularity for a single graph
  modularity(x = graph, membership = membership(cluster_louvain(graph)))
  transitivity(graph)
}

graph_list %>% purrr::map(~get_modularity(.x)) %>% unlist() -> test

graph_list %>% purrr::map(~transitivity(.x)) %>% unlist() -> test

plot(test)




