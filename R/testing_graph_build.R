rm(list=ls())
library(tidyverse)
library(igraph)

sapply(c("R/build_network_fxns.R", 
         "R/network_analysis_fxns.R", 
         "R/network_plotting_fxns.R"), FUN = source)

## Simulation starts with a cluster
g <- make_empty_graph(n = 3, directed = F) %>%
  add_edges(c(1,2, 2,3, 3,1))


## Build by clustering coefficient
cluster_graphs <- build_network(graph = g, build_fxn = cluster_build)

plot(cluster_graphs[[50]], vertex.label = NA, vertex.size=2)
plot_net_feature(gl = cluster_graphs, feature_fxn = get_modularity)
plot_net_feature(gl = cluster_graphs, feature_fxn = transitivity)

## Build by degree (power law)
degree_graphs <- build_network(graph = g, build_fxn = degree_build)
plot(degree_graphs[[50]], vertex.label = NA, vertex.size=2)
plot_net_feature(gl = degree_graphs, feature_fxn = get_modularity)
plot_net_feature(gl = degree_graphs, feature_fxn = transitivity)

## Build by eigenvector centrality
degree_graphs <- build_network(graph = g, build_fxn = eigen_build)
plot(degree_graphs[[50]], vertex.label = NA, vertex.size=2)
plot_net_feature(gl = degree_graphs, feature_fxn = get_modularity)
plot_net_feature(gl = degree_graphs, feature_fxn = transitivity)
















# 
# 
# ############# Test out connecting to neighbor instead of within community
# g <- make_empty_graph(n = 3, directed = F) %>%
#   add_edges(c(1,2, 2,3, 3,1))
# 
# tot_nodes <- 500
# edges_to_attach <- 2
# alpha <- 2
# init_graph_length <- length(V(g))
# graph_list <- vector(mode = "list", length = (tot_nodes - init_graph_length))
# p <- .7
# 
# # At each step, save the graph  in a list
# for(node_index in (init_graph_length+1):tot_nodes){
#   ## Cycles through desired number of nodes, starting with index of current graph + 1
#   ## This index should always start at 4, because graph should initialize with 3, but
#   ## Just in case changes, made it flexible
#   
#   ## First sample which edges the new node should be attached to
#   new_edges <- sample(x = V(g), size = edges_to_attach, prob = transitivity(g, type = "local")^alpha, replace=FALSE)
#   
#   if(runif(1) < p){
#     new_edges <- c(new_edges[1], sample(neighbors(graph = g, v = new_edges[1]), size = 1 ))
#     # new_edges[2] <- sample(x = V(g), size = edges_to_attach, prob = transitivity(g, type = "local")^alpha, replace=FALSE)
#   }
#   
#   ## Add vertex and then add the number of edges
#   
#   g <- g %>% add_vertices(nv = 1) %>%
#     add_edges(c(rbind(rep(node_index, length(new_edges)), new_edges)))
#   graph_list[[node_index-init_graph_length]] <- g
# }
# 
# 
# plot(cluster_louvain(graph_list[[50]]), graph_list[[50]])
# plot(cluster_louvain(graph_list[[250]]), graph_list[[250]])
# plot(cluster_louvain(graph_list[[400]]), graph_list[[400]])
# 
# 
# graph_list %>% purrr::map(~diameter(.x)) %>% unlist() -> diameter
# plot(diameter, type="l")
# 
# 
# ## Measures to test p168 Newman book:
# # Degree
# # Eigenvector centrality
# # Katz centrality
# # Pagerank
# # hubs and authorities
# # closeness centrality
# # betweeness centrality
# # Cliques, plexes, cores
# # transitivity
# # components
# # reciprocity
# # similarity
# # 
# 
