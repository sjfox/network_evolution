#####################################
## Functions for building networks
#####################################

build_network <- function(graph, build_fxn, thin = 10, n = 500, ...){
  ## Function for building networks 
  ## Each iteration of building increases the graph size by 1
  ## Parameters
  ##    graph must begin with the desired starting graph
  ##    build_fxn must be a function that takes in a graph and returns the edges that new node connects to
  ##    thin describes how many iterations should pass before the graph is saved
  ##    n is the final desired size of the graph
  ##    ... describes all parameters that can be sent to the build_function
  ## Output
  ##    A list of graphs generated from the creation process
  
  init_graph_length <- length(V(g))
  if(init_graph_length == 0){
    stop("Must start with a graph that has at least one node")
  } 
  
  ## graph_list of length that accounts for the thinning
  ## Basically want to save every 10 iterations, but not necessarily from starting point (so use floor)
  ## e.g. start with 3 nodes, and want 500, saves on 10, 20, 30 ... instead of 3, 13, 23 ...
  graph_list <- vector(mode = "list", length = floor( (n - init_graph_length)/thin))
  
  for(node_index in (init_graph_length+1):n){
    ## Adds one node at a time, based on the specified function
    ## Sends the network from the previous iteration and any specific parameters
    new_edges   <- build_fxn(g = graph, ...)
    
    ## Adds vertex to graph, with new edges specified
    graph <- graph %>% add_vertices(nv = 1) %>%
      add_edges(c(rbind(rep(node_index, length(new_edges)), new_edges)))
    
    ## Saves the result if you're on the proper step
    if(node_index %% thin == 0){
      graph_list[[node_index/thin]] <- graph
    }
  }
  
  return(graph_list)
}
  
cluster_build <- function(g, alpha = 2, edges = 2){
  ## Clustering building algorithm based on bagrow/brockmann paper
  ##  alpha - exponent for clustering weighting
  ##  edges - total number of edges to attach for each new node
  sample(x = V(g), 
         size = edges, 
         prob = transitivity(g, type = "local")^alpha, 
         replace=FALSE)
}

degree_build <- function(g, alpha = 2, edges = 2){
  ## Clustering building algorithm based on bagrow/brockmann paper
  ##  alpha - exponent for degree weighting
  ##  edges - total number of edges to attach for each new node
  sample(x = V(g), 
         size = edges, 
         prob = degree(g)^alpha, 
         replace=FALSE)
}


