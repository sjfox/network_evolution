##################################################
## Functions for returning network characteristics
##################################################

get_modularity <- function(g, clustering_alg = cluster_louvain){
  # Function to summarize modularity for each graph 
  # Modularity is a metric for how the graph is clustered into communities and how connected the communities are
  # Modularity decreases as the connectections between communities increase
  # Parameters
  #   g = graph to summarize
  #   clustering_alg - any of igraphs community detection algorithms (defaults to cluster_louvain)
  
  
  modularity(x = g, membership = membership(clustering_alg(g)))
}

