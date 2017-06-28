#####################################
## Functions for plotting network/network characteristics
#####################################


plot_net_feature <- function(gl, feature_fxn, ...){
  ## Generic function to plot network evolution through time
  ## Parameters
  ##    gl = list of igraphs generated from building
  ##    feature_fxn = function that takes a single graph and gives a single numeric value
  ##    ... further arguments for the feature_fxn
  gl %>% purrr::map(~feature_fxn(.x, ...)) %>% unlist() %>%
    plot(type="l")
}


