#' initialization
#' 
#' Function that generates randomly selected neighborhood vertex-layer sets to begin the 
#' multilayer.extraction algorithm.
#' @param adjacency: a list object whose tth entry is an adjacency matrix representing the tth layer of a multilayer network. 
#' @param prop.sample: the proportion of vertices one would like to search over for initialization. Example: prop.sample = 0.05
#' specifies that one will obtain 0.05 * n randomly selected vertex neighborhoods for initialization, where n = number of nodes in each layer.
#'  
#' @keywords community detection, multilayer networks, configuration model, random graph models
#' @return 
#' \itemize{
#'      \item neighborhoods: a list object of length prop.sample * n, where each entry contains
#'      a vertex set and layer set from which multilayer.extraction can be run.
#' }
#' @details A neighborhood of vertex u is defined as the collection of vertices that 
#' have higher than the mean connectivity of vertex u, when aggregated across layers. The 
#' chosen layer set is a random sample of size m/2. 
#'@references
#'\itemize{
#'     \item Wilson, James D., Palowitch, John, Bhamidi, Shankar, and Nobel, Andrew B. (2016) "Significance based 
#'     extraction in multilayer networks with heterogeneous community structure."
#' } 
#' @author James D. Wilson
#' @export 

#take neighborhoods from each layer
initialization = function(adjacency, prop.sample, m, n){
  layer.set = replicate(n = ceiling(prop.sample*n), sample(1:m, ceiling(m/2)), simplify = FALSE)
  neighborhoods <- neighborhood(adjacency, order = 1)
  keep.sample <- sample(1:n, ceiling(prop.sample*n)) #keep a random sample of the neighborhoods
  neighborhoods <- neighborhoods[keep.sample]
  return(list(vertex.set = neighborhoods, layer.set = layer.set))
}