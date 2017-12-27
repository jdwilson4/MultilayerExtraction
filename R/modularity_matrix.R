#' multilayer.modularity.matrix
#' 
#'Function that calculates the modularity matrix for each layer in a multilayer network
#' @param adjacency: an edgelist. Use adjacency.to.edgelist function to obtain this object
#' @keywords community detection, multilayer networks, configuration model, random graph models
#' @return mod.matrix: an igraph object representing the modularity matrix for each layer 
#'@references
#'\itemize{
#'     \item Wilson, James D., Palowitch, John, Bhamidi, Shankar, and Nobel, Andrew B. (2017) "Significance based 
#'     extraction in multilayer networks with heterogeneous community structure." Journal of Machine Learning Research
#' } 
#' @author James D. Wilson
#' @export 
#' 
#' 

multilayer.modularity.matrix <- function(adjacency, directed = FALSE){
  m <- max(adjacency[, 3]) #max of the layer index
  n <- length(unique(c(adjacency[, 1], adjacency[, 2])))
  mod.matrix <- list()
  for(i in 1:m){
    #may have to make sure that the appropriate number of nodes are specified
    graph <- graph_from_edgelist(as.matrix(subset(as.data.frame(adjacency), layer == i))[, 1:2], 
                                 directed = directed)
    graph <- add_vertices(graph, n - length(V(graph)))
    temp <- modularity_matrix(graph, membership = rep(1, n))
    #store as an igraph object with weights
    mod.matrix[[i]] <- graph_from_adjacency_matrix(temp, mode = "undirected", weighted = TRUE)
  }     
  return(mod.matrix)
}