#' expected.CM
#' 
#' Function that calculates the expected edge weight between each pair of nodes in each layer of a multilayer network.
#' @param adjacency: a list object whose tth entry is an adjacency matrix representing the tth layer of a multilayer network. 
#'
#' @keywords community detection, multilayer networks, configuration model, random graph models
#' @return 
#' \itemize{
#'      \item P: a list object whose tth entry is the expected adjacency matrix of the tth layer 
#' }
#'@references
#'\itemize{
#'     \item Wilson, James D., Palowitch, John, Bhamidi, Shankar, and Nobel, Andrew B. (2016) "Significance based 
#'     extraction in multilayer networks with heterogeneous community structure."
#' } 
#' @author James D. Wilson
#' @export 

expected.CM <- function(adjacency, directed = FALSE){
  #check that adjacency is a list object
  m <- max(adjacency[, 3]) #max of the layer index
  n <- length(unique(c(adjacency[, 1], adjacency[, 2])))
  
  P <- list()
  for(i in 1:m){
    #may have to make sure that the appropriate number of nodes are specified
    graph <- graph_from_edgelist(as.matrix(subset(as.data.frame(adjacency), layer == i))[, 1:2], 
                                 directed = directed)
    degrees <- degree(graph)
    d.tot <- sum(degrees)
    expected <- degrees%*%t(degrees) / d.tot #expected adjacency matrix under configuration model
    #save the expected value as a weighted igraph object
    P[[i]] <- graph_from_adjacency_matrix(expected, mode = "undirected", weighted = TRUE)
  }                                       
  return(P)
}