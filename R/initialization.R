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

initialization = function(adjacency, prop.sample){
  
  if(class(adjacency) != "list"){
    adjacency <- list(adjacency)
  }
  
  m <- length(adjacency) #total number of layers
  n <- dim(adjacency[[1]])[1] #total number of vertices
  
  if(m == 1){
    adj.sum <- adjacency[[1]]
  }
  
  if(m > 1){
    adj.sum = Reduce("+", adjacency[1:m])
  }
  
  #mean.connection = mean(as.matrix(adj.sum))
  
  thresh = function(x, m){
    #random choice of layer set
    layer.set = sample(1:m, ceiling(m/2))
    median.connection <- median(x) #mean connection of the given vertex
    #vertex set is for those with higher than median connection
    vertex.set <- which(x > median.connection)
    return(list(vertex.set = vertex.set, layer.set = layer.set))
  }
  
  neighborhoods <- apply(adj.sum, 1, thresh, m)
  keep.sample <- sample(1:n, ceiling(prop.sample*n)) #keep a random sample of the neighborhoods
  neighborhoods <- neighborhoods[keep.sample]
  return(neighborhoods)
}