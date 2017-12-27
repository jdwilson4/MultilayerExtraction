#' score
#' 
#'Function that calculates the score of a multilayer vertex - layer community
#' @param adjacency: a list object whose tth entry is an adjacency matrix representing the tth layer of a multilayer network. 
#' @param expected: the expected value of the connectivity in the multilayer network adjacency. This can be calculated using the 
#' expected.CM function in this package.
#' @param vertex.set: a numeric specifying the nodes within the multilayer community of interest.
#' @param layer.set: a numeric specifying the layers within the multilayer community of interest.
#' @keywords community detection, multilayer networks, configuration model, random graph models
#' @return 
#' \itemize{
#'      \item score: the score of the multilayer community
#'      }

#'@references
#'\itemize{
#'     \item Wilson, James D., Palowitch, John, Bhamidi, Shankar, and Nobel, Andrew B. (2016) "Significance based 
#'     extraction in multilayer networks with heterogeneous community structure."
#' } 
#' @author James D. Wilson
#' @export 
#' 
#' 

score = function(mod.matrix, vertex.set, layer.set, n){
  
  if(length(layer.set) < 1 || length(vertex.set) < 1){
    return(obs.score = 0)
  }
  
  if(length(layer.set) == 1){
    super.mod <- mod.matrix[[layer.set]] #just a single igraph object
  }
  
  if(length(layer.set) > 1){
    #merge the modularity graphs
    super.mod <- graph.empty(n = n, directed = FALSE)
    for(j in layer.set){
      super.mod <- igraph::union(super.mod, mod.matrix[[j]]) #take union of all networks in the layer.set
    }
  }
    #take sub-graph of the modularity matrix
    super.mod.subgraph <- induced_subgraph(super.mod, v = vertex.set)
    
    #get the edge weights all together
    edge.weights <- as.data.frame(get.edge.attribute(super.mod.subgraph))
    edge.weights[is.na(edge.weights)] <- 0
    modularity.score <- rowSums(edge.weights) #sum across vertices first
    modularity.score[which(modularity.score < 0)] <- 0 #only keep positive values
    
    tot.mod <- sum(modularity.score)
    obs.score <- (tot.mod)^2 / (n^2*choose(length(vertex.set), 2)*(length(layer.set)))
    
    return(obs.score)
  }
