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

score = function(adjacency, expected, vertex.set, layer.set){
  
  #check that input arguments are appropriately defined
  if(class(adjacency) != "list"){
    adjacency <- list(adjacency)
  }
  
  if(class(expected) != "list"){
    expected <- list(expected)
  }
  if(length(layer.set) < 1 || length(vertex.set) < 1){
    return(obs.score = 0)
  }
  
  if(length(layer.set) == 1){
    adj.sum <- adjacency[[layer.set]]
    exp.sum <- expected[[layer.set]]
  }
  
  if(length(layer.set) > 1){
    adj.sum <- Reduce('+', adjacency[layer.set])
    exp.sum <- Reduce('+', expected[layer.set])
  }
  
  D.B <- matrix(adj.sum[vertex.set, vertex.set], ncol = 1)
  P.vec <- matrix(exp.sum[vertex.set, vertex.set], ncol = 1)
  
  
  #calculate the modularity score and set all negative values to 0
  modularity.score <- D.B - P.vec
  modularity.score[which(modularity.score < 0)] <- 0
  
  #calculate the score of the community
  tot.mod <- sum(modularity.score)
  obs.score <- 2*(tot.mod)^2 / (choose(length(vertex.set), 2)*(length(layer.set)))
  
  return(obs.score)
}