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

expected.CM <- function(adjacency){
  #check that adjacency is a list object
  if(class(adjacency) != "list"){
    adjacency <- list(adjacency)
  }
  m <- length(adjacency) #number of layers
  P <- list()
  for(i in 1:m){
    degrees <- matrix(rowSums(as.matrix(adjacency[[i]])), ncol = 1)
    d.tot <- sum(degrees)
    expected <- degrees%*%t(degrees) / d.tot #expected under configuration model
    P[[i]] <- expected
  }
  return(P)
}