#' adjacency.to.edgelist
#' 
#'Function that converts a list of adjacency matrices to an edgelist. 
#' @param adjacency: a list object whose tth entry is an adjacency matrix representing the tth layer of a multilayer network. 
#' @param mode: directed or undirected 
#' @param weighted: currently not functioning. Coming in later version.
#' @keywords community detection, multilayer networks, configuration model, random graph models
#' @return edgelist: a matrix with three columns representing edge connections- node1, node2, layer 

#'@references
#'\itemize{
#'     \item Wilson, James D., Palowitch, John, Bhamidi, Shankar, and Nobel, Andrew B. (2017) "Significance based 
#'     extraction in multilayer networks with heterogeneous community structure." Journal of Machine Learning Research
#' } 
#' @author James D. Wilson
#' @export 
#' 
#' 
adjacency.to.edgelist <- function(adjacency, mode = c("undirected", "directed"), weighted = NULL){
  if(class(adjacency) != "list"){
    adjacency <- list(adjacency)
  }
  mode <- mode[1]
  m <- length(adjacency)
  edgelist <- c(0, 0, 0)
  if(mode == "undirected"){
    directed <- FALSE}else{
      directed <- TRUE
    }
  
  for(i in 1:m){
    #first convert to an igraph object
    temp.graph <- graph.adjacency(as.matrix(adjacency[[i]]), mode = mode, weighted = weighted)
    
    #elminate multiple edges
    temp.graph2 <- simplify(temp.graph)
    
    
    #store in an edgelist
    edgelist <- rbind(edgelist, cbind(get.edgelist(temp.graph2), i))
  }
  edgelist <- edgelist[-1, ]
  colnames(edgelist) <- c("node1", "node2", "layer")
  return(edgelist)
}