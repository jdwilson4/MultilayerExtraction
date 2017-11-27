#Function to convert a list of adjacency matrices to an edgelist
#requires igraph
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