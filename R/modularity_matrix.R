#modularity function
#for each observed graph in an edgelist, we will calculate the modularity matrix and store it as a list
#object. Then, we can easily calculate scores from this. Input is an edge list, the output will 
#be a list of igraph objects

modularity.matrix <- function(adjacency, directed = FALSE){
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