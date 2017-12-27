#' multilayer.extraction
#' 
#'Function that identifies statistically significant vertex-layer communities in multilayer networks.
#' @param adjacency: a list object whose tth entry is an adjacency matrix representing the tth layer of a multilayer network. 
#' @param seed: seed for reproducibility. The initial neighborhoods that act as seeds for the multilayer extraction algorithm
#' are random in this algorithm; hence, a seed will need to be set for reproducible results. Default is 123. 
#' @param min.score: the minimum score allowable for an extracted community. Default is 0.
#' @param prop.sample: the proportion of vertices one would like to search over for initialization. Example: prop.sample = 0.05
#' specifies that one will obtain 0.05 * n randomly selected vertex neighborhoods for initialization, where n = number of nodes in each layer.
#' Default is 0.05.
#' @keywords community detection, multilayer networks, configuration model, random graph models
#' @return A MultilayerCommunity object, which is a list containing the following objects
#' \itemize{
#'      \item Community.List: a list of vertex-layer communities extracted from the algorithm
#'      \item Diagnostics: the diagnostics associated with each extracted community. This is a summary of 
#'      each community, and includes for each level of overlap parameter Beta the mean score, and the total number 
#'      of communities. This is used for determining the overall number of communities in a multilayer network. 
#' }
#'@references
#'\itemize{
#'     \item Wilson, James D., Palowitch, John, Bhamidi, Shankar, and Nobel, Andrew B. (2017) "Significance based 
#'     extraction in multilayer networks with heterogeneous community structure." Journal of Machine Learning Research
#' } 
#' @author James D. Wilson
#' @export 
#' 
#' 
multilayer.extraction = function(adjacency, seed = 123, min.score = 0, prop.sample = 0.05, directed = c(FALSE, TRUE)){
  #adjacency should be an edgelist with three columns - node1, node2, layer
  #layer should be numbered with integers
  #each network has the same number of nodes
  #nodes are indexed by the integers starting from 1
  
  m <- max(adjacency[, 3]) #max of the layer index
  n <- length(unique(c(adjacency[, 1], adjacency[, 2])))
  directed <- directed[1]
  #Calculate the modularity matrix
  print(paste("Estimation Stage"))
  
  mod.matrix <- multilayer.modularity.matrix(adjacency) 
  
  #Initialize the communities
  print(paste("Initialization Stage"))
  
  for(i in 1:m){
    if(i == 1){
      graph <- graph_from_edgelist(as.matrix(subset(as.data.frame(adjacency), layer == i))[, 1:2], 
                                   directed = directed)
      initial.set <- initialization(graph, prop.sample, m, n)
    }else{
      graph <- graph_from_edgelist(as.matrix(subset(as.data.frame(adjacency), layer == i))[, 1:2], 
                                   directed = directed)
      initial.set <- Map(c, initial.set, initialization(graph, prop.sample, m, n))
    }
  }
  
  #Search Across Initial sets
  print(paste("Search Stage"))
  
  cat(paste("Searching over", length(initial.set[[1]]), "seed sets \n"))
  Results.temp <- list()
  K <- length(initial.set[[1]])
  
  #detectCores detects the number of cores available on your instance
  
  registerDoParallel(detectCores())  
  Results.temp <- foreach(i=1:K,.packages="MultilayerExtraction") %dopar% {
    starter <- list()
    starter$vertex.set <- as.numeric(initial.set$vertex.set[[i]])
    #if the initial neighborhood is of length 1, add a random vertex
    if(length(starter$vertex.set) < 2){
      starter$vertex.set <- c(starter$vertex.set, setdiff(1:n, starter$vertex.set)[1])
    }
    starter$layer.set <- as.numeric(initial.set$layer.set[[i]])
    single.swap(starter, adjacency, mod.matrix, m, n)
  } 
  
  #Cleanup the results: Keep the unique communities
  print(paste("Cleaning Stage"))
  
  if(length(Results.temp) < 1){return("No Community Found")}
  
  Scores = rep(0, length(Results.temp))
  
  for(i in 1:length(Results.temp)){
    if(length(Results.temp[[i]]$B) == 0){Scores[i] = -1000}
    if(length(Results.temp[[i]]$B) > 0){
      Scores[i] = Results.temp[[i]]$Score
    }
  }
  
  Scores = round(Scores, 5)
  #keep only unique communities with score greater than threshold
  indx = which(!duplicated(Scores) == TRUE)
  indx.2 = which(Scores > min.score)
  Results2 = Results.temp[intersect(indx, indx.2)]
  if(length(Results2) == 0){
    Results = NULL
    return(Object = NULL)
  }
  if(length(Results2) > 0){
    betas = seq(0.01, 1, by = 0.01)
    Results3 = list()
    Number.Communities = rep(0, length(betas))
    Mean.Score = rep(0, length(betas))
    for(i in 1:length(betas)){
      temp = cleanup(Results2, betas[i])
      Results3[[i]] = list(Beta = betas[i], Communities = temp$Communities)
      Mean.Score[i] = temp$Mean.Score
      Number.Communities[i] = length(temp$Communities)
    }
  }
  
  Z = data.frame(Beta = betas, Mean.Score = Mean.Score, Number.Communities = Number.Communities)
  Object = list(Community.List = Results3, Diagnostics = Z)
  class(Object) = "MultilayerCommunity"
  return(Object)
}


#######################################################################
##Swapping functions##
####Function for determining which vertex/layer should be swapped
swap.candidate = function(set, changes, add, remove, score.old){
  #If there are only some to be added
  if(length(remove) == 0 & length(add) > 0){
    if(is.na(add) == FALSE){
      if(changes[add] > 0){
        set.new <- union(set, add)
        score.old <- score.old + changes[add]
      }
      if(changes[add] < 0){
        set.new <- set
        return(list(set.new = set.new, score.old = score.old))
      }
    }
  }
  
  #If there are only some to removed
  if(length(add) == 0 & length(remove) > 0){
    if(is.na(remove) == FALSE){
      if(changes[remove] > 0){
        set.new <- setdiff(set,remove)
        score.old <- score.old + changes[remove]
        return(list(set.new = set.new, score.old = score.old))
      }
      if(changes[remove] < 0){
        set.new <- set
        return(list(set.new = set.new, score.old = score.old))
      }
    }
  }
  
  #If there are some to removed and some to be added
  if(length(add) > 0 & length(remove) > 0){
    if(changes[remove] < 0 & changes[add] < 0){
      set.new <- set
      return(list(set.new = set.new, score.old = score.old))
    }
    if(changes[remove] > changes[add] & changes[remove] > 0){
      set.new <- setdiff(set, remove)
      score.old <- score.old + changes[remove]
      return(list(set.new = set.new, score.old = score.old))
    }
    if(changes[remove] < changes[add] & changes[add] > 0){
      set.new <- union(set, add)
      score.old <- score.old + changes[add]
      return(list(set.new = set.new, score.old = score.old))
    }
  }
  
  #if there are none to be added nor removed
  if(length(add) == 0 & length(remove) == 0){
    return(list(set.new = set, score.old = score.old))
  }
}

######Choosing which layer to swap one at a time######
swap.layer = function(adjacency, mod.matrix, layer.set, vertex.set, score.old, m, n){

  if(length(layer.set) == 0){
    print('No Community Found')
    return(NULL)
  }
  
  changes <- layer.change(adjacency, mod.matrix, layer.set, vertex.set, score.old, m, n)
  changes[which(is.null(changes) == TRUE)] <- 0
  changes[which(is.na(changes) == TRUE)] <- 0
  
  outside.candidate <- which.max(changes[setdiff(1:m, layer.set)]) #which layer should we add?
  l.add <- setdiff(1:m, layer.set)[outside.candidate]
  
  inside.candidate <- which.max(changes[layer.set]) #which layer should we remove?
  l.sub <- layer.set[inside.candidate]
  
  #Make the swap
  results <- swap.candidate(layer.set, changes, l.add, l.sub, score.old)
  layer.set.new <- results$set.new
  score.old <- results$score.old
  
  return(list(layer.set.new = layer.set.new, score.old = score.old)) 
}

#######################################################################
######Choosing which vertex to swap one at a time######
swap.vertex = function(adjacency, mod.matrix, layer.set, vertex.set, score.old, m, n){
  
  if(length(layer.set) == 0){
    print('No Community Found')
    return(NULL)
  }
  
  #swap decision
  if(length(vertex.set) < 5){
    print('No Community Found')
    return(NULL)
  }
  
  if(length(vertex.set) == n){
    print('No Community Found')
    return(NULL)
  }
  changes = vertex.change(adjacency, mod.matrix, layer.set, vertex.set, score.old, m, n)
  
  #Get candidates
  outside.candidate <- which.max(changes[setdiff(1:n, vertex.set)])[1] 
  u.add <- setdiff(1:n, vertex.set)[outside.candidate]
  
  inside.candidate <- which.max(changes[vertex.set])
  u.sub <- vertex.set[inside.candidate]
  
  #Make the swap
  results <- swap.candidate(vertex.set, changes, u.add, u.sub, score.old)
  return(list(B.new = results$set.new, score.old = results$score.old)) 
}
#######################################################################
#Inner function for a single swap inside the function for Multilayer.Extraction
#Note: check the names of initial set
single.swap = function(initial.set, adjacency, mod.matrix, m, n){
  
  #initialize vertex.set and layer.set
  B.new <- initial.set$vertex.set
  I.new <- initial.set$layer.set
  score.old <- score(mod.matrix, vertex.set = B.new, layer.set = I.new, n)
  
  iterations <- 1
  B.fixed <- B.new + 1
  I.fixed <- I.new + 1
  
  #main loop
  while(length(intersect(B.fixed, B.new)) < max(length(B.fixed), length(B.new)) | 
        length(intersect(I.fixed, I.new)) < max(length(I.fixed), length(I.new))){
    
    if(length(B.new) < 2 | length(I.new) < 1){
      print('No community found')
      return(NULL)
    }
    
    #seems redundant, check...
    B.fixed <- B.new
    I.fixed <- I.new
    
    B <- B.new
    I <- I.new + 1
    
    #update layer set
    if(m > 1){
      while(length(intersect(I.new, I)) < max(length(I.new), length(I))){
        
        I <- I.new
        results <- swap.layer(adjacency, mod.matrix, I, B, score.old, m, n)
        I.new <- results$layer.set.new
        score.old <- results$score.old
      }
    }
    if(m == 1){
      I.new <- 1
    }
    
    #update vertex set
    B <- B - 1
    B.new <- B + 1
    
    while(length(intersect(B.new, B)) < max(length(B.new), length(B))){
      B <- B.new
      results <- swap.vertex(adjacency, mod.matrix, I.new, B, score.old, m, n)
      
      B.new <- results$B.new
      score.old <- results$score.old
    }
  }
  return(list(B = sort(B.new), I = sort(I.new), Score = score.old))
}

######Effect on score when adding or subtracting a layer#######
layer.change = function(adjacency, mod.matrix, layer.set, vertex.set, score.old, m, n){
  
  indx <- setdiff(1:m, layer.set) #which layers are not in the current set
  score.changes <- rep(0, m)
  
  for(i in 1:m){
    if(i %in% indx){
      score.changes[i] <- score(mod.matrix, vertex.set = 
                                  vertex.set, layer.set = union(layer.set, i), n) - score.old
    }
    if(i %in% indx == FALSE){
      score.changes[i] <- score(mod.matrix, vertex.set = 
                                  vertex.set, layer.set = setdiff(layer.set, i), n) - score.old
    }
  }
  return(score.changes)
}

######Effect on score when adding or subtracting a vertex#######
vertex.change = function(adjacency, mod.matrix, layer.set, vertex.set, score.old, m, n){
  
  indx <- setdiff(1:n, vertex.set)
  score.changes <- rep(0, n)
  
  #the following can also be parallelized!
  for(i in 1:n){
    if(i %in% indx){
      score.changes[i] <- score(mod.matrix, vertex.set = 
                                  union(vertex.set, i), layer.set = layer.set, n) - score.old
    }
    if(i %in% indx == FALSE){
      score.changes[i] <- score(mod.matrix, vertex.set = 
                                  setdiff(vertex.set, i), layer.set = layer.set, n) - score.old
    }
  }
  
  return(score.changes)
}
