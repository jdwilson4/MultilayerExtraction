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
#'     \item Wilson, James D., Palowitch, John, Bhamidi, Shankar, and Nobel, Andrew B. (2016) "Significance based 
#'     extraction in multilayer networks with heterogeneous community structure."
#' } 
#' @author James D. Wilson
#' @export 
#' 
#' 
multilayer.extraction = function(adjacency, seed = 123, min.score = 0, prop.sample = 0.05){
  #check to see if adjacency is a list object
  if(class(adjacency) != "list"){
    adjacency <- list(adjacency)
  }
  m <- length(adjacency)
  n <- dim(adjacency[[1]])[1]
  
  #Estimate the multilayer configuration model
  print(paste("Estimation Stage"))
  
  expected <- expected.CM(adjacency)
  
  #Initialize the communities
  print(paste("Initialization Stage"))
  
  initial.set = initialization(adjacency, prop.sample)
  
  #Search Across Initial sets
  print(paste("Search Stage"))
  
  cat(paste("Searching over", length(initial.set), "seed sets \n"))
  Results.temp <- list()
  K <- length(initial.set)
  #note: we can parallelize this part of the search!
  registerDoParallel(detectCores())  ###### detectCores will automatically place the number of cores your computer has.
  
  Results.temp <- foreach(i=1:K,.packages="MultilayerExtraction") %dopar% {
    single.swap(initial.set[[i]], adjacency, expected)} 
  
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
  
  Scores = round(Scores, 2)
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
    Number.Communities = rep(0,length(betas))
    Mean.Score = rep(0,length(betas))
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
      set.new <- setdiff(set,remove)
      score.old <- score.old + changes[remove]
      return(list(set.new = set.new, score.old = score.old))
    }
    if(changes[remove] < changes[add] & changes[add] > 0){
      set.new <- union(set,add)
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
swap.layer = function(adjacency, expected, layer.set, vertex.set, score.old){
  
  if(class(adjacency) != "list"){
    adjacency <- list(adjacency)
  }
  if(class(expected) != "list"){
    expected <- list(expected)
  }
  
  m <- length(adjacency)
  n <- dim(adjacency[[1]])[1]
  
  
  if(length(layer.set) == 0){
    print('No Community Found')
    return(NULL)
  }
  
  changes <- layer.change(adjacency, expected, layer.set, vertex.set, score.old)
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
swap.vertex = function(adjacency, expected, layer.set, vertex.set, score.old){
  
  if(class(adjacency) != "list"){
    adjacency <- list(adjacency)
  }
  if(class(expected) != "list"){
    expected <- list(expected)
  }
  
  m <- length(adjacency)
  n <- dim(adjacency[[1]])[1]
  
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
  changes = vertex.change(adjacency, expected, layer.set, vertex.set, score.old)
  
  #changes[which(is.null(changes) == TRUE)] <- 0
  
  #changes[which(is.na(changes) == TRUE)] <- 0
  
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
single.swap = function(initial.set, adjacency, expected){
  
  m <- length(adjacency)
  n <- dim(adjacency[[1]])[1]
  
  #initialize vertex.set and layer.set
  B.new <- initial.set$vertex.set
  I.new <- initial.set$layer.set
  score.old <- score(adjacency, expected, vertex.set = B.new, layer.set = I.new)
  
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
        results <- swap.layer(adjacency, expected, I, B, score.old)
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
      results <- swap.vertex(adjacency, expected, I.new, B, score.old)
      
      B.new <- results$B.new
      score.old <- results$score.old
    }
  }
  return(list(B = sort(B.new), I = sort(I.new), Score = score.old))
}

######Effect on score when adding or subtracting a layer#######
layer.change = function(adjacency, expected, layer.set, vertex.set, score.old){
  
  #first check that adjacency and expected are lists
  if(class(adjacency) != "list"){
    adjacency <- list(adjacency)
  }
  
  if(class(expected) != "list"){
    expected <- list(expected)
  }
  
  
  m <- length(adjacency)
  n <- dim(adjacency[[1]])[1]
  indx <- setdiff(1:m, layer.set) #which layers are not in the current set
  score.changes <- rep(0, m)
  
  for(i in 1:m){
    if(i %in% indx){
      score.changes[i] <- score(adjacency, expected, vertex.set = 
                                  vertex.set,
                                layer.set = union(layer.set, i)) - score.old
    }
    if(i %in% indx == FALSE){
      score.changes[i] <- score(adjacency, expected, vertex.set = 
                                  vertex.set,
                                layer.set = setdiff(layer.set, i)) - score.old
    }
  }
  return(score.changes)
}

######Effect on score when adding or subtracting a vertex#######
vertex.change = function(adjacency, expected, layer.set, vertex.set, score.old){
  
  #first check that adjacency and expected are lists
  if(class(adjacency) != "list"){
    adjacency <- list(adjacency)
  }
  
  if(class(expected) != "list"){
  }
  n <- dim(adjacency[[1]])[1]
  indx <- setdiff(1:n, vertex.set)
  score.changes <- rep(0, n)
  #the following can also be parallelized!
  
  for(i in 1:n){
    if(i %in% indx){
      score.changes[i] <- score(adjacency, expected, vertex.set = 
                                  union(vertex.set, i),
                                layer.set = layer.set) - score.old
    }
    if(i %in% indx == FALSE){
      score.changes[i] <- score(adjacency, expected, vertex.set = 
                                  setdiff(vertex.set, i),
                                layer.set = layer.set) - score.old
    }
  }
  
  return(score.changes)
}