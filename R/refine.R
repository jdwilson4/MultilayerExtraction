#' refine
#' 
#'Function that identifies statistically significant vertex-layer communities in multilayer networks.
#' @param Multilayer.object: a MultilayerCommunity object that contains communities extracted from the multilayer.extraction algorithm.
#' @param k: the number of communities that one would like to keep.
#' @param m: the total number of layers in the original multilayer network.
#' @param n: the total number of vertices in the original multilayer network.
#' 
#' @keywords community detection, multilayer networks, configuration model, random graph models
#' @return A list that contains the following objects
#' \itemize{
#'      \item Layers: an m x k matrix whose (i,j)th entry is non-zero if layer i is contained
#'      within community j. The value of the (i,j)th entry is the score of that community.
#'      \item Vertices: an n x k matrix whose (i,j)th entry is 1 if vertex i is contained within community j.
#' }
#'@references
#'\itemize{
#'     \item Wilson, James D., Palowitch, John, Bhamidi, Shankar, and Nobel, Andrew B. (2016) "Significance based 
#'     extraction in multilayer networks with heterogeneous community structure."
#' } 
#' @author James D. Wilson
#' @export 
#' 

#######################################################################
refine = function(Multilayer.object, k, m, n){
  #Multilayer.object = resulting object from running multilayer extraction
  indx = which(Multilayer.object$Diagnostics$Number.Communities > (k-1))[1]
  beta = Multilayer.object$Diagnostics$Beta[indx]
  Results = list()
  #score = numeric()
  h = length(Multilayer.object$Community.List)
  r = 0
  for(i in 1:h){
    g = length(Multilayer.object$Community.List[[i]]$Communities)
    for(j in 1:g){
      r = r + 1
      Results[[r]] = list(B = Multilayer.object$Community.List[[i]]$Communities[[j]]$B, I = Multilayer.object$Community.List[[i]]$Communities[[j]]$I, Score = Multilayer.object$Community.List[[i]]$Communities[[j]]$Score)
    }
  }
  Results = unique(Results)
  refined.group = cleanup(Results, beta)
  
  ##Now create a binary matrices for the results
  Score <- Matrix(0, m, k)
  Vertices <- Matrix(0, n, k)
  for(i in 1:k){
    Score[refined.group$Communities[[i]]$I, i] = refined.group$Communities[[i]]$Score
    Vertices[refined.group$Communities[[i]]$B, i] = 1
  }
  return(list(Layers = Score, Vertices = Vertices))
}

###cleanup function
cleanup = function(Results, beta){
  k = length(Results)
  if(k < 2){
    Results <- Results[[1]]
    Results$Mean.Score <- Results$Score
    Results$Number.Communities <- 1
    return(Results)
    }
  indx.rm = numeric()
  if(k > 1){
    for(i in 1:(k-1)){
      for(j in (i+1):k){
        match = length(intersect(Results[[i]]$B, Results[[j]]$B))*length(intersect(Results[[i]]$I, Results[[j]]$I))/(min(length(Results[[i]]$B)*length(Results[[i]]$I),length(Results[[j]]$B)*length(Results[[j]]$I)))
        pot = c(i,j)
        indx = pot[which.min(c(Results[[i]]$Score, Results[[j]]$Score))]
        if(match > beta-0.00001){indx.rm = c(indx.rm,indx)}
      }
    }
  }
  Results[indx.rm] = NULL
  
  temp = 0
  for(i in 1:length(Results)){
    temp = temp + Results[[i]]$Score
  }
  Mean.score = temp/length(Results)
  return(list(Communities = Results, Mean.Score = Mean.score))
}


