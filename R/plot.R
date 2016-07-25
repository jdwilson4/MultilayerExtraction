#' plot.MultilayerCommunity
#' 
#'Function plots the average score and total number of communities extracted across a grid of overlap parameters beta.
#
#' @param multilayer.object: a MultilayerCommunity object that contains communities extracted from the multilayer.extraction algorithm.
#' @param main: the title of the plot. Optional. Default is "". 
#' @keywords community detection, multilayer networks, configuration model, random graph models
#' @return A plot showing the total number of communities and average score of the communities across a grid of overlap parameters beta.
#' @details This is a useful diagnostic plot to determine how many communities were extracted in the observed multilayer network. At beta = 0,
#' all of the communities are disjoint. As beta increases, the communities are allowed to overlap more. See the below reference for more details about this function.
#'@references
#'\itemize{
#'     \item Wilson, James D., Palowitch, John, Bhamidi, Shankar, and Nobel, Andrew B. (2016) "Significance based 
#'     extraction in multilayer networks with heterogeneous community structure."
#' } 
#' 
#' @author James D. Wilson
#' @export 
#' 
plot.MultilayerCommunity = function(multilayer.object, main = ""){
  data.frame = multilayer.object$Diagnostics
  #Calculate when the biggest drop happens in total score

  diffs = diff(data.frame$Mean.Score)
  indx = which.min(diffs) 
  #Attempt to put both objects on the same plot
  par(mar=c(5, 4, 4, 6) + 0.1)
  
  ## Plot first set of data and draw its axis
  plot(data.frame$Beta, data.frame$Number.Communities, type = "l",lwd = 3, axes = FALSE, col = "blue", xlab = "", ylab = "", main = "")
  if(max(data.frame$Number.Communities) > 9){
    axis(2, c(seq(0,8,2),seq(10,max(data.frame$Number.Communities),5)),col = "blue", las = 1, col.axis = "blue")
  }
  
  if(max(data.frame$Number.Communities) < 10){
    axis(2, 1:max(data.frame$Number.Communities),col = "blue", las = 1, col.axis = "blue")
  }
  mtext("Number of Communities", side = 2, line = 2.5, cex = 1.5)
  box()
  
  #Add the second graph
  par(new = TRUE)
  plot(data.frame$Beta, (data.frame$Mean.Score), xlab = "", type = "l", lty = 2, lwd = 3, col = "darkgreen", ylab = "", axes = FALSE)
  mtext("Average Score", side = 4, line = 4, cex = 1.5)
  axis(4, col = "darkgreen", col.axis = "darkgreen", las = 1)
  
  axis(1, seq(0,1,0.1))
  mtext(expression(beta), side=1, col="black", line = 2.5, cex = 1.5)  
  
}