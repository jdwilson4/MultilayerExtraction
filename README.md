## MultilayerExtraction
An R package that implements the multilayer extraction algorithm to identify
densely connected vertex-layer sets in multilayer networks.

The key reference for this monitoring method is

Wilson, J.D., Palowitch, J., Bhamidi, S., and Nobel, A.B. (2017) **Significance based extraction in multilayer networks with heterogeneous community structure**, *Journal of Machine Learning Research* (18) 1-49

## Installation

To install MultilayerExtraction, use the following commands. Be sure to include the required package **devtools** from R version 3.2.2 or higher.

``` 
#install the latest version of devtools
install.packages("devtools")
library(devtools, quietly = TRUE)

#install and load MultilayerExtraction
devtools::install_github('jdwilson4/multilayer_extraction')

library(MultilayerExtraction, quietly = TRUE)
```

## Install all other dependencies
```
install.packages("Matrix")
install.packages("foreach")
install.packages("doParallel")
install.packages("parallel")
install.packages("igraph")

library(Matrix, quietly = TRUE)
library(igraph, quietly = TRUE)
library(foreach, quietly = TRUE)  
library(doParallel, quietly = TRUE) 
library(parallel, quietly = TRUE)  
```

## Description
This package contains four primary functions, which are briefly described below. For a function named ```function``` below, type ```?function``` in R to get full documentation.

- ```multilayer.extraction()```: identify densely connected vertex-layer communities within multilayer networks. This is a significance based approach that seeks to optimize a score derived from the strength of connection within multilayer communities. See the above reference for more details.
- ```expectation.CM()```: estimate the expected edge weight between each pair of nodes across each layer of the multilayer network.
- ```score()```: calculate the score of a specified vertex-layer community in an observed multilayer network.
- ```plot.MultilayerCommunity()```: diagnostic plot that shows the number of communities and average score of communities from extracted communities against the overall overlap between each identified community.
- ```refine()```: refine a ```MultilayerCommunity``` object identified by the ```multilayer.extraction()``` function so as to obtain a desired amount of overlap.

## Application: Austrailian computer science network
Here we show how to use multilayer.extraction in practice by demonstrating an example on the Austrailian Computer Science network. This data set is a multilayer network describing the relationships among attendees at an Austrailian computer science conference. 

There were 61 attendees (represented by the nodes in each layer), and 5 relationships that are represented by the layers in the network. The 
data is provided in the file ```AU_CS.rda``` in a list format where each entry represents a different layer, and the type of relationship is represented by the name of the list entry.  

Below, we show how to load the data and run the Multilayer Extraction algorithm on the network.

```
#load the data
data("AU_CS")

#number of nodes and layers
n <- dim(AU_CS[[1]])[1]
m <- length(AU_CS)

#types of relationships among attendees
relationship.names <- names(AU_CS)
print(relationship.names)
```

Next, we get a cursory view of some of the layers in the network. We follow this up by looking at the mean degree of the nodes in each of the layers.

```
image(AU_CS$coauthor, main = "Co-author")
image(AU_CS$facebook, main = "Leisure")
image(AU_CS$leisure, main = "Facebook")


Mean.Degree <- sapply(AU_CS, mean) * n
print(Mean.Degree)
```

Notably, this network has heterogeneous connectivity as witnessed above. The Multilayer Extraction algorithm accounts for such heterogeneity and identifies communities using the joint information from the layers.

We now run the extraction algorithm.
```
##run Multilayer Extraction algorithm on this network

##convert the list of adjacency matrices to an edgelist
network <- adjacency.to.edgelist(AU_CS)
set.seed(123)
start_time <- Sys.time()
community.object <- multilayer.extraction(adjacency = network, seed = 123, min.score = 0, prop.sample = .10)
end_time <- Sys.time()

end_time - start_time
#for me, this took 1.055319 mins

#plot the number of communities across overlap parameter beta
plot(community.object, main = "Diagnostic Plot AU_CS")
object <- refine(community.object, k = 6, m = m, n = n)

##there are 7 small communities. Let's look at the size of each
num.layers <- colSums(object$Layers > 0)
num.vertices <- colSums(object$Vertices)

print(data.frame(Layers = num.layers, Vertices = num.vertices))
```

We now re-order the adjacency matrices according to a disjoint re-ordering of the vertices from the identified communities.

```
indx1 <- which(object$Vertices[,1] == 1)
indx2 <- setdiff(which(object$Vertices[,2] == 1), indx1)
indx3 <- setdiff(setdiff(which(object$Vertices[,3] == 1), indx1), indx2)
indx4 <- setdiff(setdiff(setdiff(which(object$Vertices[,4] == 1), indx3),       indx2),indx1)
indx5 <- setdiff(setdiff(setdiff(setdiff(which(object$Vertices[,5] == 1), indx4), indx3),indx2), indx1)
                                 
indx6 <- setdiff(setdiff(setdiff(setdiff(setdiff(which(object$Vertices[,6] == 1),
                                                 indx1), indx2), indx3), indx4), indx5)
none2 <- setdiff(1:61, union(union(union(indx1, indx2), union(indx3, indx4)), union(indx5, indx6)))


image(object$Layers, main = "Layers in Communities")

#Visualizing the disjoint communities
re.order <- c(indx1, indx2, indx3, indx4, indx5, indx6, none2)
image(AU_CS$coauthor[re.order,re.order], main = "Co-Author")
image(AU_CS$leisure[re.order,re.order], main = "Leisure")
image(AU_CS$work[re.order,re.order], main = "Work")
image(AU_CS$lunch[re.order,re.order], main = "Lunch")
image(AU_CS$facebook[re.order,re.order], main = "Facebook")
```
As we can see above, the communities are strongly connected relative to the remaining network, as desired.  

## Contributors
- **James D. Wilson**, Assistant Professor of Statistics, University of San Francisco. Developer, contributor, and maintainer. 
- **Jean Carlos Paredes**, University of San Francisco. Contributor.

Please send any comments, bugs, or questions to the developer James D. Wilson at jdwilson4@usfca.edu. 