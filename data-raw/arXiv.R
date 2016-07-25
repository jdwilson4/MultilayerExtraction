library(dplyr)
library(lubridate)
library(tidyr)

load("arXiv.rda")

arXiv <- Adjacency
names(arXiv) <- layer.labels

devtools::use_data(arXiv, overwrite = TRUE)