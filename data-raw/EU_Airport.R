library(dplyr)
library(lubridate)
library(tidyr)

load("EU_Airport.rda")

EU_Airport <- Adjacency
names(EU_Airport) <- layer.labels

devtools::use_data(EU_Airport, overwrite = TRUE)