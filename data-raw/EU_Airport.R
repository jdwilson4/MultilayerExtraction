library(dplyr)
library(lubridate)
library(tidyr)

load("data-raw/EU_Airport.rda")

EU_Airport <- Adjacency
names(EU_Airport) <- layer.labels

devtools::use_data(EU_Airport, overwrite = TRUE)
