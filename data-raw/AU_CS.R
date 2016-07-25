library(dplyr)
library(lubridate)
library(tidyr)

load("data-raw/AU_CS.rda")
AU_CS <- Adjacency

devtools::use_data(AU_CS, overwrite = TRUE)
