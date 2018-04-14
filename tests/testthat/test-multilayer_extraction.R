context("test-multilayer_extraction.R")

test_that("multiplication works", {
  data("AU_CS")
  network <- adjacency.to.edgelist(AU_CS)
  community.object <- multilayer.extraction(adjacency = network, seed = 123, min.score = 0, prop.sample = .10)
})
