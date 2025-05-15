test_that("test api_worms_get_classification with sample Aphia IDs", {
  
  phylum <- 51 # Mollusca
  result_phylum <- api_worms_get_classification(phylum)
  
  expect_identical(result_phylum$taxonomic_id, 51)
  expect_identical(result_phylum$phylum, "Mollusca")
  expect_identical(result_phylum$level, "phylum")
  expect_equal(nrow(result_phylum), 1)
  expect_length(result_phylum, 4)
  
  genus <- 144018 # Lithothamnion
  multi_items <- c(phylum, genus)
  result_multi <- api_worms_get_classification(multi_items)
  
  expect_identical(result_multi$kingdom, c("Animalia", "Plantae"))
  expect_equal(nrow(result_multi), 2)
  expect_length(result_multi, 12)
  
  #invalid <- -6
  #result_invalid <- api_worms_get_classification(invalid)
  
})
