test_that("test api_worms_get_aphia_id with sample IDs", {

  phylum <- "Mollusca" # 51
  result_phylum <- api_worms_get_aphia_id(phylum)

  expect_identical(result_phylum$taxonomic_id, 51)
  expect_identical(result_phylum$name_to_evaluate, "Mollusca")
  expect_identical(result_phylum$scientific_name, "Mollusca")
  expect_equal(nrow(result_phylum), 1)
  expect_length(result_phylum, 3)

  # Turning off this test until package is further developed - hitting API slows down tests
  # class <- "Gastropoda" # 101
  # result_class <- api_worms_get_aphia_id(class)
  #
  # expect_identical(result_class$taxonomic_id, 101)
  # expect_identical(result_class$name_to_evaluate, "Gastropoda")
  # expect_identical(result_class$scientific_name, "Gastropoda")
  # expect_equal(nrow(result_class), 1)
  # expect_length(result_class, 3)
  #
  # Turning off this test until package is further developed - hitting API slows down tests
  # genus <- "Lithothamnion" # 144018
  # result_genus <- api_worms_get_aphia_id(genus)
  #
  # expect_identical(result_genus$taxonomic_id, 144018)
  # expect_identical(result_genus$name_to_evaluate, "Lithothamnion")
  # expect_identical(result_genus$scientific_name, "Lithothamnion")
  # expect_equal(nrow(result_genus), 1)
  # expect_length(result_genus, 3)

  # See issue https://github.com/MarineGEO/reef-life-survey-marinegeo/issues/12
  # species <- "Zostera marina" # 495077
  # result_species <- api_worms_get_aphia_id(species)
  #
  # expect_identical(result_species$taxonomic_id, 495077)
  # expect_identical(result_species$name_to_evaluate, "Zostera marina")
  # expect_identical(result_species$scientific_name, "Zostera marina")
  # expect_equal(nrow(result_species), 1)
  # expect_length(result_species, 3)

  genus <- "Lithothamnion" # 144018

  multi_items <- c(phylum, genus)
  result_multi <- api_worms_get_aphia_id(multi_items)

  expect_identical(result_multi$taxonomic_id, c(51, 144018))
  expect_identical(result_multi$name_to_evaluate, multi_items)
  expect_identical(result_multi$scientific_name, multi_items)
  expect_equal(nrow(result_multi), 2)

  # invalid_string <- "very rare fish"
  # result_invalid_string <- api_worms_get_aphia_id(invalid_string)

  invalid_integer <- 51
  expect_error(
    result_invalid_integer <- api_worms_get_aphia_id(invalid_integer),
    "`scientific_names` should be a character object or vector"
  )

  # Need test for one valid, one invalid

})


