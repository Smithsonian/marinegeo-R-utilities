# Helper: minimal valid seagrass density data frame
.make_density_df <- function(
    sample_event_id           = "EVT-001",
    partner_code              = "TEST",
    site_code                 = "TST-001",
    site_name                 = "Test Site",
    table_id                  = "sav_density_v1",
    date                      = as.Date("2024-01-01"),
    transect                  = 1L,
    quadrat                   = c(1L, 2L),
    scientific_name           = "Halodule wrightii",
    shoot_count               = 10,
    shoot_density_m2          = 100,
    density_quadrat_dimensions = "25x25cm",
    input_filename            = "test.xlsx"
) {
  data.frame(
    sample_event_id            = sample_event_id,
    partner_code               = partner_code,
    site_code                  = site_code,
    site_name                  = site_name,
    table_id                   = table_id,
    sample_collection_date     = date,
    transect                   = transect,
    quadrat                    = quadrat,
    scientific_name            = scientific_name,
    shoot_count                = shoot_count,
    shoot_density_m2           = shoot_density_m2,
    density_quadrat_dimensions = density_quadrat_dimensions,
    input_filename             = input_filename,
    stringsAsFactors           = FALSE
  )
}

# Mock utl_mg_assign_functional_groups to avoid dependency on marinegeo_metadata
# state. Returns "Seagrass" for known seagrass species, NA for everything else.
.mock_density_fg <- function(fg_tree, fg_labels, scientific_names) {
  seagrass <- c("Halodule wrightii", "Thalassia testudinum", "Halophila ovalis")
  dplyr::case_when(
    scientific_names %in% seagrass ~ "Seagrass",
    .default = NA_character_
  )
}

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("non-data-frame input stops with informative error", {
  expect_error(utl_sav_backfill_density(list(a = 1)), "`df` must be a data frame")
})

test_that("missing required columns stops with informative error", {
  df <- .make_density_df()
  df$scientific_name <- NULL
  expect_error(utl_sav_backfill_density(df), "missing required column")
})

test_that("multiple missing columns are all named in error message", {
  df <- .make_density_df()
  df$shoot_count    <- NULL
  df$shoot_density_m2 <- NULL
  err <- tryCatch(utl_sav_backfill_density(df), error = function(e) conditionMessage(e))
  expect_match(err, "shoot_count")
  expect_match(err, "shoot_density_m2")
})

test_that("non-character scientific_name stops with informative error", {
  df <- .make_density_df()
  df$scientific_name <- 1:nrow(df)
  expect_error(utl_sav_backfill_density(df), "`scientific_name` must be a character column")
})

test_that("empty data frame returns as-is with a message", {
  df <- .make_density_df()[0, ]
  expect_message(result <- utl_sav_backfill_density(df), "zero rows")
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# Backfilling: happy path
# ---------------------------------------------------------------------------

test_that("species observed at one quadrat is backfilled into the other quadrat", {
  # Thalassia at quadrats 1 and 2; Halodule at quadrat 1 only.
  # After backfilling, Halodule should appear at quadrat 2 with counts = 0.
  df <- rbind(
    .make_density_df(
      quadrat         = c(1L, 2L),
      scientific_name = "Thalassia testudinum",
      shoot_count     = c(20, 15),
      shoot_density_m2 = c(200, 150)
    ),
    .make_density_df(
      quadrat         = 1L,
      scientific_name = "Halodule wrightii",
      shoot_count     = 5,
      shoot_density_m2 = 50
    )
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_density_fg)
  result <- suppressMessages(utl_sav_backfill_density(df))

  hw_q2 <- result[
    result$scientific_name == "Halodule wrightii" & result$quadrat == 2L, ,
    drop = FALSE
  ]
  expect_equal(nrow(hw_q2), 1L)
  expect_equal(hw_q2$shoot_count, 0)
  expect_equal(hw_q2$shoot_density_m2, 0)
})

test_that("backfilled rows inherit density_quadrat_dimensions and input_filename", {
  df <- rbind(
    .make_density_df(quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_density_df(quadrat = 1L,        scientific_name = "Halodule wrightii")
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_density_fg)
  result <- suppressMessages(utl_sav_backfill_density(df))

  new_row <- result[
    result$scientific_name == "Halodule wrightii" & result$quadrat == 2L, ,
    drop = FALSE
  ]
  expect_equal(new_row$density_quadrat_dimensions, "25x25cm")
  expect_equal(new_row$input_filename, "test.xlsx")
})

test_that("output has at least as many rows as input", {
  df <- rbind(
    .make_density_df(quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_density_df(quadrat = 1L,        scientific_name = "Halodule wrightii")
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_density_fg)
  result <- suppressMessages(utl_sav_backfill_density(df))
  expect_gte(nrow(result), nrow(df))
})

test_that("fully crossed data produces no new rows", {
  df <- rbind(
    .make_density_df(quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_density_df(quadrat = c(1L, 2L), scientific_name = "Halodule wrightii")
  )
  n_before <- nrow(df)

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_density_fg)
  result <- suppressMessages(utl_sav_backfill_density(df))
  expect_equal(nrow(result), n_before)
})

# ---------------------------------------------------------------------------
# Non-seagrass rows pass through unchanged
# ---------------------------------------------------------------------------

test_that("non-seagrass rows are passed through unchanged", {
  # "Stylea plicata" returns NA from mock -> treated as non-seagrass
  df <- rbind(
    .make_density_df(quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_density_df(quadrat = 1L,        scientific_name = "Stylea plicata",
                     shoot_count = 3, shoot_density_m2 = 30)
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_density_fg)
  result <- suppressMessages(utl_sav_backfill_density(df))

  stylea <- result[result$scientific_name == "Stylea plicata", , drop = FALSE]
  expect_equal(nrow(stylea), 1L)
  expect_equal(stylea$quadrat, 1L)
  expect_equal(stylea$shoot_count, 3)
})

# ---------------------------------------------------------------------------
# Ambiguous metadata fields -> NA + message
# ---------------------------------------------------------------------------

test_that("ambiguous density_quadrat_dimensions within a sample event emits a message and sets NA", {
  df <- rbind(
    .make_density_df(quadrat = 1L, scientific_name = "Thalassia testudinum",
                     density_quadrat_dimensions = "25x25cm"),
    .make_density_df(quadrat = 2L, scientific_name = "Thalassia testudinum",
                     density_quadrat_dimensions = "50x50cm"),
    .make_density_df(quadrat = 1L, scientific_name = "Halodule wrightii",
                     density_quadrat_dimensions = "25x25cm")
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_density_fg)
  expect_message(
    result <- utl_sav_backfill_density(df),
    "Unable to backfill quadrat dimensions"
  )

  new_row <- result[
    result$scientific_name == "Halodule wrightii" & result$quadrat == 2L, ,
    drop = FALSE
  ]
  expect_equal(nrow(new_row), 1L)
  expect_true(is.na(new_row$density_quadrat_dimensions))
})

# ---------------------------------------------------------------------------
# Multiple sample events
# ---------------------------------------------------------------------------

test_that("backfill operates independently across multiple sample events", {
  df <- rbind(
    # Event 1: Halodule at quadrat 1 only
    .make_density_df(sample_event_id = "EVT-001",
                     quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_density_df(sample_event_id = "EVT-001",
                     quadrat = 1L,        scientific_name = "Halodule wrightii"),
    # Event 2: Halophila at quadrat 2 only
    .make_density_df(sample_event_id = "EVT-002",
                     quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_density_df(sample_event_id = "EVT-002",
                     quadrat = 2L,        scientific_name = "Halophila ovalis")
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_density_fg)
  result <- suppressMessages(utl_sav_backfill_density(df))

  hw_evt1_q2 <- result[
    result$sample_event_id == "EVT-001" &
      result$scientific_name == "Halodule wrightii" &
      result$quadrat == 2L, ,
    drop = FALSE
  ]
  ho_evt2_q1 <- result[
    result$sample_event_id == "EVT-002" &
      result$scientific_name == "Halophila ovalis" &
      result$quadrat == 1L, ,
    drop = FALSE
  ]

  expect_equal(nrow(hw_evt1_q2), 1L)
  expect_equal(hw_evt1_q2$shoot_count, 0)
  expect_equal(nrow(ho_evt2_q1), 1L)
  expect_equal(ho_evt2_q1$shoot_count, 0)
})

# ---------------------------------------------------------------------------
# No seagrass rows
# ---------------------------------------------------------------------------

test_that("data frame with no seagrass rows returns input unchanged with message", {
  df <- .make_density_df(scientific_name = "Stylea plicata")

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_density_fg)
  expect_message(
    result <- utl_sav_backfill_density(df),
    "No Seagrass rows found"
  )
  expect_equal(nrow(result), nrow(df))
})
