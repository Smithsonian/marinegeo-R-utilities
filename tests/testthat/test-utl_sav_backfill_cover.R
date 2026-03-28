# Helper: minimal valid seagrass cover data frame
.make_cover_df <- function(
    sample_event_id = "EVT-001",
    partner_code    = "TEST",
    site_code       = "TST-001",
    site_name       = "Test Site",
    table_id        = "sav_cover_v1",
    date            = as.Date("2024-01-01"),
    transect        = 1L,
    quadrat         = c(1L, 2L),
    scientific_name = "Halodule wrightii",
    percent_cover   = 50,
    cover_code      = 3,
    cover_method    = "Braun-Blanquet",
    cover_quadrat_dimensions = "50x50cm",
    input_filename  = "test.xlsx"
) {
  data.frame(
    sample_event_id          = sample_event_id,
    partner_code             = partner_code,
    site_code                = site_code,
    site_name                = site_name,
    table_id                 = table_id,
    sample_collection_date   = date,
    transect                 = transect,
    quadrat                  = quadrat,
    scientific_name          = scientific_name,
    percent_cover            = percent_cover,
    cover_code               = cover_code,
    cover_method             = cover_method,
    cover_quadrat_dimensions = cover_quadrat_dimensions,
    input_filename           = input_filename,
    stringsAsFactors         = FALSE
  )
}

# Mock utl_mg_assign_functional_groups to avoid dependency on marinegeo_metadata
# state (which may be reduced by other test files using local_mocked_bindings).
# Returns "Seagrass" for known seagrass species, "Macroalgae" for known algae,
# and NA for everything else.
.mock_fg <- function(fg_tree, fg_labels, scientific_names) {
  seagrass   <- c("Halodule wrightii", "Thalassia testudinum", "Halophila ovalis")
  macroalgae <- c("Caulerpa", "Caulerpa sertularioides", "Acanthophora spicifera")
  dplyr::case_when(
    scientific_names %in% seagrass   ~ "Seagrass",
    scientific_names %in% macroalgae ~ "Macroalgae",
    .default = NA_character_
  )
}

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("non-data-frame input stops with informative error", {
  expect_error(utl_sav_backfill_cover(list(a = 1)), "`df` must be a data frame")
})

test_that("missing required columns stops with informative error", {
  df <- .make_cover_df()
  df$scientific_name <- NULL
  expect_error(utl_sav_backfill_cover(df), "missing required column")
})

test_that("multiple missing columns are all named in error message", {
  df <- .make_cover_df()
  df$percent_cover <- NULL
  df$cover_code    <- NULL
  err <- tryCatch(utl_sav_backfill_cover(df), error = function(e) conditionMessage(e))
  expect_match(err, "percent_cover")
  expect_match(err, "cover_code")
})

test_that("non-character scientific_name stops with informative error", {
  df <- .make_cover_df()
  df$scientific_name <- 1:nrow(df)
  expect_error(utl_sav_backfill_cover(df), "`scientific_name` must be a character column")
})

test_that("empty data frame returns as-is with a message", {
  df <- .make_cover_df()[0, ]
  expect_message(result <- utl_sav_backfill_cover(df), "zero rows")
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# Backfilling: happy path
# ---------------------------------------------------------------------------

test_that("species observed at one quadrat is backfilled into the other quadrat", {
  # Thalassia at quadrats 1 and 2; Halodule at quadrat 1 only.
  # After backfilling, Halodule should also appear at quadrat 2 with cover = 0.
  df <- rbind(
    .make_cover_df(
      quadrat         = c(1L, 2L),
      scientific_name = "Thalassia testudinum",
      percent_cover   = c(50, 30),
      cover_code      = c(3, 2)
    ),
    .make_cover_df(
      quadrat         = 1L,
      scientific_name = "Halodule wrightii",
      percent_cover   = 10,
      cover_code      = 1
    )
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_fg)
  result <- suppressMessages(utl_sav_backfill_cover(df))

  hw_q2 <- result[
    result$scientific_name == "Halodule wrightii" & result$quadrat == 2L, ,
    drop = FALSE
  ]
  expect_equal(nrow(hw_q2), 1L)
  expect_equal(hw_q2$percent_cover, 0)
  expect_equal(hw_q2$cover_code, 0)
})

test_that("backfilled rows inherit cover_method, cover_quadrat_dimensions, input_filename", {
  df <- rbind(
    .make_cover_df(quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_cover_df(quadrat = 1L,        scientific_name = "Halodule wrightii")
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_fg)
  result <- suppressMessages(utl_sav_backfill_cover(df))

  new_row <- result[
    result$scientific_name == "Halodule wrightii" & result$quadrat == 2L, ,
    drop = FALSE
  ]
  expect_equal(new_row$cover_method, "Braun-Blanquet")
  expect_equal(new_row$cover_quadrat_dimensions, "50x50cm")
  expect_equal(new_row$input_filename, "test.xlsx")
})

test_that("output has at least as many rows as input", {
  df <- rbind(
    .make_cover_df(quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_cover_df(quadrat = 1L,        scientific_name = "Halodule wrightii")
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_fg)
  result <- suppressMessages(utl_sav_backfill_cover(df))
  expect_gte(nrow(result), nrow(df))
})

test_that("fully crossed data produces no new rows", {
  df <- rbind(
    .make_cover_df(quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_cover_df(quadrat = c(1L, 2L), scientific_name = "Halodule wrightii")
  )
  n_before <- nrow(df)

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_fg)
  result <- suppressMessages(utl_sav_backfill_cover(df))
  expect_equal(nrow(result), n_before)
})

# ---------------------------------------------------------------------------
# Non-macrophyte rows pass through unchanged
# ---------------------------------------------------------------------------

test_that("non-macrophyte rows are passed through unchanged", {
  # "Stylea plicata" returns NA from .mock_fg -> treated as non-macrophyte
  df <- rbind(
    .make_cover_df(quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_cover_df(quadrat = 1L,        scientific_name = "Stylea plicata",
                   percent_cover = 5, cover_code = 1)
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_fg)
  result <- suppressMessages(utl_sav_backfill_cover(df))

  stylea <- result[result$scientific_name == "Stylea plicata", , drop = FALSE]
  expect_equal(nrow(stylea), 1L)
  expect_equal(stylea$quadrat, 1L)
  expect_equal(stylea$percent_cover, 5)
})

# ---------------------------------------------------------------------------
# Ambiguous metadata fields -> NA + message
# ---------------------------------------------------------------------------

test_that("ambiguous cover_method within a sample event emits a message and sets NA", {
  df <- rbind(
    .make_cover_df(quadrat = 1L, scientific_name = "Thalassia testudinum",
                   cover_method = "Braun-Blanquet"),
    .make_cover_df(quadrat = 2L, scientific_name = "Thalassia testudinum",
                   cover_method = "Point-intercept"),
    .make_cover_df(quadrat = 1L, scientific_name = "Halodule wrightii",
                   cover_method = "Braun-Blanquet")
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_fg)
  expect_message(
    result <- utl_sav_backfill_cover(df),
    "Unable to backfill cover method"
  )

  new_row <- result[
    result$scientific_name == "Halodule wrightii" & result$quadrat == 2L, ,
    drop = FALSE
  ]
  expect_equal(nrow(new_row), 1L)
  expect_true(is.na(new_row$cover_method))
})

# ---------------------------------------------------------------------------
# Multiple sample events
# ---------------------------------------------------------------------------

test_that("backfill operates independently across multiple sample events", {
  df <- rbind(
    # Event 1: Halodule at quadrat 1 only
    .make_cover_df(sample_event_id = "EVT-001",
                   quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_cover_df(sample_event_id = "EVT-001",
                   quadrat = 1L,        scientific_name = "Halodule wrightii"),
    # Event 2: Caulerpa at quadrat 2 only
    .make_cover_df(sample_event_id = "EVT-002",
                   quadrat = c(1L, 2L), scientific_name = "Thalassia testudinum"),
    .make_cover_df(sample_event_id = "EVT-002",
                   quadrat = 2L,        scientific_name = "Caulerpa")
  )

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_fg)
  result <- suppressMessages(utl_sav_backfill_cover(df))

  hw_evt1_q2 <- result[
    result$sample_event_id == "EVT-001" &
      result$scientific_name == "Halodule wrightii" &
      result$quadrat == 2L, ,
    drop = FALSE
  ]
  caulerpa_evt2_q1 <- result[
    result$sample_event_id == "EVT-002" &
      result$scientific_name == "Caulerpa" &
      result$quadrat == 1L, ,
    drop = FALSE
  ]

  expect_equal(nrow(hw_evt1_q2), 1L)
  expect_equal(hw_evt1_q2$percent_cover, 0)
  expect_equal(nrow(caulerpa_evt2_q1), 1L)
  expect_equal(caulerpa_evt2_q1$percent_cover, 0)
})

# ---------------------------------------------------------------------------
# No macrophyte rows
# ---------------------------------------------------------------------------

test_that("data frame with no macrophyte rows returns input unchanged with message", {
  df <- .make_cover_df(scientific_name = "Stylea plicata")

  local_mocked_bindings(utl_mg_assign_functional_groups = .mock_fg)
  expect_message(
    result <- utl_sav_backfill_cover(df),
    "No Seagrass or Macroalgae rows found"
  )
  expect_equal(nrow(result), nrow(df))
})
