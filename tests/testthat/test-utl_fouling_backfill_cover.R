# Helper: minimal valid fouling panel cover data frame
.make_fouling_df <- function(
  sample_event_id = "EVT-001",
  partner_code = "TEST",
  site_code = "TST-001",
  site_name = "Test Site",
  table_id = "fouling-cover-v1",
  panel_id = "panel-A",
  deployment_date = as.Date("2024-06-01"),
  retrieval_date = as.Date("2024-07-01"),
  deployment_period = "30 day",
  scientific_name = "Didemnum perlucidum",
  point_count = 5L,
  points_in_grid = 100L,
  percent_cover = 5,
  photo_filename = "panel-A.jpg",
  habitat = "artificial",
  input_filename = "test.xlsx"
) {
  data.frame(
    sample_event_id = sample_event_id,
    partner_code = partner_code,
    site_code = site_code,
    site_name = site_name,
    table_id = table_id,
    panel_id = panel_id,
    deployment_date = deployment_date,
    retrieval_date = retrieval_date,
    deployment_period = deployment_period,
    scientific_name = scientific_name,
    point_count = point_count,
    points_in_grid = points_in_grid,
    percent_cover = percent_cover,
    photo_filename = photo_filename,
    habitat = habitat,
    input_filename = input_filename,
    stringsAsFactors = FALSE
  )
}

# Mock utl_mg_assign_ancestor_labels to avoid dependency on marinegeo_metadata
# state (which may be reduced by other test files using local_mocked_bindings).
# `type = "primary"` arrives through `...`, matching the real signature.
.mock_fouling_group <- function(fg_tree, scientific_names, ...) {
  ascidians <- c("Didemnum perlucidum", "Botrylloides niger")
  bryozoans <- c("Bugula neritina", "Crisia noronhai")
  dplyr::case_when(
    scientific_names %in% ascidians ~ "colonial ascidians",
    scientific_names %in% bryozoans ~ "arborescent bryozoans",
    .default = NA_character_
  )
}

# Pull the single row matching a panel image / taxon out of a result
.pick <- function(df, scientific_name, panel_id, deployment_period = "30 day") {
  df[
    df$scientific_name == scientific_name &
      df$panel_id == panel_id &
      df$deployment_period == deployment_period,
    ,
    drop = FALSE
  ]
}

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("non-data-frame input stops with informative error", {
  expect_error(
    utl_fouling_backfill_cover(list(a = 1)),
    "`df` must be a data frame"
  )
})

test_that("missing required columns stops with informative error", {
  df <- .make_fouling_df()
  df$panel_id <- NULL
  expect_error(utl_fouling_backfill_cover(df), "missing required column")
})

test_that("multiple missing columns are all named in error message", {
  df <- .make_fouling_df()
  df$points_in_grid <- NULL
  df$deployment_period <- NULL
  err <- tryCatch(
    utl_fouling_backfill_cover(df),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "points_in_grid")
  expect_match(err, "deployment_period")
})

test_that("non-character scientific_name stops with informative error", {
  df <- .make_fouling_df()
  df$scientific_name <- 1:nrow(df)
  expect_error(
    utl_fouling_backfill_cover(df),
    "`scientific_name` must be a character column"
  )
})

test_that("empty data frame returns as-is with a message", {
  df <- .make_fouling_df()[0, ]
  expect_message(result <- utl_fouling_backfill_cover(df), "zero rows")
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# Backfilling: happy path
# ---------------------------------------------------------------------------

test_that("taxon observed on one panel is backfilled onto the other panel", {
  # Didemnum on panels A and B; Bugula on panel A only.
  # After backfilling, Bugula should also appear on panel B with cover = 0.
  df <- rbind(
    .make_fouling_df(
      panel_id = c("panel-A", "panel-B"),
      scientific_name = "Didemnum perlucidum",
      point_count = c(40L, 30L),
      percent_cover = c(40, 30)
    ),
    .make_fouling_df(
      panel_id = "panel-A",
      scientific_name = "Bugula neritina",
      point_count = 10L,
      percent_cover = 10
    )
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))

  new_row <- .pick(result, "Bugula neritina", "panel-B")
  expect_equal(nrow(new_row), 1L)
  expect_equal(new_row$percent_cover, 0)
  expect_equal(new_row$point_count, 0L)
})

test_that("taxon observed in one deployment period is backfilled into the others", {
  df <- rbind(
    .make_fouling_df(deployment_period = c("30 day", "60 day", "90 day")),
    .make_fouling_df(
      deployment_period = "90 day",
      scientific_name = "Bugula neritina"
    )
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))

  bugula <- result[result$scientific_name == "Bugula neritina", , drop = FALSE]
  expect_equal(nrow(bugula), 3L)
  expect_setequal(bugula$deployment_period, c("30 day", "60 day", "90 day"))
  expect_equal(
    bugula$percent_cover[bugula$deployment_period != "90 day"],
    c(0, 0)
  )
})

test_that("backfilled rows inherit panel image and sample event fields", {
  df <- rbind(
    .make_fouling_df(
      panel_id = c("panel-A", "panel-B"),
      photo_filename = c("A.jpg", "B.jpg"),
      habitat = c("artificial", "seagrass"),
      points_in_grid = c(100L, 50L)
    ),
    .make_fouling_df(
      panel_id = "panel-A",
      scientific_name = "Bugula neritina"
    )
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))

  new_row <- .pick(result, "Bugula neritina", "panel-B")
  expect_equal(new_row$photo_filename, "B.jpg")
  expect_equal(new_row$habitat, "seagrass")
  expect_equal(new_row$points_in_grid, 50L)
  expect_equal(new_row$retrieval_date, as.Date("2024-07-01"))
  expect_equal(new_row$site_name, "Test Site")
  expect_equal(new_row$input_filename, "test.xlsx")
})

test_that("output has at least as many rows as input", {
  df <- rbind(
    .make_fouling_df(panel_id = c("panel-A", "panel-B")),
    .make_fouling_df(panel_id = "panel-A", scientific_name = "Bugula neritina")
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))
  expect_gte(nrow(result), nrow(df))
})

test_that("fully crossed data produces no new rows", {
  df <- rbind(
    .make_fouling_df(panel_id = c("panel-A", "panel-B")),
    .make_fouling_df(
      panel_id = c("panel-A", "panel-B"),
      scientific_name = "Bugula neritina"
    )
  )
  n_before <- nrow(df)

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))
  expect_equal(nrow(result), n_before)
})

# ---------------------------------------------------------------------------
# Panel images are never invented
# ---------------------------------------------------------------------------

test_that("a panel missing from a deployment period gains no rows in that period", {
  # panel-B was lost before the 90 day retrieval: 2 panels at 30 and 60 day,
  # 1 panel at 90 day. Backfilling must not resurrect it.
  df <- rbind(
    .make_fouling_df(
      panel_id = rep(c("panel-A", "panel-B"), each = 2),
      deployment_period = rep(c("30 day", "60 day"), times = 2)
    ),
    .make_fouling_df(panel_id = "panel-A", deployment_period = "90 day"),
    .make_fouling_df(
      panel_id = "panel-A",
      deployment_period = "30 day",
      scientific_name = "Bugula neritina"
    )
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))

  panels_90 <- unique(result$panel_id[result$deployment_period == "90 day"])
  expect_equal(panels_90, "panel-A")

  images <- unique(result[, c("deployment_period", "panel_id")])
  expect_equal(nrow(images), 5L)

  # 5 panel images x 2 taxa
  expect_equal(nrow(result), 10L)
})

# ---------------------------------------------------------------------------
# point_count depends on points_in_grid
# ---------------------------------------------------------------------------

test_that("point_count stays NA when the panel image has no points_in_grid", {
  # Percent cover reported directly, with no scoring grid.
  df <- rbind(
    .make_fouling_df(
      panel_id = c("panel-A", "panel-B"),
      point_count = NA_integer_,
      points_in_grid = NA_integer_
    ),
    .make_fouling_df(
      panel_id = "panel-A",
      scientific_name = "Bugula neritina",
      point_count = NA_integer_,
      points_in_grid = NA_integer_
    )
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))

  new_row <- .pick(result, "Bugula neritina", "panel-B")
  expect_equal(nrow(new_row), 1L)
  expect_equal(new_row$percent_cover, 0)
  expect_true(is.na(new_row$point_count))
})

test_that("point_count keeps the integer type of the input column", {
  df <- rbind(
    .make_fouling_df(panel_id = c("panel-A", "panel-B")),
    .make_fouling_df(panel_id = "panel-A", scientific_name = "Bugula neritina")
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))
  expect_true(is.integer(result$point_count))
})

# ---------------------------------------------------------------------------
# Ungrouped taxa pass through unchanged
# ---------------------------------------------------------------------------

test_that("ungrouped taxa are passed through unchanged and gain no zero rows", {
  # "zip tie" returns NA from the mock -> enrolled in no primary group
  df <- rbind(
    .make_fouling_df(panel_id = c("panel-A", "panel-B")),
    .make_fouling_df(
      panel_id = "panel-A",
      scientific_name = "zip tie",
      point_count = 2L,
      percent_cover = 2
    )
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))

  zip_tie <- result[result$scientific_name == "zip tie", , drop = FALSE]
  expect_equal(nrow(zip_tie), 1L)
  expect_equal(zip_tie$panel_id, "panel-A")
  expect_equal(zip_tie$percent_cover, 2)
})

test_that("the internal group column is not returned", {
  df <- .make_fouling_df(panel_id = c("panel-A", "panel-B"))

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))
  expect_equal(sort(colnames(result)), sort(colnames(df)))
})

# ---------------------------------------------------------------------------
# Ambiguous metadata fields -> NA + message
# ---------------------------------------------------------------------------

test_that("ambiguous input_filename within a sample event emits a message and sets NA", {
  df <- rbind(
    .make_fouling_df(panel_id = "panel-A", input_filename = "a.xlsx"),
    .make_fouling_df(panel_id = "panel-B", input_filename = "b.xlsx"),
    .make_fouling_df(
      panel_id = "panel-A",
      scientific_name = "Bugula neritina",
      input_filename = "a.xlsx"
    )
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  expect_message(
    result <- utl_fouling_backfill_cover(df),
    "Unable to backfill input_filename"
  )

  new_row <- .pick(result, "Bugula neritina", "panel-B")
  expect_equal(nrow(new_row), 1L)
  expect_true(is.na(new_row$input_filename))
})

test_that("ambiguous habitat within a panel image emits a message and sets NA", {
  df <- rbind(
    .make_fouling_df(
      panel_id = "panel-A",
      habitat = c("artificial", "seagrass")
    ),
    .make_fouling_df(panel_id = "panel-B"),
    .make_fouling_df(
      panel_id = "panel-B",
      scientific_name = "Bugula neritina"
    )
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  expect_message(
    result <- utl_fouling_backfill_cover(df),
    "Unable to backfill habitat"
  )

  new_row <- .pick(result, "Bugula neritina", "panel-A")
  expect_equal(nrow(new_row), 1L)
  expect_true(is.na(new_row$habitat))
})

# ---------------------------------------------------------------------------
# Multiple sample events
# ---------------------------------------------------------------------------

test_that("backfill operates independently across multiple sample events", {
  df <- rbind(
    # Event 1: Bugula on panel-A only
    .make_fouling_df(
      sample_event_id = "EVT-001",
      panel_id = c("panel-A", "panel-B")
    ),
    .make_fouling_df(
      sample_event_id = "EVT-001",
      scientific_name = "Bugula neritina"
    ),
    # Event 2: Botrylloides on panel-B only
    .make_fouling_df(
      sample_event_id = "EVT-002",
      panel_id = c("panel-A", "panel-B")
    ),
    .make_fouling_df(
      sample_event_id = "EVT-002",
      panel_id = "panel-B",
      scientific_name = "Botrylloides niger"
    )
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  result <- suppressMessages(utl_fouling_backfill_cover(df))

  evt1 <- result[result$sample_event_id == "EVT-001", , drop = FALSE]
  evt2 <- result[result$sample_event_id == "EVT-002", , drop = FALSE]

  bugula_evt1_b <- .pick(evt1, "Bugula neritina", "panel-B")
  expect_equal(nrow(bugula_evt1_b), 1L)
  expect_equal(bugula_evt1_b$percent_cover, 0)

  # Bugula belongs to event 1 only and must not leak into event 2
  expect_equal(sum(evt2$scientific_name == "Bugula neritina"), 0L)

  botryl_evt2_a <- .pick(evt2, "Botrylloides niger", "panel-A")
  expect_equal(nrow(botryl_evt2_a), 1L)
  expect_equal(botryl_evt2_a$percent_cover, 0)
})

# ---------------------------------------------------------------------------
# No grouped rows
# ---------------------------------------------------------------------------

test_that("data frame with no grouped rows returns input unchanged with message", {
  df <- .make_fouling_df(
    panel_id = c("panel-A", "panel-B"),
    scientific_name = "zip tie"
  )

  local_mocked_bindings(utl_mg_assign_ancestor_labels = .mock_fouling_group)
  expect_message(
    result <- utl_fouling_backfill_cover(df),
    "No rows enrolled in a primary fouling group"
  )
  expect_equal(nrow(result), nrow(df))
  expect_equal(sort(colnames(result)), sort(colnames(df)))
})
