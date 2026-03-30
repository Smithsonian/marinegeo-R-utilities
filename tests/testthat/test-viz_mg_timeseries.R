# ---------------------------------------------------------------------------
# Shared fixture
# ---------------------------------------------------------------------------

.ts_df <- data.frame(
  year    = factor(rep(2020:2022, each = 2)),
  cover   = c(10, 20, 15, 25, 12, 18),
  species = rep(c("Sp A", "Sp B"), 3),
  site    = rep(c("Site A", "Site B", "Site A"), each = 2),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Return value structure
# ---------------------------------------------------------------------------

test_that("returns a ggplot object", {
  result <- viz_mg_timeseries_annual(
    df             = .ts_df,
    y_var          = "cover",
    y_label        = "Percent cover",
    y_grouping_var = "species"
  )

  expect_s3_class(result, "ggplot")
})

# ---------------------------------------------------------------------------
# Happy path
# ---------------------------------------------------------------------------

test_that("minimal call with required args only succeeds", {
  expect_no_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species"
    )
  )
})

test_that("all optional args NULL (default) succeeds", {
  expect_no_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      x_var          = "year",
      y_limits       = NULL,
      facet_var      = NULL,
      facet_num_cols = NULL,
      plot_theme     = NULL
    )
  )
})

test_that("non-default x_var is accepted", {
  df <- .ts_df
  df$month <- factor(rep(c("Jan", "Feb"), 3))

  expect_no_error(
    viz_mg_timeseries_annual(
      df             = df,
      y_var          = "cover",
      x_var          = "month",
      y_label        = "Percent cover",
      y_grouping_var = "species"
    )
  )
})

# ---------------------------------------------------------------------------
# y_limits
# ---------------------------------------------------------------------------

test_that("valid y_limits c(0, 100) succeeds", {
  expect_no_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      y_limits       = c(0, 100)
    )
  )
})

test_that("y_limits of length 1 stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      y_limits       = c(0)
    ),
    "length 2"
  )
})

test_that("y_limits of length 3 stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      y_limits       = c(0, 50, 100)
    ),
    "length 2"
  )
})

test_that("non-numeric y_limits stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      y_limits       = c("0", "100")
    ),
    "numeric"
  )
})

test_that("y_limits with NA stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      y_limits       = c(0, NA)
    ),
    "NA"
  )
})

# ---------------------------------------------------------------------------
# facet_var and facet_num_cols
# ---------------------------------------------------------------------------

test_that("facet_var with facet_num_cols succeeds", {
  expect_no_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      facet_var      = "site",
      facet_num_cols = 2L
    )
  )
})

test_that("facet_var without facet_num_cols succeeds", {
  expect_no_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      facet_var      = "site"
    )
  )
})

test_that("facet_num_cols set but facet_var NULL is silently ignored", {
  expect_no_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      facet_var      = NULL,
      facet_num_cols = 2L
    )
  )
})

test_that("facet_num_cols <= 0 stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      facet_var      = "site",
      facet_num_cols = 0L
    ),
    "positive whole number"
  )
})

test_that("non-integer facet_num_cols stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      facet_var      = "site",
      facet_num_cols = 1.5
    ),
    "positive whole number"
  )
})

# ---------------------------------------------------------------------------
# plot_theme
# ---------------------------------------------------------------------------

test_that("valid ggplot2 theme succeeds", {
  expect_no_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      plot_theme     = ggplot2::theme_bw()
    )
  )
})

test_that("plot_theme NULL (default) succeeds", {
  expect_no_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      plot_theme     = NULL
    )
  )
})

test_that("non-theme plot_theme stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      plot_theme     = "theme_bw"
    ),
    "ggplot2 theme"
  )
})

# ---------------------------------------------------------------------------
# Input validation — df
# ---------------------------------------------------------------------------

test_that("non-data-frame df stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = list(year = 2020, cover = 10),
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species"
    ),
    "data frame"
  )
})

# ---------------------------------------------------------------------------
# Input validation — column names
# ---------------------------------------------------------------------------

test_that("y_var not in df stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "nonexistent",
      y_label        = "Percent cover",
      y_grouping_var = "species"
    ),
    "nonexistent"
  )
})

test_that("x_var not in df stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      x_var          = "nonexistent",
      y_label        = "Percent cover",
      y_grouping_var = "species"
    ),
    "nonexistent"
  )
})

test_that("y_grouping_var not in df stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "nonexistent"
    ),
    "nonexistent"
  )
})

test_that("facet_var not in df stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species",
      facet_var      = "nonexistent"
    ),
    "nonexistent"
  )
})

# ---------------------------------------------------------------------------
# Input validation — non-character scalar params
# ---------------------------------------------------------------------------

test_that("non-character y_var stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = 1,
      y_label        = "Percent cover",
      y_grouping_var = "species"
    ),
    "character"
  )
})

test_that("non-character y_grouping_var stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = 1
    ),
    "character"
  )
})

test_that("non-character y_label stops with informative error", {
  expect_error(
    viz_mg_timeseries_annual(
      df             = .ts_df,
      y_var          = "cover",
      y_label        = 123,
      y_grouping_var = "species"
    ),
    "character"
  )
})

# ---------------------------------------------------------------------------
# NA values in data columns
# ---------------------------------------------------------------------------

test_that("NA values in y_var column do not error", {
  df_na <- .ts_df
  df_na$cover[1] <- NA_real_

  expect_no_error(
    viz_mg_timeseries_annual(
      df             = df_na,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species"
    )
  )
})

test_that("NA values in y_grouping_var column do not error", {
  df_na <- .ts_df
  df_na$species[1] <- NA_character_

  expect_no_error(
    viz_mg_timeseries_annual(
      df             = df_na,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species"
    )
  )
})

# ---------------------------------------------------------------------------
# Edge cases — empty data frame
# ---------------------------------------------------------------------------

test_that("zero-row data frame does not error", {
  df_empty <- .ts_df[0, ]

  expect_no_error(
    viz_mg_timeseries_annual(
      df             = df_empty,
      y_var          = "cover",
      y_label        = "Percent cover",
      y_grouping_var = "species"
    )
  )
})

