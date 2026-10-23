# ---------------------------------------------------------------------------
# Behavioural round trip: identical QC output from either metadata source
#
# The structural round-trip test (test-api_mg_flatten_table_metadata.R) proves
# the flattened tibbles match the bundled ones. This proves the thing that
# actually matters: qc_run() reports the same issues either way. It catches
# semantic drift the structural test cannot -- an "" that slipped in where NA
# belonged still type-checks as character, but changes which rules fire.
# ---------------------------------------------------------------------------

.flat_metadata <- function() {
  skip_if_not_installed("yaml")
  flat <- .mg_flatten_table_metadata(
    .mg_parse_table_metadata_dir(test_path("fixtures/table-metadata"))
  )
  # Everything outside table-metadata/ (taxonomy and site/partner lookups) is
  # unaffected by the migration and still comes from the bundled object.
  meta <- as.list(marinegeo_metadata)
  meta[names(flat)] <- flat
  meta
}

# Build a fixture frame for a table from its own declared structure, so the
# column set and order are always correct and the test does not go stale when
# a protocol gains a column.
.make_fixture <- function(table_id, n = 4L) {
  ds <- marinegeo_metadata$database_structure
  ds <- ds[ds$table_id == table_id, , drop = FALSE]
  stopifnot(nrow(ds) > 0)

  cols <- lapply(seq_len(nrow(ds)), function(i) {
    switch(
      toupper(ds$data_type[[i]]),
      INT = seq_len(n),
      TINYINT = seq_len(n),
      DOUBLE = as.numeric(seq_len(n)),
      DATE = rep(as.Date("2026-06-01"), n),
      BOOL = rep(c(TRUE, FALSE), length.out = n),
      rep("x", n)
    )
  })
  names(cols) <- ds$column_name
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

# Same issues, independent of the row order the rule tables happen to have.
.canon_issues <- function(x) {
  d <- as.data.frame(x)
  attr(d, "spec") <- NULL
  attr(d, "problems") <- NULL
  if (nrow(d) == 0L) {
    return(d)
  }
  ord <- do.call(
    order,
    lapply(c("check", "column", "row", "rule_id", "issue"), function(k) {
      as.character(d[[k]])
    })
  )
  d <- d[ord, , drop = FALSE]
  rownames(d) <- NULL
  d
}

.expect_same_qc <- function(table_id, mutate = identity) {
  flat_meta <- .flat_metadata()
  data <- mutate(.make_fixture(table_id))

  from_bundled <- withr::with_options(
    list(marinegeo.utils.live_registry = FALSE),
    qc_run(data, table_id = table_id)
  )

  from_yaml <- local({
    local_mocked_bindings(
      marinegeo_metadata = flat_meta,
      .package = "marinegeo.utils"
    )
    withr::with_options(
      list(marinegeo.utils.live_registry = FALSE),
      qc_run(data, table_id = table_id)
    )
  })

  expect_equal(.canon_issues(from_yaml), .canon_issues(from_bundled))
  expect_identical(attr(from_yaml, "status"), attr(from_bundled, "status"))
  expect_identical(
    sort(attr(from_yaml, "checks_run")),
    sort(attr(from_bundled, "checks_run"))
  )
  invisible(from_yaml)
}

# ---------------------------------------------------------------------------

test_that("the oyster substrate table reports identically (conditional aggregate)", {
  # The only table exercising a conditional aggregate: the `primary_points`
  # condition excludes canopy taxa from the point total.
  out <- .expect_same_qc("oyster-substrate-composition-monitoring-v1")
  expect_qc_issues(out)
})

test_that("the seagrass cover table reports identically (vocab + ranges + aggregates)", {
  out <- .expect_same_qc("seagrass-cover-monitoring-v1")
  expect_qc_issues(out)
})

test_that("the seagrass leaf table reports identically (conditional range)", {
  # Carries the `zostera_leaves` conditional numeric range alongside an
  # unconditional floor on the same column.
  out <- .expect_same_qc("seagrass-leaf-monitoring-v1")
  expect_qc_issues(out)
})

test_that("the oyster count table reports identically", {
  out <- .expect_same_qc("oyster-count-monitoring-v1")
  expect_qc_issues(out)
})

test_that("the reef life survey table reports identically (numeric column names)", {
  out <- .expect_same_qc("reef-life-survey-data-marinegeo-v1")
  expect_qc_issues(out)
})

test_that("violations are reported identically, not just clean data", {
  # A fixture that passes everything would not distinguish the two sources.
  .expect_same_qc("seagrass-cover-monitoring-v1", function(d) {
    d$cover_method <- "not a real method"
    d$percent_cover <- c(-5, 50, 250, 99)
    d$cover_code <- c("0.1", "99", NA, "2")
    d$cover_quadrat_dimensions <- "7x7cm"
    d$site_name <- NA_character_
    d
  })
})

test_that("a conditional range fires identically on both sources", {
  .expect_same_qc("seagrass-leaf-monitoring-v1", function(d) {
    # Zostera rows get the tighter 20-1200 warn band; the others only the
    # unconditional min_fail floor.
    d$scientific_name <- c(
      "Zostera marina",
      "Zostera marina",
      "Halophila ovalis",
      "x"
    )
    d$leaf_length_mm <- c(5, 5000, 5, -1)
    d
  })
})

test_that("a conditional aggregate fires identically on both sources", {
  .expect_same_qc("oyster-substrate-composition-monitoring-v1", function(d) {
    d$sample_event_id <- "e1"
    d$transect <- 1L
    d$quadrat <- 1L
    d$point_count <- c(10L, 10L, 10L, 10L)
    d$points_in_quadrat <- 100L
    d
  })
})

test_that("every table in the registry reports identically", {
  # The broad sweep: whatever issues each table produces, both metadata
  # sources must produce the same ones.
  flat_meta <- .flat_metadata()
  ids <- unique(marinegeo_metadata$database_structure$table_id)
  # The five registry self-description tables are lookups, not submittable
  # tables, and carry no QC rules. They are the tables declared by
  # marinegeo-metadata-tables.yaml, across both protocols it names.
  lookup_protocols <- c("taxonomy-and-functional-groups", "sites-and-partners")
  ids <- setdiff(
    ids,
    unique(
      marinegeo_metadata$database_structure$table_id[
        marinegeo_metadata$database_structure$protocol %in% lookup_protocols
      ]
    )
  )

  for (id in ids) {
    data <- .make_fixture(id)

    a <- withr::with_options(
      list(marinegeo.utils.live_registry = FALSE),
      suppressWarnings(qc_run(data, table_id = id))
    )
    b <- local({
      local_mocked_bindings(
        marinegeo_metadata = flat_meta,
        .package = "marinegeo.utils"
      )
      withr::with_options(
        list(marinegeo.utils.live_registry = FALSE),
        suppressWarnings(qc_run(data, table_id = id))
      )
    })

    expect_equal(.canon_issues(b), .canon_issues(a), info = id)
    expect_identical(attr(b, "status"), attr(a, "status"), info = id)
  }
})

test_that("a multi-table dataset run reports identically (cross-table rules)", {
  flat_meta <- .flat_metadata()
  tables <- list(
    `seagrass-leaf-monitoring-v1` = .make_fixture(
      "seagrass-leaf-monitoring-v1"
    ),
    `seagrass-cover-monitoring-v1` = .make_fixture(
      "seagrass-cover-monitoring-v1"
    ),
    `shoot-count-monitoring-v1` = .make_fixture("shoot-count-monitoring-v1")
  )

  a <- withr::with_options(
    list(marinegeo.utils.live_registry = FALSE),
    suppressWarnings(qc_run_dataset(tables))
  )
  b <- local({
    local_mocked_bindings(
      marinegeo_metadata = flat_meta,
      .package = "marinegeo.utils"
    )
    withr::with_options(
      list(marinegeo.utils.live_registry = FALSE),
      suppressWarnings(qc_run_dataset(tables))
    )
  })

  expect_equal(.canon_issues(b), .canon_issues(a))
  expect_identical(attr(b, "status"), attr(a, "status"))
  expect_identical(sort(attr(b, "checks_run")), sort(attr(a, "checks_run")))
})
