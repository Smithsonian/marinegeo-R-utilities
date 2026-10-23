# ---------------------------------------------------------------------------
# Registry resolution from the generated dist/ artifacts
#
# Nothing here touches the network: the local-checkout path writes a small
# dist/ to a temp dir, and the live path mocks the single fetch seam
# (.mg_fetch_json).
# ---------------------------------------------------------------------------

.bundled_struct <- data.frame(
  protocol = "bundled",
  table_id = "bundled_v1",
  level = 2,
  column_name = "bundled_col",
  data_type = "STRING",
  missing_values = NA_character_,
  uuid_identity = NA,
  stringsAsFactors = FALSE
)

.bundled_index <- data.frame(
  table_id = "bundled_v1",
  protocol = "seagrass",
  stringsAsFactors = FALSE
)

.mock_md <- list(
  database_structure = .bundled_struct,
  data_index = .bundled_index,
  legacy_taxonomic_ids = data.frame(x = 1L)
)

# The live rows a fake artifact serves for database_structure.
.live_struct_rows <- list(
  list(
    protocol = "live",
    table_id = "live_v1",
    level = 2L,
    column_name = "a",
    data_type = "STRING",
    missing_values = "fail",
    uuid_identity = TRUE
  ),
  list(
    protocol = "live",
    table_id = "live_v1",
    level = 2L,
    column_name = "b",
    data_type = "INT",
    missing_values = NULL,
    uuid_identity = NULL
  )
)

.struct_schema <- list(
  rows = 2L,
  columns = list(
    list(name = "protocol", type = "character"),
    list(name = "table_id", type = "character"),
    list(name = "level", type = "double"),
    list(name = "column_name", type = "character"),
    list(name = "data_type", type = "character"),
    list(name = "missing_values", type = "character"),
    list(name = "uuid_identity", type = "logical")
  )
)

.fake_manifest <- function(schema_version = 2L) {
  list(
    schema_version = schema_version,
    artifacts = list(
      `qc-metadata` = list(
        path = "dist/qc-metadata.json",
        tables = list(database_structure = .struct_schema)
      )
    ),
    table_index = list(database_structure = "qc-metadata")
  )
}

.fake_artifact <- function() {
  list(
    schema_version = 2L,
    artifact = "qc-metadata",
    tables = list(database_structure = .live_struct_rows)
  )
}

# Serve the fake manifest/artifact through the one fetch seam.
.mock_fetch <- function(
  manifest = .fake_manifest(),
  artifact = .fake_artifact()
) {
  function(src) {
    if (grepl("manifest\\.json$", src)) {
      return(manifest)
    }
    if (grepl("qc-metadata\\.json$", src)) {
      return(artifact)
    }
    stop("unexpected fetch: ", src)
  }
}

# Write a real dist/ tree so the local path is exercised end to end.
.write_fake_dist <- function(
  root,
  manifest = .fake_manifest(),
  artifact = .fake_artifact()
) {
  dir.create(file.path(root, "dist"), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    jsonlite::toJSON(manifest, auto_unbox = TRUE, null = "null"),
    file.path(root, "dist", "manifest.json")
  )
  writeLines(
    jsonlite::toJSON(artifact, auto_unbox = TRUE, null = "null"),
    file.path(root, "dist", "qc-metadata.json")
  )
  root
}

# ---------------------------------------------------------------------------
# Options
# ---------------------------------------------------------------------------

test_that("metadata ref defaults to main and flows into the raw URL", {
  withr::local_options(marinegeo.utils.metadata_ref = NULL)

  expect_equal(.mg_metadata_ref(), "main")
  expect_match(.mg_raw_base(), "marinegeo-metadata/main/$")
  expect_match(
    .mg_file_url("dist/manifest.json"),
    "/main/dist/manifest\\.json$"
  )
})

test_that("metadata ref option retargets the raw URL", {
  withr::local_options(marinegeo.utils.metadata_ref = "qc-update")

  expect_equal(.mg_metadata_ref(), "qc-update")
  expect_match(
    .mg_file_url("dist/manifest.json"),
    "/qc-update/dist/manifest\\.json$"
  )
})

test_that("an invalid metadata ref errors rather than building a bad URL", {
  withr::local_options(marinegeo.utils.metadata_ref = "")
  expect_error(.mg_metadata_ref(), "non-empty character scalar")

  withr::local_options(marinegeo.utils.metadata_ref = c("a", "b"))
  expect_error(.mg_metadata_ref(), "non-empty character scalar")
})

test_that("an invalid local path errors", {
  withr::local_options(marinegeo.utils.metadata_local = "")
  expect_error(.mg_metadata_local(), "non-empty character scalar")
})

test_that("strict registry defaults off", {
  withr::local_options(marinegeo.utils.strict_registry = NULL)
  expect_false(.mg_strict_registry())

  withr::local_options(marinegeo.utils.strict_registry = TRUE)
  expect_true(.mg_strict_registry())
})

# ---------------------------------------------------------------------------
# Schema version
# ---------------------------------------------------------------------------

test_that("a readable schema version passes", {
  expect_equal(.mg_check_schema_version(list(schema_version = 2L), "x"), 2L)
})

test_that("a pre-rename schema version is a loud error", {
  # Schema 1 spells the missing-value rule `enforce`, which this package
  # filters out entirely -- a clean pass having checked nothing.
  expect_error(
    .mg_check_schema_version(list(schema_version = 1L), "dist/manifest.json"),
    "requires at least"
  )
})

test_that("a newer major schema version is a loud error", {
  # Continuing would run QC against rules this package cannot read, and the
  # engine treats an unreadable rule table as "no rules" -- a clean pass with
  # zero checks.
  expect_error(
    .mg_check_schema_version(list(schema_version = 99L), "dist/manifest.json"),
    "Upgrade marinegeo.utils"
  )
})

test_that("a missing or unreadable schema version is an error", {
  expect_error(.mg_check_schema_version(list(), "x"), "no schema_version")
  expect_error(
    .mg_check_schema_version(list(schema_version = "zzz"), "x"),
    "unreadable schema_version"
  )
})

# ---------------------------------------------------------------------------
# Typed coercion
# ---------------------------------------------------------------------------

test_that("columns are built with their declared types", {
  out <- .mg_coerce_registry_table(
    .live_struct_rows,
    .struct_schema,
    "database_structure"
  )

  expect_s3_class(out, "tbl_df")
  expect_identical(colnames(out), .mg_flat_schema$database_structure)
  expect_true(is.character(out$protocol))
  expect_true(is.double(out$level))
  # JSON cannot distinguish null from a typed NA, so types come from the
  # manifest. Inferring them would make an all-null column the wrong type.
  expect_true(is.logical(out$uuid_identity))
  expect_silent(which(out$uuid_identity))
})

test_that("a JSON null becomes a typed NA, not an empty string", {
  out <- .mg_coerce_registry_table(
    .live_struct_rows,
    .struct_schema,
    "database_structure"
  )

  expect_identical(out$missing_values, c("fail", NA_character_))
  expect_identical(out$uuid_identity, c(TRUE, NA))
  expect_false(any(!is.na(out$missing_values) & out$missing_values == ""))
})

test_that("a row count that disagrees with the manifest is an error", {
  schema <- .struct_schema
  schema$rows <- 5L

  expect_error(
    .mg_coerce_registry_table(.live_struct_rows, schema, "database_structure"),
    "has 2 rows but the manifest declares 5"
  )
})

test_that("a QC table missing a required column is an error", {
  schema <- .struct_schema
  schema$columns <- schema$columns[1:3]
  rows <- lapply(.live_struct_rows, function(r) {
    r[c("protocol", "table_id", "level")]
  })

  # Downstream this would be read as "no rules" and silently skip the check.
  expect_error(
    .mg_coerce_registry_table(rows, schema, "database_structure"),
    "missing required column"
  )
})

test_that("an unknown declared type is an error", {
  schema <- list(rows = 1L, columns = list(list(name = "a", type = "complex")))

  expect_error(
    .mg_coerce_registry_table(list(list(a = 1)), schema, "whatever"),
    "unknown declared type"
  )
})

test_that("a table absent from its artifact is an error", {
  expect_error(
    .mg_coerce_registry_table(NULL, .struct_schema, "database_structure"),
    "missing from its artifact"
  )
})

# ---------------------------------------------------------------------------
# Resolution
# ---------------------------------------------------------------------------

test_that("a local checkout is read from its own dist/ on disk", {
  root <- .write_fake_dist(withr::local_tempdir())
  withr::local_options(
    marinegeo.utils.metadata_local = root,
    marinegeo.utils.live_registry = TRUE
  )

  # The local path shares the memoised fetch seam; what distinguishes it is
  # that the seam is handed a file path under the checkout, never a URL.
  seen <- character(0)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_json = function(src) {
      seen <<- c(seen, src)
      if (grepl("^https?://", src)) {
        stop("resolved to the network despite a local checkout: ", src)
      }
      jsonlite::fromJSON(src, simplifyVector = FALSE)
    },
    .package = "marinegeo.utils"
  )

  out <- .mg_get_registry_table("database_structure")

  expect_true(all(startsWith(seen, root)))
  expect_setequal(basename(seen), c("manifest.json", "qc-metadata.json"))
  expect_identical(out$protocol, c("live", "live"))
  expect_identical(out$uuid_identity, c(TRUE, NA))
})

test_that("a local checkout with no dist/ errors instead of falling back", {
  root <- withr::local_tempdir()
  withr::local_options(marinegeo.utils.metadata_local = root)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_json = .mock_fetch(),
    .package = "marinegeo.utils"
  )

  # Silently serving bundled rules would validate against the wrong metadata,
  # which is the failure this override exists to prevent.
  expect_error(
    .mg_get_registry_table("database_structure"),
    "Run R/generate-metadata-dist.R"
  )
})

test_that("offline mode returns bundled data without fetching", {
  withr::local_options(marinegeo.utils.live_registry = FALSE)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_json = function(src) stop("should not fetch"),
    .package = "marinegeo.utils"
  )

  expect_equal(.mg_get_registry_table("database_structure"), .bundled_struct)
})

test_that("a live fetch returns the typed artifact table", {
  withr::local_options(
    marinegeo.utils.live_registry = TRUE,
    marinegeo.utils.metadata_local = NULL
  )
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_json = .mock_fetch(),
    .package = "marinegeo.utils"
  )

  out <- .mg_get_registry_table("database_structure")

  expect_identical(out$protocol, c("live", "live"))
  expect_identical(nrow(out), 2L)
})

test_that("a table the manifest does not publish falls back silently", {
  withr::local_options(
    marinegeo.utils.live_registry = TRUE,
    marinegeo.utils.metadata_local = NULL
  )
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_json = .mock_fetch(),
    .package = "marinegeo.utils"
  )

  # Not an error and not a warning: the bundled copy is the correct source for
  # a table dist/ does not carry.
  expect_silent(out <- .mg_get_registry_table("legacy_taxonomic_ids"))
  expect_equal(out, .mock_md$legacy_taxonomic_ids)
})

test_that("a failed fetch warns, names the cause, and falls back", {
  withr::local_options(
    marinegeo.utils.live_registry = TRUE,
    marinegeo.utils.metadata_local = NULL,
    marinegeo.utils.metadata_ref = "some-ref"
  )
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_json = function(src) stop("connection refused"),
    .package = "marinegeo.utils"
  )

  # A warning, not a message: a message is invisible in a non-interactive
  # script, which is exactly where a silent fallback does the most harm. The
  # underlying cause is included rather than discarded.
  expect_warning(
    out <- .mg_get_registry_table("database_structure"),
    "connection refused"
  )
  expect_warning(
    .mg_get_registry_table("database_structure"),
    "some-ref"
  )
  expect_equal(out, .bundled_struct)
})

test_that("strict mode turns a failed fetch into an error", {
  withr::local_options(
    marinegeo.utils.live_registry = TRUE,
    marinegeo.utils.metadata_local = NULL,
    marinegeo.utils.strict_registry = TRUE
  )
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_json = function(src) stop("connection refused"),
    .package = "marinegeo.utils"
  )

  # Falling back is right for a field user offline and exactly wrong for a job
  # whose purpose is to validate metadata.
  expect_error(
    .mg_get_registry_table("database_structure"),
    "connection refused"
  )
})

test_that("a newer schema version is not swallowed by the fallback in strict mode", {
  withr::local_options(
    marinegeo.utils.live_registry = TRUE,
    marinegeo.utils.metadata_local = NULL,
    marinegeo.utils.strict_registry = TRUE
  )
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_json = .mock_fetch(
      manifest = .fake_manifest(schema_version = 99L)
    ),
    .package = "marinegeo.utils"
  )

  expect_error(
    .mg_get_registry_table("database_structure"),
    "Upgrade marinegeo.utils"
  )
})

test_that("resolving several tables from one artifact fetches it once", {
  withr::local_options(
    marinegeo.utils.live_registry = TRUE,
    marinegeo.utils.metadata_local = NULL
  )
  seen <- character(0)
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_json = local({
      inner <- .mock_fetch()
      memoise::memoise(function(src) {
        seen <<- c(seen, src)
        inner(src)
      })
    }),
    .package = "marinegeo.utils"
  )

  .mg_get_registry_table("database_structure")
  .mg_get_registry_table("database_structure")

  # The manifest and the artifact, once each -- the reason the fetch seam is
  # memoised on its source.
  expect_identical(length(unique(seen)), 2L)
  expect_identical(length(seen), 2L)
})
