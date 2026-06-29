# ---------------------------------------------------------------------------
# .mg_get_registry_table — live fetch with bundled fallback
# ---------------------------------------------------------------------------

.obs_fixture <- data.frame(
  scientific_name = c("Zostera marina", "Halodule wrightii"),
  scientific_id = c("APHIA:1", "APHIA:2"),
  stringsAsFactors = FALSE
)

.bundled_obs <- data.frame(
  scientific_name = "Bundled species",
  scientific_id = "APHIA:999",
  stringsAsFactors = FALSE
)

.bundled_partners <- data.frame(
  partner_code = "USA-MDA",
  name = "Maryland",
  stringsAsFactors = FALSE
)

.mock_md <- list(
  observation_lookup = .bundled_obs,
  partner_codes = .bundled_partners
)

test_that("migrated table returns live data when the fetch succeeds", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_registry = function(url) .obs_fixture,
    .package = "marinegeo.utils"
  )

  result <- .mg_get_registry_table("observation_lookup")

  expect_equal(result, .obs_fixture)
})

test_that("migrated table falls back to bundled data and messages on fetch failure", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_registry = function(url) stop("offline"),
    .package = "marinegeo.utils"
  )

  expect_message(
    result <- .mg_get_registry_table("observation_lookup"),
    "using bundled fallback"
  )
  expect_equal(result, .bundled_obs)
})

test_that("non-migrated table returns bundled data without calling the fetcher", {
  local_mocked_bindings(
    marinegeo_metadata = .mock_md,
    .mg_fetch_registry = function(url) stop("fetcher should not be called"),
    .package = "marinegeo.utils"
  )

  result <- .mg_get_registry_table("partner_codes")

  expect_equal(result, .bundled_partners)
})
