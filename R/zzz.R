.onLoad <- function(libname, pkgname) {
  # Memoise bundle fetches for the lifetime of the session, keyed by source.
  # The manifest and each dist/ bundle are therefore fetched once, so a QC
  # run touching several tables in one bundle costs a single request. See
  # R/api_mg_get_registries.R for the resolver.
  .mg_fetch_json <<- memoise::memoise(.mg_read_json)
}

# Column names used as bare symbols by the aggregation checks in
# R/qc_mg_agg_*.R. dplyr's data masking makes these look like undefined globals
# to `R CMD check`, which reports them as "no visible binding for global
# variable".
#
# Declared here rather than rewritten as `.data[["point_count"]]`: bare symbols
# are what make a grouping read like the protocol it encodes, and readability is
# the reason these checks are authored in R rather than driven by a rule table.
#
# Deliberately scoped to the aggregation files. The package has other
# unregistered symbols from before this list existed; they are a separate
# cleanup, not something to fold in here.
utils::globalVariables(c(
  "cover_method",
  "cover_type",
  "deployment_period",
  "expected",
  "n_expected",
  "observed",
  "panel_id",
  "point_count",
  "points_in_grid",
  "points_in_quadrat",
  "quadrat",
  "sample_collection_date",
  "sample_event_id",
  "transect"
))
