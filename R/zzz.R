.onLoad <- function(libname, pkgname) {
  # Memoise artifact fetches for the lifetime of the session, keyed by source.
  # The manifest and each dist/ artifact are therefore fetched once, so a QC
  # run touching several tables in one artifact costs a single request. See
  # R/api_mg_get_registries.R for the resolver.
  .mg_fetch_json <<- memoise::memoise(.mg_read_json)
}
