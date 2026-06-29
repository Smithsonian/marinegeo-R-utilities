.onLoad <- function(libname, pkgname) {
  # Memoise live registry fetches for the lifetime of the session (cache keyed by
  # URL). See R/api_mg_get_registries.R for the fetch function and resolver.
  .mg_fetch_registry <<- memoise::memoise(.mg_read_remote_csv)
}
