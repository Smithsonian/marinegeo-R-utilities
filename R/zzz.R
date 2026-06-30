.onLoad <- function(libname, pkgname) {
  # Memoise live registry fetches for the lifetime of the session. See
  # R/api_mg_get_registries.R for the fetch functions and resolver. The tree
  # listing is cached once per session; CSV reads are cached keyed by URL vector.
  .mg_fetch_tree <<- memoise::memoise(.mg_list_repo_tree)
  .mg_fetch_registry <<- memoise::memoise(.mg_read_remote_csv)
}
