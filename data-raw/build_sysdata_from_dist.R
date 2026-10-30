# Rebuild R/sysdata.rda from the metadata repo's generated dist/ bundles.
#
# Run from the package root:
#   Rscript data-raw/build_sysdata_from_dist.R
#
# sysdata.rda is the OFFLINE FALLBACK for the registry tables. It is not an
# independent copy of the metadata: it must agree with what the live path
# serves, or an offline user silently validates against different rules than an
# online one.
#
# So the tables are read through the package's own resolver with
# `marinegeo.utils.metadata_local` pointed at a local metadata checkout. Reading
# dist/ directly would risk reproducing the types slightly differently; going
# through .mg_get_registry_table() means the in-package copy is byte-for-byte what
# a live fetch produces, because it is produced by the same code.
#
# The two legacy_* tables are not published in dist/ -- they come from this
# repo's own data-raw/legacy_taxonomic_data_rls/ -- so they are carried over
# from the existing sysdata.rda unchanged.
#
# NOTE: the carry-over logic below keeps only the tables this repo owns. Any
# other element of the old object that dist/ no longer publishes is a retired
# rule table and is dropped. Going from the table-keyed rule schema to the
# `column_id`-keyed one, that is `categorical_values`, `qc_conditions`,
# `aggregate_rules` and `cross_table_rules` out, and `column_ids`,
# `controlled_vocabularies` and `column_lookups` in -- `numeric_ranges` keeps
# its name and is replaced in place with the re-keyed schema, which is why the
# count falls from 14 to 13 rather than to 12.

# `repository_filepath` follows the convention the retired assembly script set:
# it points at the PARENT directory holding the sibling repos, not at
# marinegeo-metadata itself. path.expand handles a leading "~".
.repos <- path.expand(Sys.getenv("repository_filepath", NA_character_))
metadata_repo <- if (!is.na(.repos) && nzchar(.repos)) {
  file.path(.repos, "marinegeo-metadata")
} else {
  file.path(dirname(normalizePath(".")), "marinegeo-metadata")
}
if (!dir.exists(file.path(metadata_repo, "dist"))) {
  stop(
    "no dist/ found at '",
    metadata_repo,
    "'. Set repository_filepath to the ",
    "directory holding marinegeo-metadata, and run R/generate-metadata-dist.R ",
    "there first.",
    call. = FALSE
  )
}
message("metadata repo: ", metadata_repo)

suppressMessages(pkgload::load_all(".", quiet = TRUE))
ns <- asNamespace("marinegeo.utils")
get_table <- get(".mg_get_registry_table", envir = ns)

# Carry over the tables dist/ does not publish, and the canonical element order.
prev <- new.env(parent = emptyenv())
load("R/sysdata.rda", envir = prev)
old <- prev$marinegeo_metadata

manifest <- jsonlite::fromJSON(
  file.path(metadata_repo, "dist", "manifest.json"),
  simplifyVector = FALSE
)
published <- names(manifest$table_index)

# The only tables with no published source are the legacy RLS ones built here
# from data-raw/legacy_taxonomic_data_rls/. Everything else the old object
# holds but dist/ no longer publishes is a retired rule table, and dropping it
# is the point of the rebuild -- so carrying over `setdiff(old, published)`
# wholesale would silently preserve exactly what should go.
carried <- grep("^legacy_", names(old), value = TRUE)
retired <- setdiff(names(old), c(published, carried))

# A newly published table is absent from the old object, so the canonical order
# has to be a union of the two rather than the old names alone -- otherwise the
# new tables are dropped by the `[element_order]` subset below.
element_order <- c(
  intersect(names(old), published),
  setdiff(published, names(old)),
  carried
)

message("from dist/: ", paste(published, collapse = ", "))
message("carried over: ", paste(carried, collapse = ", "))
if (length(retired) > 0) {
  message("dropped (no longer published): ", paste(retired, collapse = ", "))
}

withr::with_options(
  list(
    marinegeo.utils.metadata_local = metadata_repo,
    marinegeo.utils.strict_registry = TRUE
  ),
  {
    fresh <- lapply(published, get_table)
    names(fresh) <- published
  }
)

marinegeo_metadata <- c(fresh, old[carried])[element_order]
stopifnot(
  !anyNA(names(marinegeo_metadata)),
  identical(names(marinegeo_metadata), element_order),
  all(vapply(marinegeo_metadata, is.data.frame, logical(1)))
)

for (nm in element_order) {
  d <- marinegeo_metadata[[nm]]
  o <- old[[nm]]
  flag <- if (is.null(o)) {
    "  (new)"
  } else if (!identical(dim(d), dim(o))) {
    sprintf("  (was %d x %d)", nrow(o), ncol(o))
  } else {
    ""
  }
  message(sprintf("  %-34s %5d x %2d%s", nm, nrow(d), ncol(d), flag))
}

usethis::use_data(marinegeo_metadata, internal = TRUE, overwrite = TRUE)
message("wrote R/sysdata.rda")
