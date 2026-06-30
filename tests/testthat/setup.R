# Default the whole test suite to the bundled (offline) registry data so tests
# never touch the network. Restored after the run via teardown_env(), so this
# does not leak into an interactive `devtools::test()` session.
#
# Tests that exercise the live fetch path opt back in with
# withr::local_options(marinegeo.utils.live_registry = TRUE).
withr::local_options(
  marinegeo.utils.live_registry = FALSE,
  .local_envir = testthat::teardown_env()
)
