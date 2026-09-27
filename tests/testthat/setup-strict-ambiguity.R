# Error on any grammar ambiguity instead of letting dparser break the tie (#51)
withr::local_envvar(
  MONOLIX2RX_STRICT_AMBIGUITY = "true",
  .local_envir = testthat::teardown_env()
)
