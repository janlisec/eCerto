# to be used in vdiff based plot tests using snapshots
skip_if_not_linux_ci <- function() {
  #if (Sys.getenv("CI") == "true" && Sys.info()[["sysname"]] != "Linux") {
  if (Sys.getenv("CI") == "true") {
    testthat::skip("Skipping visual tests on CI for non-Linux platforms")
  }
}
