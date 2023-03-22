testthat::test_that("test get_commit_hash", {

  # Get commit hash of lpjmlkit repository
  commit_hash <- get_git_urlhash(
    path = getwd(),
    include_url = FALSE,
    raise_error = FALSE
  )
  # Some testing instances do return commit_hashes
  if (nchar(commit_hash) > 0) {
    # Check typical lower case letter and digit combination
    testthat::expect_true(
      grepl(
        "^[[:lower:][:digit:]]+$",
        commit_hash
      )
    )
  } else {
    # Check typical lower case letter and digit combination
    testthat::expect_true(commit_hash == "")
  }
})


testthat::test_that("raise get_commit_hash error", {

  # Directory above lpjmlkit should not be a git repository
  testthat::expect_error(
    get_git_urlhash(
      path = "../../../",
      include_url = FALSE,
      raise_error = TRUE
    ),
    "For path"
  )
})


testthat::test_that("raise make_lpjml errors", {

  skip_on_os("windows")

  # Emulate as if model has not been compiled before
  testthat::expect_true(
    make_lpjml(
      raise_error = FALSE
    )$status != 0
  )

  # Emulate as if model has been compiled before
  testthat::expect_true(
    make_lpjml(
      model_path = "../testdata",
      debug = TRUE,
      raise_error = FALSE
    )$status != 0
  )
})


testthat::test_that("raise check_config errors", {

  skip_on_os("windows")

  # Emulate as if model has not been compiled before
  testthat::expect_true(
    check_config(
      x = "../testdata/config_spinup_pnv.json",
      return_output = TRUE
    )$status != 0,
  )

  # Emulate as if model has not been compiled before
  testthat::expect_output(
    check_config(
      x = "../testdata/config_spinup_pnv.json"
    ),
    "Please check for warnings"
  )

  # Emulate as if model has not been compiled before
  testthat::expect_true(
    check_config(
      x = c("../testdata/config_spinup.json",
            "../testdata/config_pnv.json"),
      return_output = TRUE
    )$status != 0,
  )
})
