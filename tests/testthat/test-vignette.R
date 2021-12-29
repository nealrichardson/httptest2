we_are_recording <- function() inherits(httr2:::req_perform, "functionWithTrace")
we_are_mocking <- function() !is.null(getOption("httr2_mock"))
path <- tempfile()

test_that("start/end_vignette with recording (dir does not exist yet)", {
  start_vignette(path)
  expect_true(we_are_recording())
  expect_false(we_are_mocking())
  expect_identical(.mockPaths()[1], file.path(path, "0"))
  # Test that state_change bumps the mockPath
  change_state()
  expect_identical(.mockPaths()[1], file.path(path, "1"))
  expect_identical(.mockPaths()[2], file.path(path, "0"))
  end_vignette()
  expect_false(we_are_recording())
  expect_false(we_are_mocking())
  expect_false(any(grepl(path, .mockPaths()[1], fixed = TRUE)))
})

test_that("start/end_vignette with mocking (dir exists)", {
  on.exit(options(httptest2.verbose = NULL))
  dir.create(path)
  start_vignette(path)
  expect_false(we_are_recording())
  expect_true(we_are_mocking())
  end_vignette()
  expect_false(we_are_recording())
  expect_false(we_are_mocking())
})

test_that("start_vignette puts path in vignettes dir, if exists", {
  d <- tempfile()
  dir.create(file.path(d, "vignettes"), recursive = TRUE)
  old <- setwd(d)
  on.exit(setwd(old))

  start_vignette("testing")
  expect_identical(.mockPaths()[1], file.path("vignettes", "testing", "0"))
  end_vignette()
})

test_that("start/end_vignette calls inst/httptest/vignette-start/end.R", {
  lib <- install_testpkg("testpkg")
  library(testpkg, lib.loc = lib)
  on.exit(detach("package:testpkg", unload = TRUE))
  expect_false(getOption("testpkg.start.vignette", FALSE))
  start_vignette(path)
  expect_true(getOption("testpkg.start.vignette"))

  end_vignette()
  expect_false(getOption("testpkg.start.vignette", FALSE))
})

test_that("change_state validation", {
  with_mock_path("foo", {
    expect_error(change_state(), "foo is not valid for change_state()")
  })
})

reset_redactors()
