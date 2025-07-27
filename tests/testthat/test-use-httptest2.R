test_add_to_desc <- function(str, msg = "Adding 'httptest2' to Suggests") {
  f <- tempfile()
  cat(str, file = f)
  expect_message(add_httptest2_to_desc(f), msg)
  setNames(read.dcf(f, keep.white = "Suggests")[, "Suggests"], NULL)
}

test_that("add to desc adds Suggests if not present", {
  expect_identical(test_add_to_desc("Title: Foo"), "httptest2")
})

test_that("add to desc adds Suggests if present but empty", {
  expect_identical(test_add_to_desc("Suggests:"), "httptest2")
})

test_that("add to desc adds Suggests inline", {
  expect_identical(test_add_to_desc("Suggests: pkg"), "httptest2, pkg")
  expect_identical(
    test_add_to_desc("Suggests: pkg, alpha (>= 2.0.0)"),
    "alpha (>= 2.0.0), httptest2, pkg"
  )
})

desc_suggests_one_multiline <- "Title: Foo
Suggests:
    pkg
"
desc_suggests_two_multiline <- "Title: Foo
Suggests:
    pkg,
    alpha (>= 2.0.0)
"
desc_suggests_two_uneven <- "Title: Foo
Suggests: pkg,
    alpha (>= 2.0.0)
"

test_that("add to desc adds Suggests multiline", {
  expect_identical(
    test_add_to_desc(desc_suggests_one_multiline),
    "\n    httptest2,\n    pkg"
  )
  expect_identical(
    test_add_to_desc(desc_suggests_two_multiline),
    "\n    alpha (>= 2.0.0),\n    httptest2,\n    pkg"
  )
  expect_identical(
    test_add_to_desc(desc_suggests_two_uneven),
    "alpha (>= 2.0.0),\n    httptest2,\n    pkg"
  )
})

test_that("add to desc doesn't add if already present", {
  expect_identical(
    test_add_to_desc("Suggests: httptest2", msg = NA),
    "httptest2"
  )
  expect_identical(
    test_add_to_desc("Suggests: pkg, httptest2", msg = NA),
    "pkg, httptest2"
  )
})

expect_added_to_setup <- function(str, msg = "Adding library\\(httptest2\\)") {
  f <- tempfile()
  cat(str, file = f)
  expect_message(add_httptest2_to_setup(f), msg)
  expect_true(any(grepl("library(httptest2)", readLines(f), fixed = TRUE)))
}

test_that("add to setup creates file if doesn't exist", {
  f <- tempfile()
  expect_false(file.exists(f))
  expect_message(
    expect_message(add_httptest2_to_setup(f), "Creating"),
    "Adding library\\(httptest2\\) to"
  )
  expect_identical(readLines(f), "library(httptest2)")
})

test_that("add to setup adds", {
  expect_added_to_setup("")
  expect_added_to_setup("library(pkg)\n")
})

test_that("add to setup doesn't duplicate", {
  expect_added_to_setup("library(httptest2)\n", msg = NA)
})

test_that("use_httptest2 integration test", {
  testpkg <- tempfile()
  dir.create(testpkg)
  expect_error(use_httptest2(testpkg), "is not an R package directory")

  desc <- file.path(testpkg, "DESCRIPTION")
  cat("Title: Foo\n", file = desc)
  setup <- file.path(testpkg, "tests", "testthat", "setup.R")
  expect_message(
    expect_message(
      expect_message(
        use_httptest2(testpkg),
        "Adding 'httptest2' to Suggests"
      ),
      "Creating "
    ),
    "Adding library\\(httptest2\\) to "
  )
  expect_identical(readLines(desc), c("Title: Foo", "Suggests: httptest2"))
  expect_identical(readLines(setup), "library(httptest2)")
  # It does nothing if you the package already uses httptest2
  expect_message(use_httptest2(testpkg), NA)
})
