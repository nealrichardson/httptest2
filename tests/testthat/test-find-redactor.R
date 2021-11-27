expect_redactor <- function(expr) {
  expect_identical(names(formals(expr)), "response")
}

test_that("prepare_redactor: function", {
  expect_identical(prepare_redactor(redact_cookies), redact_cookies)
})

multiredact <- list(redact_cookies, ~ redact_headers(., "X-Header"))
test_that("prepare_redactor: list/multiple", {
  expect_redactor(prepare_redactor(multiredact))
  expect_identical(prepare_redactor(multiredact[1]), redact_cookies)
})

test_that("prepare_redactor: NULL for no redacting", {
  expect_identical(prepare_redactor(NULL), force)
})

test_that("prepare_redactor: garbage", {
  expect_error(
    prepare_redactor("foo"),
    "Redactor must be a function or list of functions"
  )
})

test_that("get_current_redactor edge cases", {
  options(httptest.redactor = NULL)
  expect_identical(get_current_redactor(), redact_auth)
  options(httptest.redactor.packages = "NOTAPACKAGE")
  expect_identical(get_current_redactor(), redact_auth)
})

with_mock_api({
  test_that("Reading redactors from within a package (and install that package)", {
    newmocks <- tempfile()
    testthat_transition(
      expect_message(
        capture_while_mocking(path = newmocks, {
          # Install the "testpkg" to a temp lib.loc _after_ we've
          # already started recording
          lib <- install_testpkg("testpkg")
          library(testpkg, lib.loc = lib)
          expect_true("testpkg" %in% names(sessionInfo()$otherPkgs))

          r <- GET("http://example.com/get")
        }),
        paste0("Using redact.R from ", dQuote("testpkg"))
      ),
      expect_message(
        expect_message(
          capture_while_mocking(path = newmocks, {
            # Install the "testpkg" to a temp lib.loc _after_ we've
            # already started recording
            lib <- install_testpkg("testpkg")
            library(testpkg, lib.loc = lib)
            expect_true("testpkg" %in% names(sessionInfo()$otherPkgs))
            r <- GET("http://example.com/get")
          }),
          paste0("Using redact.R from ", dQuote("testpkg"))
        ),
        paste0("Using request.R from ", dQuote("testpkg"))
      )
    )
    with_mock_path(newmocks, {
      r2 <- GET("http://example.com/get")
    })
    # The resulting mock content is what we injected into it from testpkg
    expect_identical(resp_body_json(r2), list(fake = TRUE))
  })

  test_that("Request preprocessing via package inst/httptest/request.R", {
    # That function prunes a leading http://pythong.org/ from URLs
    expect_identical(
      resp_body_json(GET("http://pythong.org/api/object1/")),
      resp_body_json(GET("api/object1/"))
    )
  })

  test_that("set_redactor(NULL) to override default (and loaded packages)", {
    expect_true("testpkg" %in% names(sessionInfo()$otherPkgs))
    # Great, but let's kill it when we're done
    on.exit(detach("package:testpkg", unload = TRUE))
    newmocks2 <- tempfile()
    skip("HTTR2: adapt httr::response to httr2::httr2_response")
    with_redactor(
      NULL,
      capture_while_mocking(simplify = FALSE, path = newmocks2, {
        a <- request("http://example.com/login") %>%
          req_body_json(list(username = "password")) %>%
          req_perform()
      })
    )
    # The auth token, which would have been removed (see test-redact.R),
    # is present in the file we wrote because we set NULL as redactor
    expect_true(any(grepl(
      "token=12345",
      readLines(file.path(newmocks2, "example.com", "login-712027-POST.R"))
    )))
  })

  test_that("Loading a package with pkgload (devtools)", {
    newmocks3 <- tempfile()
    expect_false("testpkg" %in% names(sessionInfo()$otherPkgs))
    on.exit(pkgload::unload("testpkg"))
    expect_message(
      capture_while_mocking(path = newmocks3, {
        pkgload::load_all("testpkg", quiet = TRUE)
        expect_true("testpkg" %in% names(sessionInfo()$otherPkgs))
        r <- GET("http://example.com/get")
      }),
      paste0("Using redact.R from ", dQuote("testpkg"))
    )
    with_mock_path(newmocks3, {
      r2 <- GET("http://example.com/get")
    })
    # The resulting mock content is what we injected into it from testpkg
    expect_identical(resp_body_json(r2), list(fake = TRUE))
  })
})

reset_redactors()
