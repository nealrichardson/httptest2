this_req <- request("http://httpbin.org/get") %>%
  req_body_json(list(test = TRUE)) %>%
  req_method("POST")

# Most of the behavior of these functions are tested elsewhere
# These tests are just for the non-standard behavior

with_mock_api({
  test_that("expect_request with mock API prints expected mock file if not found", {
    expect_error(
      req_perform(this_req),
      paste(
        "An unexpected request was made:",
        'POST http://httpbin.org/get {"test":true}',
        "Expected mock file: httpbin.org/get-f171f3-POST.*",
        sep = "\n"
      ),
      class = "httptest2_request",
      fixed = TRUE
    )
  })

  test_that("Request body and query are appended to the mock path", {
    expect_error(
      request("api/object2?d=1") %>%
        req_method("PATCH") %>%
        req_body_json(list(arg = 45)) %>%
        req_perform(),
      paste(
        "An unexpected request was made:",
        'PATCH api/object2?d=1 {"arg":45}',
        "Expected mock file: api/object2-899b0e-3d8d62-PATCH.*",
        sep = "\n"
      ),
      class = "httptest2_request",
      fixed = TRUE
    )
  })

  test_that("expect_VERB when no request is made", {
    suppressWarnings({
      # warnings are suppressed because of some spurious thing in testthat ... handling, will report issue
      expect_failure(
        expect_POST("just a string"),
        "No request was made"
      )
      # Actual errors are passed through
      expect_error(
        expect_failure(
          expect_POST(stop("NOTAREQUEST"))
        ),
        "NOTAREQUEST"
      )
    })
  })

  test_that("expect_no_request", {
    expect_no_request(rnorm(5))
    expect_failure(
      expect_no_request(
        request("http://httpbin.org/get") %>% req_perform()
      ),
      "An unexpected request was made"
    )
    # Make sure we aren't swallowing all errors
    expect_error(
      expect_no_request(stop("NOTAREQUEST")),
      "NOTAREQUEST"
    )
  })
})

without_internet({
  test_that("expect_request without_internet", {
    expect_failure(
      expect_POST(
        req_perform(this_req),
        "http://httpbin.org/get",
        '{"test":false}'
      ),
      paste(
        "An unexpected request was made:",
        'Actual:   POST http://httpbin.org/get {"test":true}',
        'Expected: POST http://httpbin.org/get {"test":false}',
        sep = "\n  "
      ),
      fixed = TRUE
    )
    # Error messages are different without internet (no mock file)
    expect_error(
      req_perform(this_req),
      paste(
        "An unexpected request was made:",
        'POST http://httpbin.org/get {"test":true}',
        sep = "\n"
      ),
      class = "httptest2_request",
      fixed = TRUE
    )
  })
})
