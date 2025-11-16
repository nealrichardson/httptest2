with_mock_api({
  test_that("expect_request_header with mock API", {
    expect_request_header(
      request("https://test.api/object1/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_perform(),
      accept = "image/jpeg"
    )
    expect_request_header(
      request("https://test.api/object1/") %>%
        req_headers(Accept = "image/jpeg", `X-Stuff` = "more") %>%
        req_perform(),
      accept = ""
    )
    expect_request_header(
      request("https://test.api/object1/") %>%
        req_headers(Accept = "image/jpeg", `X-Stuff` = "more") %>%
        req_perform(),
      Accept = "image/jpeg",
      `X-stuff` = "mo",
      `x-not-present` = NULL
    )

    # These tests have been updated for testthat >= 3.3.0
    # Remove this skip once that has fully rolled out
    skip_if_not_installed("testthat", "3.3.0")

    expect_snapshot_failure(
      expect_request_header(
        request("https://test.api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform(),
        accept = "image/jpeg"
      )
    )
    expect_snapshot_failure(
      expect_request_header(
        request("https://test.api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform(),
        accept = NULL
      )
    )
  })

  test_that("Args passed to expect_match", {
    expect_request_header(
      request("https://test.api/object1/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_perform(),
      accept = "image.*"
    )
    expect_failure(
      expect_request_header(
        request("https://test.api/object1/") %>%
          req_headers(Accept = "image/jpeg") %>%
          req_perform(),
        accept = "image.*",
        fixed = TRUE
      )
    )
    expect_request_header(
      request("https://test.api/object1/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_perform(),
      accept = "IMAGE",
      ignore.case = TRUE
    )
    expect_failure(
      expect_request_header(
        request("https://test.api/object1/") %>%
          req_headers(Accept = "image/jpeg") %>%
          req_perform(),
        accept = "IMAGE"
      )
    )
    expect_request_header(
      request("https://test.api/object1/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_perform(),
      accept = "image.*",
      perl = TRUE
    )
    expect_request_header(
      request("https://test.api/object1/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_perform(),
      accept = "image",
      useBytes = TRUE
    )
  })

  test_that("Input validation", {
    expect_error(
      expect_request_header(
        request("https://test.api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform()
      ),
      "No headers provided"
    )
    expect_error(
      expect_request_header(
        request("https://test.api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform(),
        "image/png"
      ),
      "Header values must be named"
    )
    expect_error(
      expect_request_header(
        request("https://test.api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform(),
        accept = "image/png",
        "another"
      ),
      "Header values must be named"
    )
    expect_error(
      expect_request_header(
        request("https://test.api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform(),
        accept = c("image", "/png")
      ),
      "Expected headers must be strings (length 1)",
      fixed = TRUE
    )
    expect_error(
      expect_request_header(
        request("https://test.api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform(),
        accept = c(12, 34)
      ),
      "Expected headers must be strings (length 1)",
      fixed = TRUE
    )
  })
})

without_internet({
  test_that("expect_request_header without_internet", {
    expect_GET(
      expect_request_header(
        request("http://httpbin.not/") %>%
          req_headers(Accept = "image/jpeg") %>%
          req_perform(),
        Accept = "image/jpeg"
      )
    )
    # Before testthat 3.3.0, this was true:
    # Because the header check fails before the stop_request() mock
    # is called, there is no GET to assert
    # But after 3.3.0, the GET happens. So we use expect_GET() here like above
    # but we have to skip on older versions
    skip_if_not_installed("testthat", "3.3.0")
    expect_GET(
      expect_failure(
        expect_request_header(
          request("http://httpbin.not/") %>%
            req_headers(Accept = "image/png") %>%
            req_perform(),
          Accept = "image/jpeg"
        )
      )
    )
  })
})

test_that("expect_request_header works with actual network too", {
  expect_request_header(
    request(httpbin$url("/")) %>%
      req_headers(Accept = "image/jpeg") %>%
      req_perform(),
    Accept = "image/jpeg"
  )
  expect_failure(
    expect_request_header(
      request(httpbin$url("/")) %>%
        req_headers(Accept = "image/png") %>%
        req_perform(),
      Accept = "image/jpeg"
    )
  )
})
