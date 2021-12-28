with_mock_api({
  test_that("expect_request_header with mock API", {
    expect_request_header(
      request("api/object1/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_perform(),
      accept = "image/jpeg"
    )
    expect_request_header(
      request("api/object1/") %>%
        req_headers(Accept = "image/jpeg", `X-Stuff` = "more") %>%
        req_perform(),
      accept = "image/jpeg"
    )
    expect_request_header(
      request("api/object1/") %>%
        req_headers(Accept = "image/jpeg", `X-Stuff` = "more") %>%
        req_perform(),
      Accept = "image/jpeg",
      `X-stuff` = "mo"
    )
    expect_failure(
      expect_request_header(
        request("api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform(),
        accept = "image/jpeg"
      ),
      'Header "accept" does not match "image/jpeg"'
    )
  })
  # behaviors to test:
  # * existence of header ("")
  # * absence of header? (NULL)
  # * a header value (with args for regex)
  # * multiple headers
  # * also test invalid ...

  # test_that("expect_request_header ignore.case", {
  #   expect_success(expect_request_header(
  #     request("api/object1/") %>%
  #       req_headers(Accept = "image/jpeg") %>%
  #       req_perform(),
  #     "accept: image/jpeg"
  #   ))
  #   suppressWarnings(
  #     expect_failure(expect_request_header(
  #       request("api/object1/") %>%
  #         req_headers(Accept = "image/jpeg") %>%
  #         req_perform(),
  #       "accept: image/jpeg",
  #       ignore.case = FALSE
  #     ))
  #   )
  # })
})

without_internet({
  test_that("expect_request_header without_internet", {
    expect_GET(
      expect_request_header(
        request("http://httpbin.org/") %>%
          req_headers(Accept = "image/jpeg") %>%
          req_perform(),
        Accept = "image/jpeg"
      )
    )
    # Because the header check fails before the stop_request() mock
    # is called, there is no GET to assert
    expect_no_request(
      expect_failure(
        expect_request_header(
          request("http://httpbin.org/") %>%
            req_headers(Accept = "image/png") %>%
            req_perform(),
          Accept = "image/jpeg"
        )
      )
    )
  })
})

test_that("expect_request_header works with actual network too", {
  skip_if_disconnected()
  expect_request_header(
    request("http://httpbin.org/") %>%
      req_headers(Accept = "image/jpeg") %>%
      req_perform(),
    Accept = "image/jpeg"
  )
  expect_failure(
    expect_request_header(
      request("http://httpbin.org/") %>%
        req_headers(Accept = "image/png") %>%
        req_perform(),
      Accept = "image/jpeg"
    )
  )
})
