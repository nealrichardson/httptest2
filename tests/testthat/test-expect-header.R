with_mock_api({
  test_that("expect_header with mock API", {
    expect_success(expect_header(
      request("api/object1/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_perform(),
      "Accept: image/jpeg"
    ))
    suppressWarnings(
      expect_failure(expect_header(
        request("api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform(),
        "Accept: image/jpeg"
      ))
    )
    expect_success(expect_header(
      request("api/object1/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_method("POST") %>%
        req_perform(),
      "Accept: image/jpeg"
    ))
    skip_if(third_edition)
    expect_failure(expect_header(
      expect_POST(
        request("api/object1/") %>%
          req_headers(Accept = "image/png") %>%
          req_method("POST") %>%
          req_perform(),
        silent = TRUE
      ),
      "Accept: image/jpeg"
    ))
  })
  test_that("expect_header ignore.case", {
    expect_success(expect_header(
      request("api/object1/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_perform(),
      "accept: image/jpeg"
    ))
    suppressWarnings(
      expect_failure(expect_header(
        request("api/object1/") %>%
          req_headers(Accept = "image/jpeg") %>%
          req_perform(),
        "accept: image/jpeg",
        ignore.case = FALSE
      ))
    )
  })
})

without_internet({
  test_that("expect_header without_internet", {
    expect_GET(expect_success(expect_header(
      request("http://httpbin.org/") %>%
        req_headers(Accept = "image/jpeg") %>%
        req_perform(),
      "Accept: image/jpeg"
    )))
    expect_GET(expect_warning(
      expect_failure(expect_header(
        request("http://httpbin.org/") %>%
          req_headers(Accept = "image/png") %>%
          req_perform(),
        "Accept: image/jpeg"
      )),
      "Accept: image/png"
    ))
  })
})

test_that("expect_header works with actual network too", {
  skip_if_disconnected()
  expect_success(expect_header(
    request("http://httpbin.org/") %>%
      req_headers(Accept = "image/jpeg") %>%
      req_perform(),
    "Accept: image/jpeg"
  ))
  expect_failure(expect_warning(
    expect_header(
      request("http://httpbin.org/") %>%
        req_headers(Accept = "image/png") %>%
        req_perform(),
      "Accept: image/jpeg"
    ),
    "Accept: image/png"
  ))
})
