test_that("Outside of without_internet, requests work", {
  skip_if_disconnected()
  expect_error(request("http://httpbin.org/get") %>% req_perform(), NA)
})
test_that("without_internet throws errors on GET", {
  without_internet({
    expect_error(
      request("http://httpbin.org/get") %>% req_perform(),
      "GET http://httpbin.org/get"
    )
    expect_GET(
      request("http://httpbin.org/get") %>% req_perform(),
      "http://httpbin.org/get"
    )
  })
})

without_internet({
  test_that("without_internet throws error on other verbs", {
    expect_PUT(
      request("http://httpbin.org/get") %>% req_method("PUT") %>% req_perform(),
      "http://httpbin.org/get"
    )
    expect_POST(
      request("http://httpbin.org/get") %>% req_method("POST") %>% req_perform(),
      "http://httpbin.org/get"
    )
    expect_PATCH(
      request("http://httpbin.org/get") %>% req_method("PATCH") %>% req_perform(),
      "http://httpbin.org/get"
    )
    expect_DELETE(
      request("http://httpbin.org/get") %>% req_method("DELETE") %>% req_perform(),
      "http://httpbin.org/get"
    )
  })

  test_that("without_internet includes request body in message", {
    this_req <- request("http://httpbin.org/get") %>%
      req_body_json(list(test = TRUE))
    expect_PUT(
      this_req %>%
        req_method("PUT") %>%
        req_perform(),
      "http://httpbin.org/get",
      '{"test":true}'
    )
    expect_POST(
      this_req %>%
        req_method("POST") %>%
        req_perform(),
      "http://httpbin.org/get",
      '{"test":true}'
    )
    expect_PATCH(
      this_req %>%
        req_method("PATCH") %>%
        req_perform(),
      "http://httpbin.org/get",
      '{"test":true}'
    )
  })

  test_that("max.print option", {
    options(httptest2.max.print = 3)
    on.exit(options(httptest2.max.print = NULL))
    expect_PUT(
      request("http://httpbin.org/get") %>%
        req_body_json(list(test = TRUE)) %>%
        req_method("PUT") %>%
        req_perform(),
      "http://httpbin.org/get",
      '\\{"t$',
      fixed = FALSE # To use the regular expression $ to match the end
    )
  })

  test_that("without_internet respects query params", {
    expect_GET(
      request("http://httpbin.org/get") %>%
        req_url_query(test = "a phrase", two = 3) %>%
        req_perform(),
      "http://httpbin.org/get?test=a%20phrase&two=3"
    )
  })
})

test_that("block_requests()", {
  block_requests()
  on.exit(stop_mocking())
  expect_GET(
    request("http://httpbin.org/get") %>% req_perform(),
    "http://httpbin.org/get"
  )
})
