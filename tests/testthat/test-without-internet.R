without_internet({
  test_that("without_internet throws error on all verbs", {
    expect_GET(
      request(httpbin$url("/get")) %>% req_perform(),
      httpbin$url("/get")
    )
    expect_PUT(
      request(httpbin$url("/get")) %>% req_method("PUT") %>% req_perform(),
      httpbin$url("/get")
    )
    expect_POST(
      request(httpbin$url("/get")) %>% req_method("POST") %>% req_perform(),
      httpbin$url("/get")
    )
    expect_PATCH(
      request(httpbin$url("/get")) %>% req_method("PATCH") %>% req_perform(),
      httpbin$url("/get")
    )
    expect_DELETE(
      request(httpbin$url("/get")) %>% req_method("DELETE") %>% req_perform(),
      httpbin$url("/get")
    )
  })

  test_that("without_internet includes request body in message", {
    this_req <- request(httpbin$url("/get")) %>%
      req_body_json(list(test = TRUE))
    expect_PUT(
      this_req %>%
        req_method("PUT") %>%
        req_perform(),
      httpbin$url("/get"),
      '{"test":true}'
    )
    expect_POST(
      this_req %>%
        req_method("POST") %>%
        req_perform(),
      httpbin$url("/get"),
      '{"test":true}'
    )
    expect_PATCH(
      this_req %>%
        req_method("PATCH") %>%
        req_perform(),
      httpbin$url("/get"),
      '{"test":true}'
    )
  })

  test_that("max.print option", {
    options(httptest2.max.print = 3)
    on.exit(options(httptest2.max.print = NULL))
    expect_PUT(
      request(httpbin$url("/get")) %>%
        req_body_json(list(test = TRUE)) %>%
        req_method("PUT") %>%
        req_perform(),
      httpbin$url("/get"),
      '\\{"t$',
      fixed = FALSE # To use the regular expression $ to match the end
    )
  })

  test_that("without_internet respects query params", {
    expect_GET(
      request(httpbin$url("/get")) %>%
        req_url_query(test = "a phrase", two = 3) %>%
        req_perform(),
      paste0(httpbin$url("/get"), "?test=a%20phrase&two=3")
    )
  })
})

test_that("block_requests()", {
  block_requests()
  on.exit(stop_mocking())
  expect_GET(
    request(httpbin$url("/get")) %>% req_perform(),
    httpbin$url("/get")
  )
})
