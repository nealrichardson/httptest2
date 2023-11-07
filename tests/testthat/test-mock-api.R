with_mock_api({
  test_that("Can load an object and file extension is added", {
    a <- request("https://test.api/") %>% req_perform()
    expect_identical(resp_body_json(a), list(value = "https://test.api/object1/"))
    b <- request(resp_body_json(a)$value) %>% req_perform()
    expect_identical(resp_body_json(b), list(object = TRUE))
  })
  test_that("GET with query", {
    obj <- request("https://test.api/object1/") %>%
      req_url_query(a = 1) %>%
      req_perform()
    expect_equal(
      resp_body_json(obj),
      list(query = list(a = 1), mocked = "yes"),
      list_as_map = TRUE
    )
  })
  test_that("GET with special characters", {
    a <- request("https://test.api/x(y='1',z='2')") %>% req_perform()
    expect_identical(resp_body_json(a), list(object = TRUE))
  })
  test_that("GET files that don't exist errors", {
    expect_GET(
      request("https://test.api/NOTAFILE/") %>% req_perform(), 
      "https://test.api/NOTAFILE/"
    )
    expect_GET(
      request("https://test.api/NOTAFILE/") %>%
        req_url_query(a = 1) %>%
        req_perform(),
      "https://test.api/NOTAFILE/?a=1"
    )
  })
  test_that("POST method reads from correct file", {
    b <- request("https://test.api/object1") %>%
      req_method("POST") %>%
      req_perform()
    expect_identical(resp_body_json(b), list(method = "POST"))
    b2 <- request("https://test.api/object1") %>%
      req_method("POST") %>%
      req_headers(
        Accept = "application/json",
        "Content-Type" = "application/json"
      ) %>%
      req_perform()
    expect_identical(resp_body_json(b2), list(method = "POST"))
  })
  test_that("Request body is appended to mock file path", {
    p <- request("https://test.api/object1") %>%
      req_body_raw('{"a":1}', type = "application/json") %>%
      req_headers(
        Accept = "application/json"
      ) %>%
      req_perform()
    expect_identical(resp_body_json(p), list(content = TRUE))
    expect_error(
      request("https://test.api/object1") %>%
        req_body_raw('{"b":2}', type = "application/json") %>%
        req_headers(
          Accept = "application/json"
        ) %>%
        req_perform(),
      "api/object1-3e8d9a-POST."
    )
  })
  test_that("max.print option", {
    options(httptest2.max.print = 3)
    on.exit(options(httptest2.max.print = NULL))
    expect_PUT(
      request("http://httpbin.not/get") %>%
        req_method("PUT") %>%
        req_body_json(list(test = TRUE)) %>%
        req_perform(),
      "http://httpbin.not/get",
      '{"t '
    )
  })
  test_that("Request body and query", {
    expect_PATCH(
      request("https://test.api/object2?d=1") %>%
        req_method("PATCH") %>%
        req_body_json(list(arg = 45)) %>%
        req_perform(),
      'https://test.api/object2?d=1 {"arg":45}'
    )
  })
  test_that("Other verbs error too", {
    expect_PUT(
      request("https://test.api/") %>%
        req_method("PUT") %>%
        req_perform(),
      "https://test.api/"
    )
    expect_PATCH(
      request("https://test.api/") %>%
        req_method("PATCH") %>%
        req_perform(),
      "https://test.api/"
    )
    expect_POST(
      request("https://test.api/") %>%
        req_method("POST") %>%
        req_perform(),
      "https://test.api/"
    )
    expect_POST(
      request("https://test.api/") %>%
        req_body_raw('{"arg":true}') %>%
        req_perform(),
      "https://test.api/",
      '{"arg":true}'
    )
    expect_DELETE(
      request("https://test.api/") %>%
        req_method("DELETE") %>%
        req_perform(),
      "https://test.api/"
    )
  })

  test_that("mock API with http:// URL, not file path", {
    expect_GET(
      request("http://httpbin.not/get") %>% req_perform(),
      "http://httpbin.not/get"
    )
    expect_GET(
      request("https://httpbin.not/get") %>% req_perform(),
      "https://httpbin.not/get"
    )
    expect_identical(
      resp_body_json(request("http://example.com/get") %>% req_perform()),
      list(loaded = TRUE)
    )
  })

  test_that("Mock GET with non-JSON", {
    dick <- request("http://example.com/html") %>% req_perform()
    expect_true(grepl("Melville", resp_body_string(dick)))
  })

  test_that("POST with all body types", {
    r <- request("http://httpbin.not/post") %>% req_method("POST")
    expect_POST(
      r %>%
        req_body_raw("A simple text string") %>%
        req_perform(),
      "http://httpbin.not/post",
      "A simple text string "
    )
    expect_POST(
      r %>%
        req_body_form(x = "A simple text string") %>%
        req_perform(),
      "http://httpbin.not/post",
      "x=A%20simple%20text%20string "
    )
    expect_POST(
      r %>%
        req_body_json(data = list(x = "A simple text string")) %>%
        req_perform(),
      "http://httpbin.not/post",
      '{"x":"A simple text string"} '
    )
    file_to_upload <- tempfile()
    cat("testing", file = file_to_upload)
    expect_POST(
      r %>%
        req_body_file(file_to_upload) %>%
        req_perform(),
      "http://httpbin.not/post",
      "File: ae2b1fca515949e5d54fb22b8ed95575"
    )

    expect_POST(
      r %>%
        req_body_multipart(
          a = curl::form_file(file_to_upload),
          b = curl::form_data("strings")
        ) %>%
        req_perform(),
      "http://httpbin.not/post",
      "Multipart form:
  a = File: ae2b1fca515949e5d54fb22b8ed95575
  b = strings"
    )
  })

  test_that("Regular expressions in expect_VERB", {
    expect_GET(request("http://example.com/1234/abcd/") %>% req_perform(),
      "http://example.com/[0-9]{4}/[a-z]{4}/",
      fixed = FALSE
    )
    expect_GET(request("http://example.com/1234/abcd/") %>% req_perform(),
      "http://EXAMPLE.com/[0-9]{4}/[a-z]{4}/",
      fixed = FALSE,
      ignore.case = TRUE
    )
    expect_POST(request("http://example.com/1234/abcd/") %>% req_method("POST") %>% req_perform(),
      "http://example.com/[0-9]{4}/[a-z]{4}/",
      fixed = FALSE
    )
    expect_DELETE(request("http://example.com/1234/abcd/") %>% req_method("DELETE") %>% req_perform(),
      "http://example.com/[0-9]{4}/[a-z]{4}/",
      fixed = FALSE
    )
  })
})

test_that("build_mock_url file path construction with character URL", {
  # GET (default) method
  file <- build_mock_url(request("http://www.test.com/api/call"))
  expect <- "www.test.com/api/call"
  expect_identical(file, expect, label = "Get method without query string")

  # GET method with query in URL
  file <- build_mock_url(request("http://www.test.com/api/call?q=1"))
  expect <- "www.test.com/api/call-a3679d"
  expect_identical(file, expect, label = "Get method with query string")

  # POST method
  file <- build_mock_url(request("http://www.test.com/api/call") %>% req_method("POST"))
  expect <- "www.test.com/api/call-POST"
  expect_identical(file, expect, "POST method without query string")

  # POST method with query in URL
  file <- build_mock_url(request("http://www.test.com/api/call?q=1") %>% req_method("POST"))
  expect <- "www.test.com/api/call-a3679d-POST"
  expect_identical(file, expect, "POST method with query string")
})

test_that("build_mock_url returns file names that are valid on all R platforms", {
  u <- "https://language.googleapis.com/v1/documents:annotateText/"
  expect_identical(
    build_mock_url(request(u)),
    "language.googleapis.com/v1/documents-annotateText"
  )
})

test_that("load_response invalid extension handling", {
  expect_error(
    load_response("foo.qwert"),
    "Unsupported mock file extension: qwert"
  )
})

test_that("mock_request code paths are covered (outside of trace)", {
  expect_s3_class(
    mock_request(list(method = "GET", url = "https://test.api/")),
    "httr2_response"
  )
  expect_s3_class(
    mock_request(list(method = "GET", url = "http://example.com/html")),
    "httr2_response"
  )
  expect_error(mock_request(list(method = "PUT", url = "api/")))
})
