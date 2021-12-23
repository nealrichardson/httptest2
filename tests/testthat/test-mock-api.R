with_mock_api({
  test_that("Can load an object and file extension is added", {
    a <- GET("api/")
    expect_identical(resp_body_json(a), list(value = "api/object1/"))
    b <- GET(resp_body_json(a)$value)
    expect_identical(resp_body_json(b), list(object = TRUE))
  })
  test_that("GET with query", {
    obj <- request("api/object1/") %>%
      req_url_query(a = 1) %>%
      req_perform()
    expect_json_equivalent(
      resp_body_json(obj),
      list(query = list(a = 1), mocked = "yes")
    )
  })
  test_that("GET with special characters", {
    a <- GET("api/x(y='1',z='2')")
    expect_identical(resp_body_json(a), list(object = TRUE))
  })
  test_that("GET files that don't exist errors", {
    expect_GET(GET("api/NOTAFILE/"), "api/NOTAFILE/")
    expect_GET(
      request("api/NOTAFILE/") %>%
        req_url_query(a = 1) %>%
        req_perform(),
      "api/NOTAFILE/?a=1"
    )
  })
  test_that("POST method reads from correct file", {
    b <- POST("api/object1")
    expect_identical(resp_body_json(b), list(method = "POST"))
    b2 <- request("api/object1") %>%
      req_method("POST") %>%
      req_headers(
        Accept = "application/json",
        "Content-Type" = "application/json"
      ) %>%
      req_perform()
    expect_identical(resp_body_json(b2), list(method = "POST"))
  })
  test_that("Request body is appended to mock file path", {
    p <- request("api/object1") %>%
      req_body_raw('{"a":1}', type = "application/json") %>%
      req_headers(
        Accept = "application/json"
      ) %>%
      req_perform()
    expect_identical(resp_body_json(p), list(content = TRUE))
    expect_POST(
      request("api/object1") %>%
        req_body_raw('{"b":2}', type = "application/json") %>%
        req_headers(
          Accept = "application/json"
        ) %>%
        req_perform(),
      'api/object1 {"b":2} (api/object1-3e8d9a-POST.json)'
    )
  })
  test_that("max.print option", {
    options(httptest.max.print = 3)
    on.exit(options(httptest.max.print = NULL))
    expect_PUT(
      request("http://httpbin.org/get") %>%
        req_method("PUT") %>%
        req_body_json(list(test = TRUE)) %>%
        req_perform(),
      "http://httpbin.org/get",
      '{"t ',
      "(httpbin.org/get-"
    )
  })
  test_that("Request body and query", {
    expect_PATCH(
      request("api/object2?d=1") %>%
        req_method("PATCH") %>%
        req_body_json(list(arg = 45)) %>%
        req_perform(),
      'api/object2?d=1 {"arg":45} (api/object2-899b0e-3d8d62-PATCH.json)'
    )
  })
  test_that("Other verbs error too", {
    expect_PUT(PUT("api/"), "api/")
    expect_PATCH(PATCH("api/"), "api/")
    expect_POST(POST("api/"), "api/")
    expect_POST(
      request("api/") %>%
        req_body_raw('{"arg":true}') %>%
        req_perform(),
      "api/",
      '{"arg":true}'
    )
    expect_DELETE(DELETE("api/"), "api/")
  })

  test_that("mock API with http:// URL, not file path", {
    expect_GET(
      GET("http://httpbin.org/get"),
      "http://httpbin.org/get",
      "(httpbin.org/get.json)"
    )
    expect_GET(
      GET("https://httpbin.org/get"),
      "https://httpbin.org/get",
      "(httpbin.org/get.json)"
    )
    expect_identical(
      resp_body_json(GET("http://example.com/get")),
      list(loaded = TRUE)
    )
  })

  test_that("Mocking a GET with more function args (path, auth)", {
    skip("HTTR2: rewrite test with httr2")
    expect_identical(
      resp_body_json(GET("http://example.com",
        path = "/get",
        add_headers("Content-Type" = "application/json"),
        authenticate("d", "d")
      )),
      list(loaded = TRUE)
    )
  })

  test_that("Mock GET with non-JSON", {
    dick <- GET("http://example.com/html")
    expect_true(grepl("Melville", resp_body_string(dick)))
  })

  test_that("POST/PUT/etc. with other body types", {
    skip("HTTR2: update tests for all req_body_*()")
    b2 <- "http://httpbin.org/post"
    expect_POST(
      POST(b2, body = list(x = "A simple text string")),
      "http://httpbin.org/post",
      'list(x = "A simple text string") ',
      "(httpbin.org/post-97fc23-POST.json)"
    )
    expect_POST(
      POST(b2, body = list(x = "A simple text string"), encode = "form"),
      "http://httpbin.org/post",
      "x=A%20simple%20text%20string ",
      "(httpbin.org/post-aa2999-POST.json)"
    )
    expect_PUT(
      PUT(b2, body = list(x = "A simple text string")),
      "http://httpbin.org/post",
      'list(x = "A simple text string") ',
      "(httpbin.org/post-97fc23-PUT.json)"
    )
    expect_POST(
      POST(b2, body = list(x = "A simple text string"), encode = "json"),
      "http://httpbin.org/post",
      '{"x":"A simple text string"} ',
      "(httpbin.org/post-34199a-POST.json)"
    )
    expect_POST(
      POST(b2, body = list(y = upload_file(testthat::test_path("setup.R")))),
      "http://httpbin.org/post",
      'list(y = list(path = "',
      testthat::test_path("setup.R"),
      '", type = "text/plain")) ',
      "(httpbin.org/post-79b618-POST.json)"
    )
  })

  test_that("PUT/POST with only a upload_file in body", {
    skip("HTTR2: handle file uploads")
    b1 <- "http://httpbin.org/post"
    expect_POST(
      POST(b1, body = upload_file(testthat::test_path("setup.R"))),
      "http://httpbin.org/post",
      "(httpbin.org/post-POST.json)"
    )
    # and ensure that there are no floating connections still open
    open_conns <- showConnections()
    expect_false(any(open_conns[, "description"] == "setup.R"))
    b2 <- "http://httpbin.org/put"
    expect_PUT(
      PUT(b2, body = upload_file(testthat::test_path("setup.R"))),
      "http://httpbin.org/put",
      "(httpbin.org/put-PUT.json)"
    )
    # and ensure that there are no floating connections still open
    open_conns <- showConnections()
    expect_false(any(open_conns[, "description"] == "setup.R"))
  })

  test_that("Regular expressions in expect_VERB", {
    expect_GET(GET("http://example.com/1234/abcd/"),
      "http://example.com/[0-9]{4}/[a-z]{4}/",
      fixed = FALSE
    )
    expect_GET(GET("http://example.com/1234/abcd/"),
      "http://EXAMPLE.com/[0-9]{4}/[a-z]{4}/",
      fixed = FALSE,
      ignore.case = TRUE
    )
    expect_POST(POST("http://example.com/1234/abcd/"),
      "http://example.com/[0-9]{4}/[a-z]{4}/",
      fixed = FALSE
    )
    expect_DELETE(DELETE("http://example.com/1234/abcd/"),
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
    mock_request(list(method = "GET", url = "api/")),
    "httr2_response"
  )
  expect_s3_class(
    mock_request(list(method = "GET", url = "http://example.com/html")),
    "httr2_response"
  )
  expect_error(mock_request(list(method = "PUT", url = "api/")))
})
