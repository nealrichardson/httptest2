d <- tempfile()
dl_file <- tempfile()
webp_file <- tempfile()
file_path <- tempfile()
cat(letters[1:6], file = file_path)

# The webfake URL will be something like 127.0.0.1:port, and port may vary
# so the mock paths will be different every time it runs
httpbin_mock_url <- build_mock_url(request(httpbin$url("")))

test_that("We can record a series of requests (a few ways)", {
  with_mock_path(d, {
    capture_requests({
      # <<- assign these so that they're available in the next test_that too
      r1 <<- request(httpbin$url("/get")) %>% req_perform()
      r2 <<- request(httpbin$url("")) %>% req_perform()
      r3 <<- request(httpbin$url("/status/418")) %>%
        req_error(is_error = ~FALSE) %>%
        req_perform()
      r4 <<- request(httpbin$url("/put")) %>%
        req_method("PUT") %>%
        req_perform()
    })
    # Now do some with start/stop
    start_capturing()
    r5 <<- request(httpbin$url("/response-headers")) %>%
      req_url_query(`Content-Type` = "application/json") %>%
      req_perform()
    # Now, some requests that write to disk
    r6 <<- request(httpbin$url("/anything")) %>%
      req_perform(path = dl_file)
    r7 <<- request(httpbin$url("/image/webp")) %>%
      req_perform(path = webp_file)
    r8 <<- request(httpbin$url("/status/202")) %>% req_perform()
    r9 <<- request(httpbin$url("/status/200")) %>% req_perform()
    r10 <<- request(httpbin$url("/post")) %>%
      req_body_multipart(
        file = curl::form_file(file_path),
        string = "some text",
        form_data = curl::form_data("form data"),
        raw_data = charToRaw("raw data")
      ) %>%
      req_method("POST") %>%
      req_perform()
    stop_capturing()
  })

  # If these were recorded against httpbin.org, it would look like this:
  expected_files <- c(
    "httpbin.org.html", # it's HTML, and we now support that simplified
    "httpbin.org/anything.json",
    "httpbin.org/get.json",
    "httpbin.org/image/webp.R", # Not a simplifiable format, so .R
    "httpbin.org/image/webp.R-FILE", # The `write_disk` location
    "httpbin.org/post-48db67-POST.json",
    "httpbin.org/put-PUT.json", # Not a GET, but returns 200
    "httpbin.org/response-headers-ac4928.json",
    "httpbin.org/status/200.txt", # empty 200 response "text/plain", so .txt
    "httpbin.org/status/202.R", # Not 200 response, so .R
    "httpbin.org/status/418.R" # Not 200 response, so .R
  )
  # But since we don't use httpbin anymore, they're in the localhost-port dir
  expected_files <- sub("httpbin.org", httpbin_mock_url, expected_files)
  expect_identical(sort(dir(d, recursive = TRUE)), expected_files)

  # Test the contents of the .R files
  teapot <- source(file.path(d, httpbin_mock_url, "status", "418.R"))$value
  expect_s3_class(teapot, "httr2_response")
  expect_identical(resp_status(teapot), 418L)
  expect_false("request" %in% names(teapot))
  # Make sure that our .html file has HTML
  expect_true(any(grepl(
    "</body>",
    suppressWarnings(readLines(file.path(d, paste0(httpbin_mock_url, ".html"))))
  )))
})

test_that("We can then load the mocks it stores", {
  # Look for mocks in our temp dir
  with_mock_path(d, {
    # Because the place we wrote out the file in our real request might not
    # naturally be in our mock directory, assume that that file doesn't exist
    # when we load our mocks.
    content_r6 <<- resp_body_json(r6)
    file.remove(dl_file)
    content_r7 <<- resp_body_raw(r7)
    file.remove(webp_file)

    mock_dl_file <- tempfile()
    mock_webp_file <- tempfile()
    with_mock_api({
      m1 <- request(httpbin$url("/get")) %>% req_perform()
      m2 <- request(httpbin$url("")) %>% req_perform()
      m3 <- request(httpbin$url("/status/418")) %>%
        req_error(is_error = ~FALSE) %>%
        req_perform()
      m4 <- request(httpbin$url("/put")) %>%
        req_method("PUT") %>%
        req_perform()
      m5 <- request(httpbin$url("/response-headers")) %>%
        req_url_query(`Content-Type` = "application/json") %>%
        req_perform()
      m6 <- request(httpbin$url("/anything")) %>%
        req_perform(path = mock_dl_file)
      m7 <- request(httpbin$url("/image/webp")) %>%
        req_perform(path = mock_webp_file)
      m8 <- request(httpbin$url("/status/202")) %>% req_perform()
      m9 <- request(httpbin$url("/status/200")) %>% req_perform()
      m10 <- request(httpbin$url("/post")) %>%
        req_body_multipart(
          file = curl::form_file(file_path),
          string = "some text",
          form_data = curl::form_data("form data"),
          raw_data = charToRaw("raw data")
        ) %>%
        req_method("POST") %>%
        req_perform()
    })
  })
  expect_identical(resp_body_json(m1), resp_body_json(r1))
  # Compare the HTML as text because the parsed HTML (XML) object has a
  # C pointer that is different between the two objects.
  expect_identical(
    enc2native(resp_body_string(m2)),
    enc2native(resp_body_string(r2))
  )

  expect_true(grepl("</body>", resp_body_string(m2)))
  expect_identical(resp_body_string(m3), resp_body_string(r3))
  expect_identical(resp_body_json(m4), resp_body_json(r4))
  expect_identical(resp_body_json(m5), resp_body_json(r5))
  expect_identical(resp_body_json(m6), content_r6)
  expect_identical(resp_body_raw(m7), content_r7)
  expect_equal(resp_status(m8), 202)
  expect_equal(resp_status(m9), 200)
  expect_equal(resp_content_type(m9), "text/plain")
  expect_false(resp_has_body(m9))
  expect_identical(resp_body_json(m10), resp_body_json(r10))
})

test_that("write_disk mocks can be reloaded even if the mock directory moves", {
  # This is an edge case caught because `crunch` package puts fixtures in
  # `inst/`, so you record to one place but when you read them from the
  # installed package, it's a different directory.
  d2 <- tempfile()
  dir.create(file.path(d2, httpbin_mock_url, "image"), recursive = TRUE)
  for (f in c("webp.R", "webp.R-FILE")) {
    file.rename(
      file.path(d, httpbin_mock_url, "image", f),
      file.path(d2, httpbin_mock_url, "image", f)
    )
  }
  with_mock_path(d2, {
    with_mock_api({
      m7b <- request(httpbin$url("/image/webp")) %>%
        req_perform(tempfile())
    })
  })
  expect_identical(resp_body_raw(m7b), content_r7)
})

with_mock_api({
  d2 <- tempfile()
  test_that("Recording requests even with the mock API", {
    capture_while_mocking(path = d2, {
      request("http://example.com/get/") %>% req_perform()
      request("https://test.api/object1/") %>% req_perform()
      request("http://httpbin.not/status/204/") %>% req_perform()
    })
    expect_setequal(
      dir(d2, recursive = TRUE),
      c(
        "example.com/get.json",
        "test.api/object1.json",
        "httpbin.not/status/204.204"
      )
    )
    expect_identical(
      readLines(file.path(d2, "example.com/get.json")),
      readLines("example.com/get.json")
    )
  })

  test_that("Loading 204 response status recorded with simplify=TRUE", {
    original <- request("http://httpbin.not/status/204/") %>% req_perform()
    expect_length(original$body, 0)
    expect_length(
      readLines(file.path(d2, "httpbin.not/status/204.204")),
      0
    )
    with_mock_path(
      d2,
      {
        mocked <- request("http://httpbin.not/status/204/") %>% req_perform()
        expect_length(mocked$body, 0)
      },
      replace = TRUE
    )
  })

  d3 <- tempfile()
  test_that("Using simplify=FALSE (and setting .mockPaths)", {
    with_mock_path(d3, {
      capture_while_mocking(simplify = FALSE, {
        request("http://example.com/get/") %>% req_perform()
        request("https://test.api/object1/") %>% req_perform()
        request("http://httpbin.not/status/204/") %>% req_perform()
      })
    })
    expect_setequal(
      dir(d3, recursive = TRUE),
      c("example.com/get.R", "test.api/object1.R", "httpbin.not/status/204.R")
    )
    response <- source(file.path(d3, "example.com/get.R"))$value
    expect_s3_class(response, "httr2_response")
    expect_identical(
      resp_body_json(response),
      resp_body_json(request("http://example.com/get/") %>% req_perform())
    )
  })

  test_that("Recorded JSON is prettified", {
    expect_length(
      readLines(file.path(d2, "example.com/get.json")),
      3L
    )
    skip("TODO: prettify when simplify=FALSE")
    response <- readLines(file.path(d3, "api/object1.R"))
  })

  test_that("Using options(httptest2.verbose = TRUE) works", {
    d4 <- tempfile()
    old <- options(httptest2.verbose = TRUE)
    on.exit(options(old))
    with_mock_path(d4, {
      capture_while_mocking(
        expect_message(
          expect_message(
            request("http://example.com/get/") %>% req_perform(),
            "Writing .*example.com.get.json"
          ),
          "Reading .*example.com.get.json"
        )
      )
    })
    expect_setequal(
      dir(d4, recursive = TRUE),
      c("example.com/get.json")
    )
    expect_identical(
      readLines(file.path(d4, "example.com/get.json")),
      readLines("example.com/get.json")
    )
  })
})

test_that("If the httr2 request function exits with an error, capture_requests warns", {
  capture_requests({
    with_mocked_responses(
      function(req) stop("Error!"),
      expect_warning(
        expect_error(
          request("http://httpbin.not/get") %>% req_perform()
        ),
        "Request errored; no captured response file saved"
      )
    )
  })
})
