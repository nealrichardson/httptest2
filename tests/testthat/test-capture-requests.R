d <- tempfile()
dl_file <- tempfile()
webp_file <- tempfile()

test_that("We can record a series of requests (a few ways)", {
  skip_if_disconnected()
  capture_requests(path = d, {
    # <<- assign these so that they're available in the next test_that too
    r1 <<- GET("http://httpbin.org/get")
    r2 <<- GET("http://httpbin.org")
    r3 <<- GET("http://httpbin.org/status/418")
    r4 <<- PUT("http://httpbin.org/put")
  })
  start_capturing(path = d)
  r5 <<- request("http://httpbin.org/response-headers") %>%
    req_url_query(`Content-Type` = "application/json") %>%
    req_perform()
  # HTTR2: implement write_disk (see also commented code below)
  # r6 <<- GET("http://httpbin.org/anything", config = write_disk(dl_file))
  # r7 <<- GET("http://httpbin.org/image/webp", config = write_disk(webp_file))
  r8 <<- GET("http://httpbin.org/status/202")
  stop_capturing()
  .mockPaths(NULL) # because start_capturing with path modifies global state
  expect_identical(
    sort(dir(d, recursive = TRUE)),
    c(
      "httpbin.org.html", # it's HTML, and we now support that simplified
      # "httpbin.org/anything.json",
      "httpbin.org/get.json",
      # "httpbin.org/image/webp.R", # Not a simplifiable format, so .R
      # "httpbin.org/image/webp.R-FILE", # The `write_disk` location
      "httpbin.org/put-PUT.json", # Not a GET, but returns 200
      "httpbin.org/response-headers-ac4928.json",
      "httpbin.org/status/202.R", # Not 200 response, so .R
      "httpbin.org/status/418.R" # Not 200 response, so .R
    )
  )
  # Test the contents of the .R files
  teapot <- source(file.path(d, "httpbin.org/status/418.R"))$value
  expect_s3_class(teapot, "httr2_response")
  expect_identical(resp_status(teapot), 418L)
  # Make sure that our .html file has HTML
  expect_true(any(grepl(
    "</body>",
    suppressWarnings(readLines(file.path(d, "httpbin.org.html")))
  )))
})

test_that("We can then load the mocks it stores", {
  skip_if_disconnected()
  # Look for mocks in our temp dir
  with_mock_path(d, {
    # Because the place we wrote out the file in our real request might not
    # naturally be in our mock directory, assume that that file doesn't exist
    # when we load our mocks.
    # HTTR2: implement write_disk
    # content_r6 <<- content(r6)
    # file.remove(dl_file)
    # content_r7 <<- content(r7)
    # file.remove(webp_file)

    mock_dl_file <- tempfile()
    mock_webp_file <- tempfile()
    with_mock_api({
      m1 <- GET("http://httpbin.org/get")
      m2 <- GET("http://httpbin.org")
      m3 <- GET("http://httpbin.org/status/418")
      m4 <- PUT("http://httpbin.org/put")
      m5 <- request("http://httpbin.org/response-headers") %>%
        req_url_query(`Content-Type` = "application/json") %>%
        req_perform()
      # m6 <- GET("http://httpbin.org/anything", config = write_disk(mock_dl_file))
      # m7 <- GET("http://httpbin.org/image/webp", config = write_disk(mock_webp_file))
      m8 <- GET("http://httpbin.org/status/202")
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
  # expect_identical(resp_body_string(m6), content_r6)
  # expect_identical(resp_body_string(m7), content_r7)
  expect_equal(resp_status(m8), 202)
})

test_that("write_disk mocks can be reloaded even if the mock directory moves", {
  skip("HTTR2: implement write_disk")
  skip_if_disconnected()
  # This is an edge case caught because `crunch` package puts fixtures in
  # `inst/`, so you record to one place but when you read them from the
  # installed package, it's a different directory.
  d2 <- tempfile()
  dir.create(file.path(d2, "httpbin.org", "image"), recursive = TRUE)
  for (f in c("httpbin.org/image/webp.R", "httpbin.org/image/webp.R-FILE")) {
    file.rename(file.path(d, f), file.path(d2, f))
  }
  with_mock_path(d2, {
    with_mock_api({
      m7b <- GET("http://httpbin.org/image/webp",
        config = write_disk(tempfile())
      )
    })
  })
  expect_identical(content(m7b), content_r7)
})

with_mock_api({
  d2 <- tempfile()
  test_that("Recording requests even with the mock API", {
    capture_while_mocking(path = d2, {
      GET("http://example.com/get/")
      GET("api/object1/")
      # HTTR2: adapt httr::response to httr2::httr2_response
      # GET("http://httpbin.org/status/204/")
    })
    expect_setequal(
      dir(d2, recursive = TRUE),
      c("example.com/get.json", "api/object1.json") # , "httpbin.org/status/204.204")
    )
    expect_identical(
      readLines(file.path(d2, "example.com/get.json")),
      readLines("example.com/get.json")
    )
  })

  test_that("Loading 204 response status recorded with simplify=TRUE", {
    skip("HTTR2: adapt httr::response to httr2::httr2_response")
    original <- GET("http://httpbin.org/status/204/")
    expect_null(content(original))
    expect_length(
      readLines(file.path(d2, "httpbin.org/status/204.204")),
      0
    )
    with_mock_path(d2,
      {
        mocked <- GET("http://httpbin.org/status/204/")
        expect_null(content(mocked))
      },
      replace = TRUE
    )
  })

  d3 <- tempfile()
  test_that("Using simplify=FALSE (and setting .mockPaths)", {
    with_mock_path(d3, {
      capture_while_mocking(simplify = FALSE, {
        GET("http://example.com/get/")
        GET("api/object1/")
        # HTTR2: adapt httr::response to httr2::httr2_response
        # GET("http://httpbin.org/status/204/")
      })
    })
    expect_setequal(
      dir(d3, recursive = TRUE),
      c("example.com/get.R", "api/object1.R") # , "httpbin.org/status/204.R")
    )
    response <- source(file.path(d3, "example.com/get.R"))$value
    expect_s3_class(response, "httr2_response")
    expect_identical(
      resp_body_json(response),
      resp_body_json(GET("http://example.com/get/"))
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

  test_that("Using options(httptest.verbose=TRUE) works", {
    d4 <- tempfile()
    old <- options(httptest.verbose = TRUE)
    on.exit(options(old))
    with_mock_path(d4, {
      capture_while_mocking(
        expect_message(
          GET("http://example.com/get/"),
          "Writing .*example.com.get.json"
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

test_that("If the httr request function exits with an error, capture_requests warns", {
  skip_on_R_older_than("3.5.0") # IDK why but it fails on travis
  capture_requests({
    expect_warning(
      expect_error(GET(stop("Error!"))),
      "Request errored; no captured response file saved"
    )
  })
})
