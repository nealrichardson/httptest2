d <- tempfile()

with_mock_api({
  # Auth headers aren't recorded
  capture_while_mocking(simplify = FALSE, path = d, {
    a <- request("https://test.api/") %>%
      req_headers(`Authorization` = "Bearer token") %>%
      req_perform()
  })
  test_that("The mock file does not have the request headers", {
    # In httr2, response objects do not include the request,
    # so by construction there won't be request headers
    expect_false(any(grepl(
      "Bearer token",
      readLines(file.path(d, "test.api.R"))
    )))
  })
  test_that("And the redacted .R mock can be loaded", {
    with_mock_path(d, {
      b <- request("https://test.api/") %>%
        req_headers(`Authorization` = "Bearer token") %>%
        req_perform()
    })
    expect_equal(resp_body_json(b), resp_body_json(a))
  })

  # redact_cookies from response
  c2_req <- request("http://httpbin.not/cookies/set") %>%
    req_url_query(token = 12345)
  test_that("redact_cookies: the response has the set-cookie in the response", {
    capture_while_mocking(simplify = FALSE, path = d, {
      # Slight hack: httpbin does a 302 redirect, and the Set-Cookie appears
      # in the 302, but the final 200 response doesn't have it, so I've
      # modified the mock to have the Set-Cookie header, just so we can test
      # redacting it.
      # This could also be done by setting `req_options(followlocation = FALSE)`
      c2 <- req_perform(c2_req)
      expect_equal(
        resp_header(c2, "set-cookie"),
        "token=12345; Domain=example.com; Max-Age=31536000; Path=/"
      )
    })
  })
  test_that("redact_cookies removes set-cookies from response in the mock file", {
    expect_length(
      grep(
        "REDACTED",
        readLines(file.path(d, "httpbin.not", "cookies", "set-5b2631.R"))
      ),
      1
    )
  })
  test_that("And when loading that .R mock, the redacted value doesn't appear", {
    with_mock_path(d, {
      expect_identical(
        req_perform(c2_req) %>% resp_header("set-cookie"),
        "REDACTED"
      )
    })
  })

  # HTTP auth credentials aren't recorded
  auth_req <- request("http://httpbin.not/basic-auth/user/passwd") %>%
    # Slight hack: the mock was recorded with user:passwd to get a 200 response
    # but then the grepl would fail because the password is in the URL
    # (a feature of httpbin)
    req_auth_basic("user", "SeCrEtPaSsWoRd!") %>%
    req_error(is_error = ~FALSE)
  capture_while_mocking(simplify = FALSE, path = d, {
    pwauth <- req_perform(auth_req)
  })
  test_that("there is no password in the mock", {
    expect_false(any(grepl(
      "SeCrEtPaSsWoRd!",
      readLines(file.path(d, "httpbin.not", "basic-auth", "user", "passwd.R"))
    )))
  })
  test_that("And the redacted .R mock can be loaded", {
    with_mock_path(d, {
      pwauthb <- req_perform(auth_req)
    })
    expect_equal(resp_body_json(pwauthb), resp_body_json(pwauth))
  })

  # Custom redacting function
  my_redactor <- function(response) {
    # Proof that you can alter other parts of the response/mock when recording
    # Slight finesse: because requests get preprocessed with the redactor too,
    # it's tricky when we're mocking and recording, so in this test we're not
    # to change the URL when determining the mock file to load
    last_call <- unlist(tail(sys.calls(), 1))
    if (!grepl("get_current_redactor()(req)", last_call, fixed = TRUE)) {
      response$url <- "http://example.com/fakeurl"
    }
    # Proof that you can alter the response body
    cleaner <- function(x) gsub("loaded", "changed", x)
    response <- within_body_text(response, cleaner)
    return(response)
  }
  with_redactor(
    my_redactor,
    capture_while_mocking(simplify = FALSE, path = d, {
      r <- request("http://example.com/get") %>% req_perform()
    })
  )
  test_that("The real request is not affected by the redactor", {
    expect_identical(r$url, "http://example.com/get")
    expect_identical(resp_body_json(r), list(loaded = TRUE))
  })
  test_that("But the mock file gets written to the modified path with altered content", {
    # Use replace=TRUE to make sure that "." isn't in the search path.
    # We're checking that the original request doesn't have a mock,
    # but of course we made it from a mock in the working directory
    with_mock_path(d, replace = TRUE, {
      expect_GET(
        request("http://example.com/get") %>% req_perform(),
        "http://example.com/get"
      )
      expect_error(
        alt <- request("http://example.com/fakeurl") %>% req_perform(),
        NA
      )
      expect_identical(resp_body_json(alt), list(changed = TRUE))
    })
  })

  # New in httptest2: redactor is used as request preprocessor/URL shortener
  test_that("Redactors are applied when making requests to alter the mock file path we're reading", {
    with_redactor(
      function(resp) gsub_response(resp, "long/url.*$", "get"),
      r <- request("http://example.com/long/url/with/lots/of/segments") %>%
        req_perform()
    )
    # The URL of the mock response in this case is actually the full request URL
    # because it is a JSON mock so the httr2_response object is generated
    # based on the request
    expect_identical(r$url, "http://example.com/long/url/with/lots/of/segments")
    # But this is the response body of the mock corresponding to example.com/get
    expect_identical(resp_body_json(r), list(loaded = TRUE))
  })

  a <- request("https://test.api/") %>%
    req_headers(`Authorization` = "Bearer token") %>%
    req_perform()
  test_that("gsub_response", {
    asub <- gsub_response(a, "api", "OTHER")
    expect_identical(asub$url, "https://test.OTHER/")
    expect_identical(
      resp_body_json(asub),
      list(value = "https://test.OTHER/object1/")
    )
  })
  test_that("as.redactor", {
    a2 <- prepare_redactor(~ gsub_response(., "api", "OTHER"))(a)
    expect_identical(
      resp_body_json(a2),
      list(value = "https://test.OTHER/object1/")
    )
  })

  loc <- request("http://httpbin.not/response-headers") %>%
    req_url_query(Location = "http://httpbin.not/status/201") %>%
    req_perform()
  loc_sub <- gsub_response(
    loc,
    "http://httpbin.not/status/201",
    "http://httpbin.not/status/404"
  )
  test_that("gsub_response touches Location header", {
    expect_identical(
      resp_header(loc_sub, "location"),
      "http://httpbin.not/status/404"
    )
    expect_identical(
      resp_body_json(loc_sub)$Location,
      "http://httpbin.not/status/404"
    )
  })
  test_that("gsub_response handles URL encoding", {
    skip("TODO: handle URL escaping")
    expect_identical(
      loc_sub$url,
      "http://httpbin.not/response-headers?Location=http%3A%2F%2Fhttpbin.not%2Fstatus%2F404"
    )
  })
})

test_that("gsub_response handles empty response bodies (#20)", {
  with_redactor(
    function(resp) gsub_response(resp, "status", "code"),
    capture_requests({
      # Prior to the fix, the redactor errored here on retrieving an empty body
      r <- request(httpbin$url("/status/204")) %>% req_perform()
    })
  )
  # Nothing here
  expect_length(r$body, 0)
})

test_that("chain_redactors", {
  f1 <- function(x) x * 4
  f2 <- ~ sum(c(., 3))
  f12 <- chain_redactors(list(f1, f2))
  f21 <- chain_redactors(list(f2, f1))
  expect_equal(f12(5), 23)
  expect_equal(f21(5), 32)
})

reset_redactors()
