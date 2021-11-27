d <- tempfile()

with_mock_api({
  # Auth headers aren't recorded
  capture_while_mocking(simplify = FALSE, path = d, {
    a <- request("api/") %>%
      req_headers(`Authorization` = "Bearer token") %>%
      req_perform()
  })
  test_that("The mock file does not have the request headers", {
    expect_false(any(grepl("Bearer token", readLines(file.path(d, "api.R")))))
  })
  test_that("And the redacted .R mock can be loaded", {
    with_mock_path(d, {
      b <- request("api/") %>%
        req_headers(`Authorization` = "Bearer token") %>%
        req_perform()
    })
    expect_equal(resp_body_json(b), resp_body_json(a))
  })

  # HTTR2: adapt httr::set_cookies()
  # Request cookies aren't recorded
  # test_that("redact_cookies: the response has the cookie set in the request", {
  #   capture_while_mocking(simplify = FALSE, path = d, {
  #     cooks <- GET("http://httpbin.org/cookies", set_cookies(token = "12345"))
  #   })
  #   expect_identical(cooks$request$options$cookie, "token=12345")
  # })
  # test_that("redact_cookies removes cookies from request in the mock file", {
  #   cooksfile <- readLines(file.path(d, "httpbin.org", "cookies.R"))
  #   expect_false(any(grepl("token=12345", cooksfile)))
  # })
  # test_that("And the redacted .R mock can be loaded", {
  #   with_mock_path(d, {
  #     cooksb <- GET("http://httpbin.org/cookies", set_cookies(token = "12345"))
  #   })
  #   expect_equal(content(cooksb), content(cooks))
  # })

  # HTTR2: adapt httr::response to httr2::httr2_response
  # # redact_cookies from response
  # c2_req <- request("http://httpbin.org/cookies/set") %>%
  #   req_url_query(token = 12345)
  # test_that("redact_cookies: the response has the set-cookie in the response", {
  #   capture_while_mocking(simplify = FALSE, path = d, {
  #     c2 <- req_perform(c2_req)
  #   })
  #   # Note the "all_headers": the request did a 302 redirect
  #   expect_identical(
  #     c2$all_headers[[1]]$headers[["set-cookie"]],
  #     "token=12345; Path=/"
  #   )
  #   expect_identical(c2$cookies$value, "12345")
  # })
  # test_that("redact_cookies removes set-cookies from response in the mock file", {
  #   # Note that "token=12345" appears in the request URL because of how
  #   # httpbin works. But normally your cookie wouldn't be in the URL.
  #   # Of course, if you wanted to sanitize URLs too, you could write your
  #   # own custom redacting function.
  #   expect_false(any(grepl(
  #     '"token=12345',
  #     readLines(file.path(d, "httpbin.org", "cookies", "set-5b2631.R"))
  #   )))
  #   expect_length(
  #     grep(
  #       "REDACTED",
  #       readLines(file.path(d, "httpbin.org", "cookies", "set-5b2631.R"))
  #     ),
  #     2
  #   )
  # })
  # test_that("And when loading that .R mock, the redacted value doesn't appear", {
  #   with_mock_path(d, {
  #     c2b <- req_perform(c2_req)
  #   })
  #   expect_identical(
  #     c2b$all_headers[[1]]$headers[["set-cookie"]],
  #     "REDACTED"
  #   )
  #   expect_identical(c2b$cookies$value, "REDACTED")
  # })

  # HTTR2: adapt httr::response to httr2::httr2_response
  # # another redact_cookies with POST example.com/login
  # login_req <- request("http://example.com/login") %>%
  #   req_body_json(list(username = "password"))
  # test_that("redact_cookies: the response has the set-cookie in the response", {
  #   capture_while_mocking(simplify = FALSE, path = d, {
  #     login <- req_perform(login_req)
  #   })
  #   # Note the "all_headers": the request did a 302 redirect
  #   expect_true(grepl(
  #     "token=12345",
  #     login$all_headers[[1]]$headers[["set-cookie"]]
  #   ))
  #   expect_true(grepl(
  #     "token=12345",
  #     login$headers[["set-cookie"]]
  #   ))
  #   expect_identical(login$cookies$value, "12345")
  # })
  # test_that("redact_cookies removes set-cookies from response in the mock file", {
  #   # Unlike other example, token=12345 isn't in the URL
  #   expect_false(any(grepl(
  #     "12345",
  #     readLines(file.path(d, "example.com", "login-712027-POST.R"))
  #   )))
  #   expect_length(
  #     grep(
  #       "REDACTED",
  #       readLines(file.path(d, "example.com", "login-712027-POST.R"))
  #     ),
  #     3
  #   )
  # })
  # test_that("And when loading that .R mock, the redacted value doesn't appear", {
  #   with_mock_path(d, {
  #     loginb <- req_perform(login_req)
  #   })
  #   expect_identical(
  #     loginb$all_headers[[1]]$headers[["set-cookie"]],
  #     "REDACTED"
  #   )
  #   expect_identical(loginb$headers[["set-cookie"]], "REDACTED")
  #   expect_identical(loginb$cookies$value, "REDACTED")
  # })

  # HTTR2: adapt httr::response to httr2::httr2_response
  # # HTTP auth credentials aren't recorded
  # auth_req <- request("http://httpbin.org/basic-auth/user/passwd") %>%
  #   req_auth_basic("user", "passwd")
  # test_that("redact_http_auth: the request has the user:pw set", {
  #   capture_while_mocking(simplify = FALSE, path = d, {
  #     pwauth <- req_perform(auth_req)
  #   })
  #   expect_identical(pwauth$request$options$userpwd, "user:passwd")
  # })
  # test_that("redact_http_auth removes user:pw from request in the mock file", {
  #   expect_false(any(grepl(
  #     "user:passwd",
  #     readLines(file.path(d, "httpbin.org", "basic-auth", "user", "passwd.R"))
  #   )))
  # })
  # test_that("And the redacted .R mock can be loaded", {
  #   with_mock_path(d, {
  #     pwauthb <- req_perform(auth_req)
  #   })
  #   expect_equal(resp_body_json(pwauthb), resp_body_json(pwauth))
  # })

  # HTTR2: update for httr2 oauth
  # # OAuth credentials aren't recorded
  # # Example token copied from a test in httr
  # token <- Token2.0$new(
  #   app = oauth_app("x", "y", "z"),
  #   endpoint = oauth_endpoints("google"),
  #   credentials = list(access_token = "ofNoArms")
  # )
  # token$params$as_header <- TRUE
  # capture_while_mocking(simplify = FALSE, path = d, {
  #   oauth <- GET("api/object1/", config(token = token))
  # })
  # test_that("The response has the 'auth_token' object'", {
  #   expect_s3_class(oauth$request$auth_token, "Token2.0")
  # })

  # test_that("But the mock doesn't have the auth_token", {
  #   oauthfile <- readLines(file.path(d, "api", "object1.R"))
  #   expect_false(any(grepl("auth_token", oauthfile)))
  # })
  # test_that("And the .R mock can be loaded", {
  #   with_mock_path(d, {
  #     oauthb <- GET("api/object1/", config(token = token))
  #   })
  #   expect_equal(resp_body_json(oauthb), resp_body_json(oauth))
  # })

  # HTTR2: redacting can't alter the mock file path because request isn't contained in the response (file path is constructed outside)
  # # Custom redacting function
  # my_redactor <- function(response) {
  #   # Proof that you can alter other parts of the response/mock
  #   response$url <- response$request$url <- "http://example.com/fakeurl"
  #   # Proof that you can alter the response body
  #   cleaner <- function(x) gsub("loaded", "changed", x)
  #   response <- within_body_text(response, cleaner)
  #   return(response)
  # }
  # with_redactor(
  #   my_redactor,
  #   capture_while_mocking(simplify = FALSE, path = d, {
  #     r <- GET("http://example.com/get")
  #   })
  # )
  # test_that("The real request is not affected by the redactor", {
  #   expect_identical(r$url, "http://example.com/get")
  #   expect_identical(resp_body_json(r), list(loaded = TRUE))
  # })
  # test_that("But the mock file gets written to the modified path with altered content", {
  #   # Use replace=TRUE to make sure that "." isn't in the search path.
  #   # We're checking that the original request doesn't have a mock,
  #   # but of course we made it from a mock in the working directory
  #   with_mock_path(d, replace = TRUE, {
  #     expect_GET(
  #       GET("http://example.com/get"),
  #       "http://example.com/get"
  #     )
  #     expect_error(alt <- GET("http://example.com/fakeurl"), NA)
  #     expect_identical(resp_body_json(alt), list(changed = TRUE))
  #   })
  # })

  a <- request("api/") %>%
    req_headers(`Authorization` = "Bearer token") %>%
    req_perform()
  test_that("gsub_response", {
    asub <- gsub_response(a, "api", "OTHER")
    expect_identical(asub$url, "OTHER/")
    expect_identical(resp_body_json(asub), list(value = "OTHER/object1/"))
  })
  test_that("as.redactor", {
    a2 <- prepare_redactor(~ gsub_response(., "api", "OTHER"))(a)
    expect_identical(resp_body_json(a2), list(value = "OTHER/object1/"))
  })

  skip("HTTR2: adapt httr::response to httr2::httr2_response")
  loc <- request("http://httpbin.org/response-headers") %>%
    req_url_query(Location = "http://httpbin.org/status/201") %>%
    req_perform()
  loc_sub <- gsub_response(
    loc, "http://httpbin.org/status/201",
    "http://httpbin.org/status/404"
  )
  test_that("gsub_response touches Location header", {
    expect_identical(
      loc_sub$headers$location,
      "http://httpbin.org/status/404"
    )
    expect_identical(
      loc_sub$all_headers[[1]]$headers$location,
      "http://httpbin.org/status/404"
    )
    expect_identical(
      resp_body_json(loc_sub)$Location,
      "http://httpbin.org/status/404"
    )
  })
  test_that("gsub_response handles URL encoding", {
    skip("TODO: handle URL escaping")
    expect_identical(
      loc_sub$url,
      "http://httpbin.org/response-headers?Location=http%3A%2F%2Fhttpbin.org%2Fstatus%2F404"
    )
  })
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
