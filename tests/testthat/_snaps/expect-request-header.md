# expect_request_header with mock API

    Code
      expect_request_header(request("https://test.api/object1/") %>% req_headers(
        Accept = "image/png") %>% req_perform(), accept = "image/jpeg")
    Condition
      Error:
      ! Expected Header "accept" to match regexp "image/jpeg".
      Actual text:
      x | image/png

---

    Code
      expect_request_header(request("https://test.api/object1/") %>% req_headers(
        Accept = "image/png") %>% req_perform(), accept = NULL)
    Condition
      Error:
      ! Expected Header "accept" to be NULL.
      Differences:
      `actual` is a character vector ('image/png')
      `expected` is NULL

