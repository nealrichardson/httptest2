# expect_VERB when no request is made

    Code
      expect_POST("just a string")
    Condition
      Error:
      ! Expected `object` to throw a error with class <httptest2_request>.

# expect_request without_internet

    Code
      expect_POST(req_perform(this_req), "http://httpbin.not/get", "{\"test\":false}")
    Condition
      Error:
      ! An unexpected request was made:
        Actual:   POST http://httpbin.not/get {"test":true}
        Expected: POST http://httpbin.not/get {"test":false}

