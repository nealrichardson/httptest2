# expect_request without_internet

    Code
      expect_POST(req_perform(this_req), "http://httpbin.not/get", "{\"test\":false}")
    Condition
      Error:
      ! An unexpected request was made:
        Actual:   POST http://httpbin.not/get {"test":true}
        Expected: POST http://httpbin.not/get {"test":false}

