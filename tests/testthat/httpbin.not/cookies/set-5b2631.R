structure(list(
  method = "GET", url = "http://httpbin.not/cookies",
  status_code = 200L, headers = structure(
    list(
      Date = "Sun, 05 Dec 2021 20:01:43 GMT",
      `Content-Type` = "application/json", `Content-Length` = "44",
      Connection = "keep-alive", Server = "gunicorn/19.9.0",
      `Access-Control-Allow-Origin` = "*", `Access-Control-Allow-Credentials` = "true",
      # Set-Cookie is artificially added here because it is lost in a 302 redirect
      `Set-Cookie` = "token=12345; Domain=example.com; Max-Age=31536000; Path=/"
    ),
    class = "httr2_headers"
  ),
  body = charToRaw("{\n  \"cookies\": {\n    \"token\": \"12345\"\n  }\n}\n")
), class = "httr2_response")
