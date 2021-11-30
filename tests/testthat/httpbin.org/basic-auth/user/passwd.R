structure(list(
  method = "GET", url = "http://httpbin.org/basic-auth/user/passwd",
  status_code = 200L, headers = structure(list(
    Date = "Tue, 30 Nov 2021 20:15:06 GMT",
    `Content-Type` = "application/json", `Content-Length` = "47",
    Connection = "keep-alive", Server = "gunicorn/19.9.0",
    `Access-Control-Allow-Origin` = "*", `Access-Control-Allow-Credentials` = "true"
  ), class = "httr2_headers"),
  body = charToRaw("{\n  \"authenticated\": true, \n  \"user\": \"user\"\n}\n")
), class = "httr2_response")
