# httptest2 0.0.0.9000

Initial port of `httptest` to work on top of `httr2`. All APIs preserved except:

* `save_response()` requires a `file` path argument because `httr2_response` objects do not contain their `request`, which is needed to construct the mock file path
* `gsub_request()` was removed because it is no longer necessary
* `redact_auth()`, which had previously been reduced to an alias for `redact_cookies()`, was removed
* `with_fake_http()` was removed
* `fake_response()` was removed (just use `httr2::response()`)
* `build_mock_url()` no longer accepts a string URL as an input; it only accepts `request` objects
