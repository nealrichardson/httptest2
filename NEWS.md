# httptest2 0.0.0.9000

Initial port of `httptest` to work on top of `httr2`. All APIs preserved except:

* Mock file hashes and error messages for multipart or file upload requests have changed.
* Redacting functions are applied to both the `httr2_response` and `httr2_request` objects, the latter in order to be able to alter the URL/mock file path being written. You do not need separate "redactors" and "requesters".

Changes to function signatures:

* `save_response()` requires a `file` path argument because `httr2_response` objects do not contain their `request`, which is needed to construct the mock file path
* `build_mock_url()` no longer accepts a string URL as an input; it only accepts `request` objects
* The `path` argument to `capture_requests()` and `start_capturing()` has been removed; instead set the mock path explicitly with `.mockPaths()` or use `with_mock_path()`.

Some functions were removed:

* `redact_auth()`, which had previously been reduced to an alias for `redact_cookies()`
* `with_fake_http()`
* `public()`
* `fake_response()` (just use `httr2::response()`)
* `gsub_request()` and `set_requester()` (no longer necessary)

Other improvements:

* The failure message when an unexpected request is made is more clear and is distinguished from other "errors".