# httptest2 1.2.1

* Address check warnings with `httr2` 1.2

# httptest2 1.2.0

* `build_mock_url()` now works with multipart body requests containing character strings or raw data (#40, @jmaspons)
* Compatibility with upcoming `httr2` release (#51, @hadley)

# httptest2 1.1.0

* `request` is now removed when saving `httr2_response` objects. In earlier versions of `httr2`, requests were not included in responses, but in httr2 1.0.0, [they were added](https://github.com/r-lib/httr2/pull/359) in order to improve error messages. *If you recorded any responses with httr2 >= 1.0 and httptest2 prior to this version, you may have leaked auth secrets: this would happen if your requests included auth information (as in an `Authentication` header), and the response was saved in a .R file, not simplified to .json or other response-body-only formats. Please inspect your recorded responses and invalidate any tokens that were exposed.*
* `save_response()` now works with `simplify = TRUE` when the response body is empty (#37, @jmaspons)

# httptest2 1.0.0

* `capture_requests()` can be used without loading `library(httptest2)` (#22, @kforner)
* `gsub_response()` safely redacts responses with an empty body (#28)
* Remove unused feature that would allow mocking with arbitrary local files via invalid URLs
* httr2 compatibility: workarounds for issues in `httr2 < 0.2` have been removed, and adaptations for `httr2 >= 1.0` added
* Tests and examples no longer use `httpbin.org`

# httptest2 0.1.0

Initial port of `httptest` to work on top of `httr2`. All APIs preserved except:

* Mock file hashes and error messages for requests with multipart forms or file uploads have changed. Aside from this, all mock files and file paths recorded by `httptest` using `httr` will work with `httptest2` and return valid `httr2_response` objects when loaded.
* Redacting functions are applied to both the `httr2_response` and `httr2_request` objects, the latter in order to be able to alter the URL/mock file path being written. You do not need separate "redactors" and "requesters".
* `expect_header()` has been renamed to `expect_request_header()` and takes headers to match by argument name rather than a single string "Header: value". This allows more flexibility in matching headers, plus the ability to match multiple headers. It also no longer uses `warning()` to extract the headers, so test failures are cleaner.
* The failure message when an unexpected request is made is more clear and is distinguished from other "errors". Expected mock file paths are more clearly shown when appropriate, and they are no longer included in the message itself that you would assert in `expect_GET()`, `expect_POST()`, et al.
* All `options()` have been renamed to match the package name (e.g. `options(httptest2.verbose)`)
* With `options(httptest2.verbose = TRUE)`, messages are printed in more places; messages about when a package redactor is called are now only printed if the option is set.

Some functions were removed:

* `redact_auth()`, which had previously been reduced to an alias for `redact_cookies()`
* `with_fake_http()`
* `public()`
* `expect_json_equivalent()` (just use `testthat::expect_equal(list_as_map = TRUE)`)
* `skip_if_disconnected()` (just use `testthat::skip_if_offline()`)
* `fake_response()` (just use `httr2::response()`)
* `gsub_request()` and `set_requester()` (no longer necessary)

Changes to function signatures:

* The `path` argument to `capture_requests()` and `start_capturing()` has been removed; instead set the mock path explicitly with `.mockPaths()` or use `with_mock_dir()`.
* Internal function `save_response()` requires a `file` path argument because `httr2_response` objects do not contain their `request`, which is needed to construct the mock file path
* Internal function `build_mock_url()` no longer accepts a string URL as an input; it only accepts `request` objects
