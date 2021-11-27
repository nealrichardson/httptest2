test_that("currently_offline interacts with the mock contexts", {
  with_mock_api(
    # Supply a URL we have a mock for
    expect_false(currently_offline("http://httpbin.org/status/204"))
  )
  without_internet(
    expect_true(currently_offline("http://httpbin.org/status/204"))
  )
})

test_that("skip_if_disconnected when disconnected", {
  without_internet({
    skip_if_disconnected("This should skip")
    expect_true(FALSE)
  })
})
test_that("skip_if_disconnected when 'connected'", {
  with_mock_api({
    skip_if_disconnected(
      "This should not skip",
      "http://httpbin.org/status/204"
    )
    expect_failure(expect_true(FALSE))
  })
})
