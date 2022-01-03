test_that(".mockPaths works more or less like .libPaths", {
  expect_identical(.mockPaths(), ".")
  .mockPaths("something else")
  expect_identical(.mockPaths(), c("something else", "."))
  # Unique paths
  .mockPaths(".")
  expect_identical(.mockPaths(), c(".", "something else"))
  .mockPaths(NULL)
  expect_identical(.mockPaths(), ".")
})

test_that(".mockPaths default path prefers to be in tests/testthat", {
  d <- tempfile()
  dir.create(file.path(d, "tests", "testthat"), recursive = TRUE)
  old <- setwd(d)
  on.exit(setwd(old))

  expect_identical(.mockPaths(), "tests/testthat")
})

with_mock_api({
  test_that("GET with no query, default mock path", {
    b <- request("api/object1/") %>% req_perform()
    expect_identical(resp_body_json(b), list(object = TRUE))
  })
  test_that("GET with query, default mock path", {
    obj <- request("api/object1/") %>%
      req_url_query(a = 1) %>%
      req_perform()
    expect_equal(
      resp_body_json(obj),
      list(query = list(a = 1), mocked = "yes"),
      list_as_map = TRUE
    )
  })
  test_that("There is no api/object2/ mock", {
    expect_GET(request("api/object2/") %>% req_perform())
  })

  .mockPaths("alt")
  test_that("GET with query, different mock path", {
    obj <- request("api/object1/") %>%
      req_url_query(a = 1) %>%
      req_perform()
    expect_equal(
      resp_body_json(obj),
      list(query = list(a = 1), mocked = "twice"),
      list_as_map = TRUE
    )
  })
  test_that("Now there is an api/object2/ mock", {
    obj <- request("api/object2/") %>% req_perform()
    expect_identical(resp_body_json(obj), list(object2 = TRUE))
  })
  test_that("If the primary mock dir doesn't have a mock, it passes to next", {
    b <- request("api/object1/") %>% req_perform()
    expect_identical(resp_body_json(b), list(object = TRUE))
  })
  test_that("Failure to find a mock in any dir", {
    expect_GET(request("api/NOTAFILE/") %>% req_perform())
  })

  .mockPaths(NULL)
  test_that("NULL mockPaths resets to default", {
    obj <- request("api/object1/") %>%
      req_url_query(a = 1) %>%
      req_perform()
    expect_equal(
      resp_body_json(obj),
      list(query = list(a = 1), mocked = "yes"),
      list_as_map = TRUE
    )
    expect_GET(request("api/object2/") %>% req_perform())
  })
})
