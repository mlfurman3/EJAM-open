test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

testthat::test_that("proxistat works at all", {
  expect_no_error({
    proxistat(testpoints_10)
  })
  
})
