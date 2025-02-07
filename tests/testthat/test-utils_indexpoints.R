
# tests for 
# 
#      indexfrs() 
# and 
#      indexpoints()

test_that("indexpoints works at all", {
  expect_no_error({
    indexpoints(testpoints_10[1:2,])
    rm(custom_index, envir = globalenv()) # default name of index
    })
  expect_no_error({
    indexpoints(testpoints_10[1:2,], indexname = "tempindex")
    rm(tempindex, envir = globalenv())
    })
  expect_no_error({
    tempenv <- new.env()
    indexpoints(testpoints_10[1:2,], envir = tempenv)
    rm(tempenv) # delete the environment, including anything created in it
    })
})
test_that("indexpoints assigns object to global envt", {
  indexpoints(testpoints_10[1:2,], indexname = "tempindex")
  expect_true(exists("tempindex", envir = globalenv()))
  rm(tempindex, envir = globalenv())
})
test_that("indexpoints can assign object to a specified envt", {
  tempenv <- new.env()  
  indexpoints(testpoints_10[1:2,], indexname = "tempindex", envir = tempenv)
  expect_true(exists("tempindex", envir = tempenv))
  rm(tempenv) 
})
test_that("indexpoints returns right class", {
  tempenv <- new.env()
  x <- indexpoints(testpoints_10[1:2,], envir = tempenv)
  expect_true("QuadTree" %in% class(x))
  expect_s4_class(x, "QuadTree")
  rm(tempenv)
})
test_that("indexpoints number of points looks right", {
  tempenv <- new.env()
  n <- 10
  x <- indexpoints(testpoints_10, envir = tempenv)
  expect_equal(x@dataNodes, n)
  rm(tempenv)
})
