source("cachematrix.R")
context("Inverse matrix cached test")

test_that("testing the matrix inverse", {

	m <- matrix(c(2, -3, 5, -7), nrow=2, ncol=2) 
	newm <- makeCacheMatrix(m)
	inverse <- cacheSolve(newm)

	expect_that(inverse, is_equivalent_to(matrix(c(-7, 3, -5, 2))))
})

test_that("testing the ", {
  
  m <- matrix(c(2, -3, 5, -7), nrow=2, ncol=2) 
  newm <- makeCacheMatrix(m)
  expect_that(newm$getInverse(), is_equivalent_to(NULL))
  
  inverse <- cacheSolve(newm)
  expect_that(inverse, is_equivalent_to(matrix(c(-7, 3, -5, 2))))
})