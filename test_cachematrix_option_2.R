source("cachematrix_option_2.R")

context("option 2 inverse matrix cached test")

test_that("testing the matrix inverse", {

	normalMatrix <- matrix(c(2, -3, 5, -7), nrow=2, ncol=2)
	 
	decoratedMatrix <- inverseCacheableMatrix(normalMatrix)

	inverse <- decoratedMatrix$getInverse()

	expect_that(inverse, is_equivalent_to(matrix(c(-7, 3, -5, 2))))
})