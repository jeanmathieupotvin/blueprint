testthat::test_that("assertions for scalar characters work",
{
    testthat::expect_false(is_scalar_character())
    testthat::expect_false(is_scalar_character(NULL))
    testthat::expect_false(is_scalar_character(1L))
    testthat::expect_false(is_scalar_character(c("vector", "string")))
    testthat::expect_false(is_scalar_character(list("string")))
    testthat::expect_true(is_scalar_character("string"))
})


testthat::test_that("assertions for scalar logicals work",
{
    testthat::expect_false(is_scalar_logical())
    testthat::expect_false(is_scalar_logical(NULL))
    testthat::expect_false(is_scalar_logical(1L))
    testthat::expect_false(is_scalar_logical(c(TRUE, FALSE)))
    testthat::expect_false(is_scalar_logical(list(TRUE)))
    testthat::expect_true(is_scalar_logical(TRUE))
})


testthat::test_that("assertions for scalar integers work",
{
    testthat::expect_false(is_scalar_integer())
    testthat::expect_false(is_scalar_integer(NULL))
    testthat::expect_false(is_scalar_integer(1))
    testthat::expect_false(is_scalar_integer(TRUE))
    testthat::expect_false(is_scalar_integer(c(1L, 2L)))
    testthat::expect_false(is_scalar_integer(list(1L)))
    testthat::expect_true(is_scalar_integer(1L))
})


testthat::test_that("assertions for scalar numerics work",
{
    testthat::expect_false(is_scalar_numeric())
    testthat::expect_false(is_scalar_numeric(NULL))
    testthat::expect_false(is_scalar_numeric(c(1, 2)))
    testthat::expect_false(is_scalar_numeric(list(1)))
    testthat::expect_true(is_scalar_numeric(1L))
    testthat::expect_true(is_scalar_numeric(1))
})
