testthat::test_that("is_scalar_character() works",
{
    testthat::expect_false(is_scalar_character())
    testthat::expect_false(is_scalar_character(NULL))
    testthat::expect_false(is_scalar_character(1L))
    testthat::expect_false(is_scalar_character(c("vector", "string")))
    testthat::expect_false(is_scalar_character(list("string")))
    testthat::expect_true(is_scalar_character("string"))
})


testthat::test_that("is_scalar_logical() works",
{
    testthat::expect_false(is_scalar_logical())
    testthat::expect_false(is_scalar_logical(NULL))
    testthat::expect_false(is_scalar_logical(1L))
    testthat::expect_false(is_scalar_logical(c(TRUE, FALSE)))
    testthat::expect_false(is_scalar_logical(list(TRUE)))
    testthat::expect_true(is_scalar_logical(TRUE))
})


testthat::test_that("is_scalar_integer() works",
{
    testthat::expect_false(is_scalar_integer())
    testthat::expect_false(is_scalar_integer(NULL))
    testthat::expect_false(is_scalar_integer(1))
    testthat::expect_false(is_scalar_integer(TRUE))
    testthat::expect_false(is_scalar_integer(c(1L, 2L)))
    testthat::expect_false(is_scalar_integer(list(1L)))
    testthat::expect_true(is_scalar_integer(1L))
})


testthat::test_that("is_scalar_numeric() works",
{
    testthat::expect_false(is_scalar_numeric())
    testthat::expect_false(is_scalar_numeric(NULL))
    testthat::expect_false(is_scalar_numeric(c(1, 2)))
    testthat::expect_false(is_scalar_numeric(list(1)))
    testthat::expect_true(is_scalar_numeric(1L))
    testthat::expect_true(is_scalar_numeric(1))
})


testthat::test_that("is_strict_atomic() works",
{
    # Test normal usage on atomic types.
    testthat::expect_true(is_strict_atomic(NULL))                         # NULL.
    testthat::expect_true(is_strict_atomic(logical(2L)))                  # Logical.
    testthat::expect_true(is_strict_atomic(single(2L)))                   # Single (numeric with a Csingle attribute).
    testthat::expect_true(is_strict_atomic(integer(2L)))                  # Integer.
    testthat::expect_true(is_strict_atomic(numeric(2L)))                  # Numeric.
    testthat::expect_true(is_strict_atomic(double(2L)))                   # Numeric.
    testthat::expect_true(is_strict_atomic(complex(2L)))                  # Complex.
    testthat::expect_true(is_strict_atomic(character(2L)))                # Character.
    testthat::expect_true(is_strict_atomic(raw(2L)))                      # Raw.

    # Test normal usage on some recursive structures.
    testthat::expect_false(is_strict_atomic(list()))                      # Lists.
    testthat::expect_false(is_strict_atomic(pairlist(test = 1L)))         # Pairlists.
    testthat::expect_false(is_strict_atomic(matrix()))                    # Implicit: matrices.
    testthat::expect_false(is_strict_atomic(array()))                     # Implicit: arrays.

    # Test normal usage on some R low-level types.
    testthat::expect_false(is_strict_atomic(function(){}))                # Functions and closures.
    testthat::expect_false(is_strict_atomic(as.name("test")))             # Names and symbols.
    testthat::expect_false(is_strict_atomic(environment()))               # Environment.
    testthat::expect_false(is_strict_atomic(call("vector")))              # Calls.
    testthat::expect_false(is_strict_atomic(str2expression("test")))      # Expression (language).
    testthat::expect_false(is_strict_atomic(methods::new("externalptr"))) # Externalptr.
})
