testthat::test_that("$new() works and instance has an appropriate structure",
{
    b <- blueprint::Blueprint$new()
    pkg_ver <- as.character(utils::packageVersion("blueprint"))

    # Test general structure.
    testthat::expect_type(b, "environment")

    # Test public fields.
    testthat::expect_type(b$is_blueprint, "logical")
    testthat::expect_type(b$blueprint_version, "character")

    # Test methods.
    testthat::expect_type(b$initialize, "closure")
    testthat::expect_type(b$validate, "closure")
    testthat::expect_type(b$format, "closure")
    testthat::expect_type(b$print, "closure")

    # Test class inheritance.
    testthat::expect_s3_class(b, "Blueprint")
    testthat::expect_s3_class(b, "R6")

    # Test public fields.
    # They should have constant values, so we check these.
    testthat::expect_true(isTRUE(b$is_blueprint))
    testthat::expect_identical(b$blueprint_version, pkg_ver)
})


testthat::test_that("$validate() works",
{
    b <- blueprint::Blueprint$new()

    # Test if $validate() returns $self invisibly when valid.
    testthat::expect_identical(b$validate(), b)
    testthat::expect_invisible(quiet(b$print()))

    # Inject type related errors.
    b$is_blueprint      <- "a character"
    b$blueprint_version <- 1L

    # Test type related errors.
    testthat::expect_error(b$validate(), regexp = "errors detected")

    # Inject length related errors.
    b$is_blueprint      <- c(TRUE, TRUE)
    b$blueprint_version <- c("0.1.1", "0.1.2")

    # Test length related errors.
    testthat::expect_error(b$validate(), regexp = "errors detected")
})


testthat::test_that("$print() works",
{
    b <- blueprint::Blueprint$new()

    # Test if $print() returns $self invisibly when valid.
    testthat::expect_identical(quiet(b$print()), b)
    testthat::expect_invisible(quiet(b$print()))

    # Test if $print() prints something to the console.
    testthat::expect_output(b$print())
})


testthat::test_that("$format() works",
{
    b <- blueprint::Blueprint$new()

    # Test if $format() returns the appropriate string.
    testthat::expect_identical(b$format(), "<Blueprint>")
})


testthat::test_that("is_blueprint() works",
{
    b <- blueprint::Blueprint$new()

    # Test if it normally works.
    testthat::expect_true(is_blueprint(b))

    # Test value related error.
    # Should return FALSE if $is_blueprint is FALSE.
    b$is_blueprint <- FALSE
    testthat::expect_false(is_blueprint(b))

    # Test type related error.
    # Should return FALSE if $is_blueprint is not a scalar logical TRUE.
    b$is_blueprint <- "false"
    testthat::expect_false(is_blueprint(b))
})


testthat::test_that("valid_blueprint() works",
{
    b <- blueprint::Blueprint$new()

    # Test if it normally works.
    testthat::expect_s3_class(valid_blueprint(b), "R6")
    testthat::expect_s3_class(valid_blueprint(b), "Blueprint")

    # Test if an error is returned if tested object is not a Blueprint object.
    testthat::expect_error(valid_blueprint(1L), regexp = "not a 'Blueprint'")
})


testthat::test_that("test S3 internal methods dispatch",
{
    b <- blueprint::Blueprint$new()

    # Test if $format() output is returned.
    testthat::expect_identical(format(b), "<Blueprint>")
})
