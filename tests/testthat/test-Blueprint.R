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

    # Test if validator return $self when object is valid.
    testthat::expect_identical(b$validate(), b)

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


testthat::test_that("method $print() works", {
    testthat::expect_identical(quiet(b$print()), b)
    testthat::expect_output(b$print())
})


testthat::test_that("method $format() works", {
    testthat::expect_identical(b$format(), "<Blueprint>")
})


# We only test if wrapper effectively returns a Blueprint object.
# Its actual structure is tested in the context of $new().
testthat::test_that("constructor new_blueprint() function works", {
    testthat::expect_s3_class(new_blueprint(), "Blueprint")
})


testthat::test_that("introspector is_blueprint() function works", {
    testthat::expect_true(is_blueprint(b))
    testthat::expect_false(is_blueprint(b_wrong_scalar))
    testthat::expect_false(is_blueprint(b_wrong_vec))
})


testthat::test_that("test S3 internal methods dispatch", {
    testthat::expect_identical(format(b), "<Blueprint>")
})
