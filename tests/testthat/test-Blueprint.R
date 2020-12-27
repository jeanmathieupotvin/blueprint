test_that("$new() works and instance has an appropriate structure",
{
    b <- Blueprint$new()
    pkg_ver <- as.character(utils::packageVersion("blueprint"))

    # Test general structure.
    expect_type(b, "environment")

    # Test public fields.
    expect_type(b$is_blueprint, "logical")
    expect_type(b$blueprint_version, "character")

    # Test methods.
    expect_type(b$initialize, "closure")
    expect_type(b$validate, "closure")
    expect_type(b$format, "closure")
    expect_type(b$print, "closure")

    # Test class inheritance.
    expect_s3_class(b, "Blueprint")
    expect_s3_class(b, "R6")

    # Test public fields.
    # They should have constant values, so we check these.
    expect_true(isTRUE(b$is_blueprint))
    expect_identical(b$blueprint_version, pkg_ver)
})


test_that("$validate() works",
{
    b <- Blueprint$new()

    # Test if $validate() returns $self invisibly when valid.
    expect_identical(b$validate(), b)
    expect_invisible(quiet(b$print()))

    # Inject type related errors.
    b$is_blueprint      <- "a character"
    b$blueprint_version <- 1L

    # Test type related errors.
    expect_error(b$validate(), regexp = "errors detected")

    # Inject length related errors.
    b$is_blueprint      <- c(TRUE, TRUE)
    b$blueprint_version <- c("0.1.1", "0.1.2")

    # Test length related errors.
    expect_error(b$validate(), regexp = "errors detected")
})


test_that("$print() works",
{
    b <- Blueprint$new()

    # Test if $print() returns $self invisibly when valid.
    expect_identical(quiet(b$print()), b)
    expect_invisible(quiet(b$print()))

    # Test if $print() prints something to the console.
    expect_output(b$print())
})


test_that("$format() works",
{
    b <- Blueprint$new()

    # Test if $format() returns the appropriate string.
    expect_identical(b$format(), "<Blueprint>")
})


test_that("is_blueprint() works",
{
    b <- Blueprint$new()

    # Test if it normally works.
    expect_true(is_blueprint(b))

    # Test value related error.
    # Should return FALSE if $is_blueprint is FALSE.
    b$is_blueprint <- FALSE
    expect_false(is_blueprint(b))

    # Test type related error.
    # Should return FALSE if $is_blueprint is not a scalar logical TRUE.
    b$is_blueprint <- "false"
    expect_false(is_blueprint(b))
})


test_that("valid_blueprint() works",
{
    b <- Blueprint$new()

    # Test if it normally works.
    expect_s3_class(valid_blueprint(b), "R6")
    expect_s3_class(valid_blueprint(b), "Blueprint")

    # Test if an error is returned if tested object is not a Blueprint object.
    expect_error(valid_blueprint(1L), regexp = "not a 'Blueprint'")
})
