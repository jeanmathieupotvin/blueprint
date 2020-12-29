test_that("$new() works and instance has an appropriate structure",
{
    b <- Blueprint$new()
    pkg_ver <- as.character(utils::packageVersion("blueprint"))

    # Test general structure.
    expect_type(b, "environment")

    # Test class inheritance.
    expect_s3_class(b, "Blueprint")
    expect_s3_class(b, "R6")

    # Test methods.
    expect_type(b$initialize, "closure")
    expect_type(b$validate,   "closure")
    expect_type(b$print,      "closure")
    expect_type(b$format,     "closure")
    expect_type(b$get,        "closure")
    expect_type(b$set,        "closure")

    # Test active fields.
    # They should have constant values, so we check these.
    expect_true(b$is_blueprint)
    expect_identical(b$version, pkg_ver)
})


test_that("$validate() works",
{
    b <- Blueprint$new()

    expect_invisible(b$validate())
    expect_identical(b$validate(), b)
})


test_that("$print() works",
{
    expect_output(Blueprint$new()$print())
})


test_that("$format() works",
{
    expect_identical(Blueprint$new()$format(), "<Blueprint>")
})


test_that("$get() works",
{
    b <- Blueprint$new()

    expect_identical(b$get(), b)
    expect_true(b$get("is_blueprint"))
    expect_null(b$get("not_existent_field"))
})


test_that("set() works",
{
    # $set() can only be tested partially in the context of
    # class Blueprint because it has no modifiable fields.
    # It is thoroughly tested elsewhere.

    expect_error(Blueprint$new()$set("version", "1"), "no modifiable")
})


test_that("is_blueprint() works",
{
    expect_true(is_blueprint(Blueprint$new()))
    expect_false(is_blueprint(1L))
})


test_that("valid_blueprint() works",
{
    expect_s3_class(valid_blueprint(Blueprint$new()), "Blueprint")
    expect_error(valid_blueprint(1L), "not a 'Blueprint'")
})
