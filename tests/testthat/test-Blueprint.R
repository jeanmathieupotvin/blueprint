test_that("instance has an appropriate structure",
{
    b <- Blueprint$new()

    # Test general structure.
    expect_type(b, "environment")

    # Test class inheritance.
    expect_s3_class(b, "Blueprint")
    expect_s3_class(b, "R6")

    # Test active fields.
    # They should have constant values, so we check these.
    expect_true(b$is_blueprint)
    expect_identical(b$version, as.character(utils::packageVersion("blueprint")))
})


test_that("$new()",
{
    # Test normal usage.
    expect_s3_class(Blueprint$new(), "Blueprint")
})


test_that("$validate()",
{
    b <- Blueprint$new()

    # Test if output is returned invisibly.
    expect_invisible(b$validate())
    expect_identical(b$validate(), b)
})


test_that("$format()",
{
    expect_identical(Blueprint$new()$format(), "<Blueprint>")
})


test_that("$get()",
{
    # $get() can only be tested partially in the context of
    # class Blueprint because it is always valid (it has no
    # field). It is thoroughly tested elsewhere.

    b <- Blueprint$new()

    # Test normal usage.
    expect_identical(b$get(), b)
    expect_true(b$get("is_blueprint"))
    expect_null(b$get("not_existent_field"))

    # Test arguments checks.
    expect_error(b$get(1L),          "scalar character")
    expect_error(b$get(c("1", "2")), "scalar character")
})


test_that("set()",
{
    # $set() can only be tested partially in the context of
    # class Blueprint because it has no modifiable fields.
    # It is thoroughly tested elsewhere.

    b <- Blueprint$new()

    expect_error(b$set("version", "1"), "no modifiable")

    # Test arguments checks.
    expect_error(b$set(1L),          "scalar character")
    expect_error(b$set(c("1", "2")), "scalar character")
})


test_that("is_blueprint()",
{
    expect_true(is_blueprint(Blueprint$new()))
    expect_false(is_blueprint(1L))
})


test_that("valid_blueprint()",
{
    expect_true(valid_blueprint(Blueprint$new()))
    expect_error(valid_blueprint(1L), "not a 'Blueprint'")
})
