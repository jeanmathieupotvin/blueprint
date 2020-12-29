test_that("validate_blueprint() works",
{
    # Test normal usage.
    expect_true(validate_blueprint(NULL))
    expect_true(validate_blueprint(c(NULL, NULL, NULL)))

    expect_error(validate_blueprint("msg"),            "msg")
    expect_error(validate_blueprint(c("msg1", "msg2"), "msg1"))
    expect_error(validate_blueprint(c("msg1", "msg2"), "msg2"))

    # Test argument checks.
    expect_error(validate_blueprint(1L),             "character vector")
    expect_error(validate_blueprint(list("string")), "character vector")
})
