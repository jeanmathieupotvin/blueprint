test_that("validate_blueprint() works",
{
    # Test normal usage.
    expect_error(
        validate_blueprint("$port must be an integer of length 1."),
        regexp = "\\$port must be an integer of length 1."
    )
    expect_error(
        validate_blueprint(
            c("$field1 must be an integer of length 1.",
              "$field2 must be a character of length 1.")
        ),
        regexp = "\\$field1 must be an integer of length 1.",
    )
    expect_error(
        validate_blueprint(
            c("$field1 must be an integer of length 1.",
              "$field2 must be a character of length 1.")
        ),
        regexp = "\\$field2 must be a character of length 1.",
    )

    # Test if TRUE is returned when NULLs are passed to function.
    expect_true(validate_blueprint(NULL))
    expect_true(validate_blueprint(c(NULL, NULL, NULL)))

    # Test argument checks.
    expect_error(validate_blueprint(1L), regexp = "character vector")
    expect_error(validate_blueprint(list("string")), regexp = "character vector")
})
