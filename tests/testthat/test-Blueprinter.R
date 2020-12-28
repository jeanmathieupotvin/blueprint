test_that("operator %bp% works",
{
    # Test normal usage.
    expect_s3_class(test %bp% 1.0, c("Atomic", "Blueprint"))
    expect_s3_class("test" %bp% 1.0, c("Atomic", "Blueprint"))

    # Test arguments checks.
    expect_error(1L %bp% raw(), regexp = "inappropriate 'symbol'")
    expect_error(c("bad", "name") %bp% raw(), regexp = "inappropriate 'symbol'")

    # Test fallback case for unsupported classes.
    # Currently, we use function `+` that has type/class "builtin".
    expect_null(suppressWarnings(test %bp% `+`))
    expect_warning(test %bp% `+`, regexp = "No suitable blueprint")
})


test_that("pseudo-dispatch to Atomic$new() works",
{
    # Test dispatch to Atomic$new().
    expect_s3_class(test %bp% NULL,          "Atomic")
    expect_s3_class(test %bp% logical(2L),   "Atomic")
    expect_s3_class(test %bp% integer(2L),   "Atomic")
    expect_s3_class(test %bp% single(2L),    "Atomic")
    expect_s3_class(test %bp% double(2L),    "Atomic")
    expect_s3_class(test %bp% complex(2L),   "Atomic")
    expect_s3_class(test %bp% character(2L), "Atomic")
    expect_s3_class(test %bp% raw(2L),       "Atomic")

    # Test if values are correctly passed to Atomic$new().
    ab <- test %bp% sample.int(10L)

    expect_identical(ab$name, "test")
    expect_identical(ab$type, "integer")
    expect_identical(ab$length, 10L)
})
