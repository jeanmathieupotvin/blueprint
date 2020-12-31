test_that("report_errors() works",
{
    # Test normal usage.
    expect_true(report_errors(NULL))
    expect_true(report_errors(c(NULL, NULL, NULL)))

    expect_error(report_errors("msg"),            "msg")
    expect_error(report_errors(c("msg1", "msg2"), "msg1"))
    expect_error(report_errors(c("msg1", "msg2"), "msg2"))

    # Test argument checks.
    expect_error(report_errors(1L),             "character vector")
    expect_error(report_errors(list("string")), "character vector")
})
