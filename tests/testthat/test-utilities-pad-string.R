test_that("pad_string() works",
{
    # Test normal usage.
    expect_identical(pad_string(c("one", "three"), " "), c("one  ", "three"))
    expect_identical(pad_string(c("one", "three"), "/"), c("one//", "three"))

    # Test argument checks.
    expect_error(pad_string(c(1L, 1L), " "),               "x")
    expect_error(pad_string(c("one", "two"), 1L),          "pad")
    expect_error(pad_string(c(NA_character_, "one"), " "), "anyNA")
})
