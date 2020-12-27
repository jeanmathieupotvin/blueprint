test_that("pad_string() works",
{
    # Test normal usage.
    expect_identical(
        object   = pad_string(c("one", "two", "three"), pad = " "),
        expected = c("one  ", "two  ", "three")
    )
    expect_identical(
        object   = nchar(pad_string(c("one", "two", "three"), pad = "  ")),
        expected = c(7L, 7L, 5L)
    )
    expect_identical(
        object   = pad_string(c("one", "two", "three"), pad = "/pad"),
        expected = c("one/pad/pad", "two/pad/pad", "three")
    )

    # Test argument checks.
    expect_error(pad_string(c(1L, 1L), pad = 1))
    expect_error(pad_string(c("one", "two", "three"), pad = 1))
    expect_error(pad_string(c(NA_character_, "one"), pad = " "))
})
