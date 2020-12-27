# Encoding returns UTF-8 for UTF-8 marked characters.
# ASCII character are never marked. Encoding() returns unknown for them.


test_that("as_utf8() generic works",
{
    # Test dispatch. Any non UTF-8 R values should just be returned as is.
    expect_identical(as_utf8(list()), list())
    expect_identical(as_utf8(character()), character())
    expect_identical(as_utf8(double()), double())
})


test_that("as_utf8() works on atomic structures",
{
    # Create an atomic structure (not strict).
    struct <- c(unknown = "eagjh", unknown = "1234", `UTF-8` = "éèë½¾")

    expect_equal(as_utf8(struct), struct)
    expect_equal(Encoding(as_utf8(struct)), names(struct))
})


test_that("as_utf8() works on recursive structures",
{
    # Create a recursive structure itself containing a recursive structure.
    struct <- list(
        unknown = "eagjh",
        unknown = "1234",
        `UTF-8` = "é",
        recurse = list(
            `UTF-8` = "éèë½¾",
            `UTF-8` = "ê",
            unknown = "+-"
        )
    )

    expect_equal(as_utf8(struct), struct)
    expect_equivalent(
        list("unknown", "unknown", "UTF-8", list("UTF-8", "UTF-8", "unknown")),
        rapply(as_utf8(struct), Encoding, how = "replace")
    )
})
