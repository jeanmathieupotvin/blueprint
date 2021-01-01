test_that("inject.default()",
{
    # This method focuses on named vectors.

    x <- c(a = "a", b = "b")

    # Test normal usage.
    expect_identical(inject(x), x)
    expect_identical(inject(x, x), x)
    expect_identical(inject(x, c(c = "c")),           c(a = "a",  b = "b", c = "c"))
    expect_identical(inject(x, c(a = "aa", b = "b")), c(a = "aa", b = "b"))
    expect_identical(inject(x, c(c = "c",  d = "d")), c(a = "a",  b = "b", c = "c", d = "d"))

    # Test argument checks.
    expect_error(inject(x, 1L),                      "is_named_vctr\\(values\\)")
    expect_error(inject(x, c(b = "b", b = "bb")),    "is_named_vctr\\(values\\)")
    expect_error(inject(c(a = "a", a = "aa")),       "is_named_vctr\\(x\\)")
    expect_error(inject(c(1L, b = "b"), c(c = "c")), "is_named_vctr\\(x\\)")
})


test_that("inject.list()",
{
    x <- list(a = "a", b = "b")

    # Test normal usage.
    expect_identical(inject(x), x)
    expect_identical(inject(x, x), x)
    expect_identical(inject(x, list(c = "c")),           list(a = "a",  b = "b", c = "c"))
    expect_identical(inject(x, list(a = "aa", b = "b")), list(a = "aa", b = "b"))
    expect_identical(inject(x, list(c = "c",  d = "d")), list(a = "a",  b = "b", c = "c", d = "d"))

    # Test argument checks.
    expect_error(inject(x, 1L),                      "is_named_list\\(values\\)")
    expect_error(inject(x, list(b = "b", b = "bb")), "is_named_list\\(values\\)")
    expect_error(inject(list(1L, b = "b")),          "is_named_list\\(x\\)")
    expect_error(inject(list(a = "a", a = "aa")),    "is_named_list\\(x\\)")
})
