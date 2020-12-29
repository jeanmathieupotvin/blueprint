test_that("inject() works on atomic structures",
{
    v <- c(a = "a", b = "b")

    # Test normal usage.
    expect_identical(inject(v), v)
    expect_identical(inject(v, b = "bb", c = "c"), c(a = "a", b = "bb", c = "c"))
    expect_identical(inject(v, a = "a", b = "b"),  c(a = "a", b = "b"))

    # Test argument checks.
    expect_error(inject(v, 1L),                         "dotnames")
    expect_error(inject(v, b = "b", b = "bb"),          "dotnames")
    expect_error(inject(c(a = "a", a = "aa"), b = "b"), "objnames")
    expect_error(inject(c(1L, b = "b"), c = "c"),       "objnames")
})


test_that("inject() works on recursive structures",
{
    l <- list(a = "a", b = "b")

    # Test normal usage.
    expect_identical(inject(l), l)
    expect_identical(inject(l, b = "bb", c = "c"), list(a = "a", b = "bb", c = "c"))
    expect_identical(inject(l, a = "a", b = "b"),  list(a = "a", b = "b"))

    # Test argument checks.
    expect_error(inject(l, 1L),                            "dotnames")
    expect_error(inject(l, b = "b", b = "bb"),             "dotnames")
    expect_error(inject(list(a = "a", a = "aa"), b = "b"), "objnames")
    expect_error(inject(list(1L, b = "b"), c = "c"),       "objnames")
})
