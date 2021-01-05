test_that("vapply_1l() works",
{
    # Test normal usage.
    expect_identical(
        vapply_1l(month.name, is.character, TRUE),
        sapply(month.name, is.character)
    )
    expect_identical(
        vapply_1l(month.name, is.character),
        sapply(month.name, is.character, USE.NAMES = FALSE)
    )

    # Test argument checks.
    expect_error(vapply_1l(c(pi, pi), `+`, e1 = 1))
})


test_that("vapply_1c() works",
{
    # Test normal usage.
    expect_identical(
        vapply_1c(month.name, substr, TRUE, start = 1L, stop = 1L),
        structure(substr(month.name, start = 1L, stop = 1L), names = month.name)
    )
    expect_identical(
        vapply_1c(month.name, substr, start = 1L, stop = 1L),
        structure(substr(month.name, start = 1L, stop = 1L), names = NULL)
    )

    # Test argument checks.
    expect_error(vapply_1c(c(pi, pi), `+`, TRUE, e1 = 1))
})
