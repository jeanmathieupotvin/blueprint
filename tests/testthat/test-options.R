testthat::test_that("jsonlite_atomic_opts() works",
{
    # Generate list of options.
    opts <- jsonlite_atomic_opts()

    # Test general structure.
    testthat::expect_type(opts, "list")
    testthat::expect_length(opts, 7L)
})
