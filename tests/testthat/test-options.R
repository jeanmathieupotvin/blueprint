test_that("jsonlite_atomic_opts() works",
{
    # Generate list of options.
    opts <- opts_jsonlite_atomic()

    # Test general structure (not values, we could change them).
    expect_type(opts, "list")
    expect_length(opts, 7L)
})
