test_that("jsonlite_atomic_opts()",
{
    opts <- opts_jsonlite_atomic()

    # Test general structure (not values, we could change them).
    expect_type(opts, "list")
    expect_length(opts, 7L)
})


test_that("opts_yaml_handlers()",
{
    opts <- opts_yaml_handlers()

    # Test general structure (not values, we could change them).
    expect_type(opts, "list")
    expect_length(opts, 1L)

    # Test if custom handlers are
    # properly passed to the list.
    handlers <- list(
        numeric = function(x) { return(x) },
        logical = function(x) { return(x) }
    )

    expect_named(opts_yaml_handlers(handlers), c("raw", "numeric", "logical"))

    # Test if custom handlers can
    # overwrite values of the list.
    handlers <- list(
        raw     = function(x) { return(x) },
        logical = function(x) { return(x) }
    )

    expect_named(opts_yaml_handlers(handlers), c("raw", "logical"))
    expect_identical(opts_yaml_handlers(handlers)$raw("success"), "success")

    # Test that nothing else other than named list
    # of functions can be passed to handlers. Names
    # must be unique.
    expect_error(opts_yaml_handlers(1L))
    expect_error(opts_yaml_handlers(list()))
    expect_error(opts_yaml_handlers(list(fun = 1L)))
    expect_error(opts_yaml_handlers(list(fun = function(){}, function(){})))
    expect_error(opts_yaml_handlers(list(f1 = function(){}, f1 = function(){})))
})
