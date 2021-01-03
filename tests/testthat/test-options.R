test_that("opts_json_atomic()",
{
    opts <- opts_json_atomic()

    # Test general structure (not values, we could change them).
    expect_type(opts, "list")
    expect_length(opts, 7L)

    # Test if custom arguments are
    # properly passed to the list.
    opts <- opts_json_atomic(digits = 0L, factor = "integer")
    ref  <- c(opts_json_atomic(), list(digits = 0L, factor = "integer"))

    expect_identical(opts, ref)

    # Test if custom arguments can
    # overwrite values of the list.
    opts <- opts_json_atomic(auto_unbox = FALSE, pretty = FALSE)
    ref  <- list(
        auto_unbox = FALSE,
        pretty     = FALSE,
        force      = FALSE,
        complex    = "string",
        raw        = "base64",
        null       = "null",
        na         = "string"
    )

    expect_identical(opts, ref)

    # Test that nothing else other named arguments
    # can be passed to ... Names must be unique.
    expect_error(opts_json_atomic(1L))
    expect_error(opts_json_atomic(a = 1L, a = 2L))
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
    # of functions can be passed to handlers. Names must be unique.
    expect_error(opts_yaml_handlers(1L))
    expect_error(opts_yaml_handlers(list(fun = 1L)))
    expect_error(opts_yaml_handlers(list(fun = function(){}, function(){})))
    expect_error(opts_yaml_handlers(list(f1 = function(){}, f1 = function(){})))
})
