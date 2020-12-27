test_that("inject() works on atomic structures",
{
    simplevec <- c(opt1 = "test", opt2 = "test")

    # Test normal usage.
    expect_identical(
        inject(simplevec, opt2 = "succeed", opt3 = "new"),
        c(opt1 = "test", opt2 = "succeed", opt3 = "new")
    )

    # Test injection of equal values (it should work).
    expect_identical(
        inject(simplevec, opt1 = "test", opt2 = "new"),
        c(opt1 = "test", opt2 = "new")
    )

    # Test argument checks.
    # We focus on unnamed values and/or not unique names in either .Obj or ...
    expect_error(inject(simplevec, opt2 = "test1", opt2 = "test2"))
    expect_error(
        inject(
            c(opt1 = "contains", opt1 = "errors"),
            opt1 = "test1", opt2 = "test2"
        )
    )
    expect_error(inject(simplevec, 1L))
    expect_error(inject(c(1L, opt2 = "test"), opt1 = "test1"))
})


test_that("inject() works on recursive structures",
{
    simplelist <- list(opt1 = "test", opt2 = "test")

    # Test normal usage.
    expect_identical(
        inject(simplelist,  opt2 = "succeed", opt3 = "new"),
        list(opt1 = "test", opt2 = "succeed", opt3 = "new")
    )

    # Test injection of equal values (it should work).
    expect_identical(
        inject(simplelist,  opt1 = "test", opt2 = "new"),
        list(opt1 = "test", opt2 = "new")
    )

    # Test argument checks.
    # We focus on unnamed values and/or not unique names in either .Obj or ...
    expect_error(inject(simplelist, opt2 = "test1", opt2 = "test2"))
    expect_error(
        inject(
            list(opt1 = "contains", opt1 = "errors"),
            opt1 = "test1", opt2 = "test2"
        )
    )
    expect_error(inject(simplelist, 1L))
    expect_error(inject(list(1L, opt2 = "test"), opt1 = "test1"))
})
