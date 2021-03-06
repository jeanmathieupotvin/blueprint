test_that("is_single() works",
{
    # A single value is a double with a
    # Csingle attribute set equal to TRUE.

    # Test normal usage.
    expect_true(is_single(single()))
    expect_true(is_single(as.single(double())))
    expect_false(is_single(double()))
    expect_false(is_single(numeric()))
    expect_false(is_single(structure(double(), Csingle = FALSE)))
})


test_that("is_scalar_character() works",
{
    # Test normal usage.
    expect_true(is_scalar_character("chr"))
    expect_false(is_scalar_character(NULL))
    expect_false(is_scalar_character(1L))
    expect_false(is_scalar_character(c("vector", "chr")))
    expect_false(is_scalar_character(list("chr")))

    # Test if NAs are properly handled.
    expect_true(is_scalar_character(NA_character_,  TRUE))
    expect_false(is_scalar_character(NA_character_, FALSE))

    # Test arguments checks.
    expect_error(is_scalar_character("chr", 1L),          "accept_na")
    expect_error(is_scalar_character("chr", c("t", "f")), "accept_na")
})


test_that("is_scalar_logical() works",
{
    # Test normal usage.
    expect_true(is_scalar_logical(TRUE))
    expect_false(is_scalar_logical(NULL))
    expect_false(is_scalar_logical(1L))
    expect_false(is_scalar_logical(c(TRUE, FALSE)))
    expect_false(is_scalar_logical(list(TRUE)))

    # Test if NAs are properly handled.
    expect_true(is_scalar_logical(NA,  TRUE))
    expect_false(is_scalar_logical(NA, FALSE))

    # Test arguments checks.
    # Argument accept_na is handled differently by is_scalar_logical().
    expect_false(is_scalar_logical(1L, 1L))
    expect_false(is_scalar_logical(1L, c("t", "f")))
})


test_that("is_scalar_integer() works",
{
    # Test normal usage.
    expect_true(is_scalar_integer(1L))
    expect_false(is_scalar_integer(NULL))
    expect_false(is_scalar_integer(1))
    expect_false(is_scalar_integer(TRUE))
    expect_false(is_scalar_integer(c(1L, 2L)))
    expect_false(is_scalar_integer(list(1L)))

    # Test if NAs are properly handled.
    expect_true(is_scalar_integer(NA_integer_,  TRUE))
    expect_false(is_scalar_integer(NA_integer_, FALSE))

    # Test arguments checks.
    expect_error(is_scalar_integer(1L, 1L),          "accept_na")
    expect_error(is_scalar_integer(1L, c("t", "f")), "accept_na")
})


test_that("is_scalar_numeric() works",
{
    expect_true(is_scalar_numeric(1L))
    expect_true(is_scalar_numeric(1.0))
    expect_false(is_scalar_numeric(NULL))
    expect_false(is_scalar_numeric(c(1, 2)))
    expect_false(is_scalar_numeric(list(1)))

    # Test normal usage.
    expect_true(is_scalar_numeric(1L))
    expect_true(is_scalar_numeric(1.0))
    expect_false(is_scalar_numeric(NULL))
    expect_false(is_scalar_numeric(c(1, 2)))
    expect_false(is_scalar_numeric(c(1L, 2L)))
    expect_false(is_scalar_numeric(list(1)))

    # Test if NAs are properly handled.
    expect_true(is_scalar_numeric(NA_integer_,  TRUE))
    expect_true(is_scalar_numeric(NA_real_,     TRUE))
    expect_false(is_scalar_numeric(NA_integer_, FALSE))
    expect_false(is_scalar_numeric(NA_real_,    FALSE))

    # Test arguments checks.
    expect_error(is_scalar_numeric(1L, 1L),          "accept_na")
    expect_error(is_scalar_numeric(1L, c("t", "f")), "accept_na")
})


test_that("is_strict_atomic() works",
{
    # The function uses the output of class(). This function returns
    # either an S3 class, an S4 class, an implicit class or typeof().
    # It always follow that order / hierarchy. We must test all these
    # cases stemming from R bad objects' design.
    # ----------------- ------------------- ------------------------------------
    # Class             Formally tested?    Constructor / Prototype
    # ----------------- ------------------- ------------------------------------
    # all S4 classes    Yes                 methods::new("mle")
    # all S3 classes    Yes                 data.frame()
    # matrix            Yes                 matrix()
    # array             Yes                 array()
    # function          Yes                 function(){}
    # if                Yes                 call("if")
    # while             Yes                 call("while")
    # for               Yes                 call("for")
    # =                 Yes                 call("=")
    # <-                Yes                 call("<-")
    # (                 Yes                 call("(")
    # {                 Yes                 call("{")
    # call              Yes                 call("call")
    # NULL              Yes                 NULL
    # logical           Yes                 logical()
    # integer           Yes                 integer()
    # double            Yes                 single(), double(), numeric()
    # complex           Yes                 complex()
    # character         Yes                 character()
    # raw               Yes                 raw()
    # list              Yes                 list()
    # closure           Yes                 function(){}
    # special           Yes                 .Internal
    # builtin           Yes                 `+`
    # environment       Yes                 environment()
    # S4                Yes                 methods::new("mle"), methods::new("externalptr")
    # symbol            Yes                 as.name("test")
    # pairlist          Yes                 pairlist(a = 1)
    # promise           No                  None
    # language          Indirectly          as.name("test"), call("call"), expression()
    # char              No                  None
    # ...               No                  None
    # any               No                  None
    # expression        Yes                 expression()
    # externalptr       Yes                 methods::new("externalptr")
    # bytecode          No                  None
    # weakref           No                  None

    # Test normal usage on strict atomic types.
    # Single values are also tested. These are double
    # values with a Csingle attribute, useful for .C().
    expect_true(is_strict_atomic(NULL))
    expect_true(is_strict_atomic(logical()))
    expect_true(is_strict_atomic(integer()))
    expect_true(is_strict_atomic(single()))
    expect_true(is_strict_atomic(numeric()))
    expect_true(is_strict_atomic(double()))
    expect_true(is_strict_atomic(complex()))
    expect_true(is_strict_atomic(character()))
    expect_true(is_strict_atomic(raw()))

    # Test normal usage on non-strict atomic vectors (atomic structures).
    vctr_with_names <- c(a = 1L, b = 2L, c = 3L)
    vctr_with_attrs <- structure(
        vctr_with_names,
        comment = "Hello, this is a test.",
        length  = 3L
    )

    expect_false(is_strict_atomic(vctr_with_names))
    expect_false(is_strict_atomic(vctr_with_attrs))

    # Test if NAs are properly handled.
    expect_true(is_strict_atomic(NA,             TRUE))
    expect_true(is_strict_atomic(NA_integer_,    TRUE))
    expect_true(is_strict_atomic(NA_real_,       TRUE))
    expect_true(is_strict_atomic(NA_complex_,    TRUE))
    expect_true(is_strict_atomic(NA_character_,  TRUE))
    expect_false(is_strict_atomic(NA,            FALSE))
    expect_false(is_strict_atomic(NA_integer_,   FALSE))
    expect_false(is_strict_atomic(NA_real_,      FALSE))
    expect_false(is_strict_atomic(NA_complex_,   FALSE))
    expect_false(is_strict_atomic(NA_character_, FALSE))

    expect_true(is_strict_atomic(c(TRUE,   NA),  TRUE))
    expect_false(is_strict_atomic(c(TRUE,  NA),  FALSE))

    # Test arguments checks.
    expect_error(is_scalar_numeric(1L, 1L),          "accept_na")
    expect_error(is_scalar_numeric(1L, c("t", "f")), "accept_na")

    # Test normal usage on various R classes.
    expect_false(is_strict_atomic())
    expect_false(is_strict_atomic(data.frame()))
    expect_false(is_strict_atomic(matrix()))
    expect_false(is_strict_atomic(array()))
    expect_false(is_strict_atomic(function(){}))
    expect_false(is_strict_atomic(call("if")))
    expect_false(is_strict_atomic(call("while")))
    expect_false(is_strict_atomic(call("for")))
    expect_false(is_strict_atomic(call("=")))
    expect_false(is_strict_atomic(call("(")))
    expect_false(is_strict_atomic(call("{")))
    expect_false(is_strict_atomic(call("call")))
    expect_false(is_strict_atomic(list()))
    expect_false(is_strict_atomic(`+`))
    expect_false(is_strict_atomic(.Internal))
    expect_false(is_strict_atomic(environment()))
    expect_false(is_strict_atomic(as.name("test")))
    expect_false(is_strict_atomic(pairlist(a = 1L)))
    expect_false(is_strict_atomic(expression()))

    # Test the following classes only if stats4 and methods can
    # be loaded. Else, skip them.
    skip_if_not_installed("stats4")
    skip_if_not_installed("methods")
        expect_false(is_strict_atomic(methods::new("mle")))
        expect_false(is_strict_atomic(methods::new("externalptr")))
})


test_that("is_named_list()",
{
    # Test normal usage.
    expect_false(is_named_list(list()))
    expect_false(is_named_list(list(1L, 2L)))
    expect_false(is_named_list(list(a = 1L, 2L)))
    expect_false(is_named_list(list(a = 1L, a = 2L)))
    expect_true(is_named_list(list(a = 1L, b = 2L)))
    expect_true(is_named_list(list(a = 1L, a = 2L), FALSE))

    # Test arguments checks.
    expect_error(is_named_list(list(), "TRUE"),         "logical")
    expect_error(is_named_list(list(), c(TRUE, FALSE)), "scalar")
})


test_that("is_named_vctr()",
{
    # Test normal usage.
    expect_false(is_named_vctr(vector()))
    expect_false(is_named_vctr(c(1L, 2L)))
    expect_false(is_named_vctr(c(a = 1L, 2L)))
    expect_false(is_named_vctr(c(a = 1L, a = 2L)))
    expect_true(is_named_vctr(c(a = 1L, b = 2L)))
    expect_true(is_named_vctr(c(a = 1L, a = 2L), FALSE))

    # Test arguments checks.
    expect_error(is_named_vctr(vector(), "TRUE"),         "logical")
    expect_error(is_named_vctr(vector(), c(TRUE, FALSE)), "scalar")
})
