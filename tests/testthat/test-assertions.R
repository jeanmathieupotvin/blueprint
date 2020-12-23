testthat::test_that("is_scalar_character() works",
{
    testthat::expect_false(is_scalar_character())
    testthat::expect_false(is_scalar_character(NULL))
    testthat::expect_false(is_scalar_character(1L))
    testthat::expect_false(is_scalar_character(c("vector", "string")))
    testthat::expect_false(is_scalar_character(list("string")))
    testthat::expect_true(is_scalar_character("string"))
})


testthat::test_that("is_scalar_logical() works",
{
    testthat::expect_false(is_scalar_logical())
    testthat::expect_false(is_scalar_logical(NULL))
    testthat::expect_false(is_scalar_logical(1L))
    testthat::expect_false(is_scalar_logical(c(TRUE, FALSE)))
    testthat::expect_false(is_scalar_logical(list(TRUE)))
    testthat::expect_true(is_scalar_logical(TRUE))
})


testthat::test_that("is_scalar_integer() works",
{
    testthat::expect_false(is_scalar_integer())
    testthat::expect_false(is_scalar_integer(NULL))
    testthat::expect_false(is_scalar_integer(1))
    testthat::expect_false(is_scalar_integer(TRUE))
    testthat::expect_false(is_scalar_integer(c(1L, 2L)))
    testthat::expect_false(is_scalar_integer(list(1L)))
    testthat::expect_true(is_scalar_integer(1L))
})


testthat::test_that("is_scalar_numeric() works",
{
    testthat::expect_false(is_scalar_numeric())
    testthat::expect_false(is_scalar_numeric(NULL))
    testthat::expect_false(is_scalar_numeric(c(1, 2)))
    testthat::expect_false(is_scalar_numeric(list(1)))
    testthat::expect_true(is_scalar_numeric(1L))
    testthat::expect_true(is_scalar_numeric(1))
})


testthat::test_that("is_strict_atomic() works",
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

    # Test if required packages are installed first. Else, throw an error.
    if (!requireNamespace("methods", quietly = TRUE) ||
        !requireNamespace("stats4", quietly = TRUE)) {
        stop("packages 'methods' and 'stats4' are required to ",
             "run tests of the Assertions context.",
             call. = FALSE)
    }

    # Test normal usage on missing value.
    # By design, we return FALSE.
    testthat::expect_false(is_strict_atomic())

    # Test normal usage on strict atomic types.
    # Single values are also tested. These are double
    # values with a Csingle attribute, useful for .C().
    testthat::expect_true(is_strict_atomic(NULL))
    testthat::expect_true(is_strict_atomic(logical()))
    testthat::expect_true(is_strict_atomic(integer()))
    testthat::expect_true(is_strict_atomic(single()))
    testthat::expect_true(is_strict_atomic(numeric()))
    testthat::expect_true(is_strict_atomic(double()))
    testthat::expect_true(is_strict_atomic(complex()))
    testthat::expect_true(is_strict_atomic(character()))
    testthat::expect_true(is_strict_atomic(raw()))

    # Test normal usage on various R classes.
    # By design, we return FALSE.
    testthat::expect_false(is_strict_atomic(methods::new("mle")))
    testthat::expect_false(is_strict_atomic(data.frame()))
    testthat::expect_false(is_strict_atomic(matrix()))
    testthat::expect_false(is_strict_atomic(array()))
    testthat::expect_false(is_strict_atomic(function(){}))
    testthat::expect_false(is_strict_atomic(call("if")))
    testthat::expect_false(is_strict_atomic(call("while")))
    testthat::expect_false(is_strict_atomic(call("for")))
    testthat::expect_false(is_strict_atomic(call("=")))
    testthat::expect_false(is_strict_atomic(call("(")))
    testthat::expect_false(is_strict_atomic(call("{")))
    testthat::expect_false(is_strict_atomic(call("call")))
    testthat::expect_false(is_strict_atomic(list()))
    testthat::expect_false(is_strict_atomic(`+`))
    testthat::expect_false(is_strict_atomic(.Internal))
    testthat::expect_false(is_strict_atomic(environment()))
    testthat::expect_false(is_strict_atomic(as.name("test")))
    testthat::expect_false(is_strict_atomic(pairlist(a = 1L)))
    testthat::expect_false(is_strict_atomic(expression()))
    testthat::expect_false(is_strict_atomic(methods::new("externalptr")))
})
