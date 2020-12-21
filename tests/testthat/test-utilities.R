testthat::test_that("valid_r6_instance() works",
{
    # Create a dummy R6 object generator with a $validate() method.
    TestWithValidator <- R6::R6Class("TestWithValidator",
        public = list(
            validate = function() { return(TRUE) }
        )
    )

    # Create a dummy R6 object generator with no $validate() method.
    TestWithoutValidator <- R6::R6Class("TestWithoutValidator")

    # Test normal usage.
    testthat::expect_true(valid_r6_instance(TestWithValidator$new()))

    # Test if an error is returned when argument is not a R6 object.
    testthat::expect_error(
        valid_r6_instance(1L),
        regexp = "not a R6 object"
    )

    # Test if an error is returned when class has no $validate() method.
    testthat::expect_error(
        valid_r6_instance(TestWithoutValidator$new()),
        regexp = "not possess a \\$validate\\(\\) method"
    )
})


testthat::test_that("validate_blueprint() works",
{
    # Test normal usage.
    testthat::expect_error(
        validate_blueprint("$port must be an integer of length 1."),
        regexp = "\\$port must be an integer of length 1."
    )
    testthat::expect_error(
        validate_blueprint(
            c("$field1 must be an integer of length 1.",
              "$field2 must be a character of length 1.")
        ),
        regexp = "\\$field1 must be an integer of length 1.",
    )
    testthat::expect_error(
        validate_blueprint(
            c("$field1 must be an integer of length 1.",
              "$field2 must be a character of length 1.")
        ),
        regexp = "\\$field2 must be a character of length 1.",
    )

    # Test if TRUE is returned when NULLs are passed to function.
    testthat::expect_true(validate_blueprint(NULL))
    testthat::expect_true(validate_blueprint(c(NULL, NULL, NULL)))

    # Test if an error is returned when values other than character
    # vectors are passed.
    testthat::expect_error(
        validate_blueprint(1L),
        regexp = "passed as a character vector"
    )
    testthat::expect_error(
        validate_blueprint(list("string")),
        regexp = "passed as a character vector"
    )
})


# We use simple R constants for convenience in the following test chunk.
testthat::test_that("vapply_1l() works",
{
    # Test normal function usage.
    testthat::expect_identical(
        object   = vapply_1l(month.name, TRUE, is.character),
        expected = sapply(month.name, is.character)
    )
    testthat::expect_identical(
        object   = vapply_1l(month.name, FALSE, is.character),
        expected = sapply(month.name, is.character, USE.NAMES = FALSE)
    )

    # Test if an error is returned when numeric values are passed.
    testthat::expect_error(vapply_1l(c(pi, pi), TRUE, `+`, e1 = 1))
})


# We use simple R constants for convenience in the following test chunk.
testthat::test_that("vapply_1c() works",
{
    # Test normal function usage.
    testthat::expect_identical(
        object   = vapply_1c(month.name, TRUE, substr, start = 1L, stop = 1L),
        expected = structure(substr(month.name, start = 1L, stop = 1L), names = month.name)
    )
    testthat::expect_identical(
        object   = vapply_1c(month.name, FALSE, substr, start = 1L, stop = 1L),
        expected = structure(substr(month.name, start = 1L, stop = 1L), names = NULL)
    )

    # Test if an error is returned when numeric values are passed.
    testthat::expect_error(vapply_1c(c(pi, pi), TRUE, `+`, e1 = 1))
})


# We use simple R constants for convenience in the following test chunk.
testthat::test_that("pad_string() works",
{
    # Test normal usage.
    testthat::expect_identical(
        object   = pad_string(c("one", "two", "three"), pad = " "),
        expected = c("one  ", "two  ", "three")
    )
    testthat::expect_identical(
        object   = nchar(pad_string(c("one", "two", "three"), pad = "  ")),
        expected = c(7L, 7L, 5L)
    )
    testthat::expect_identical(
        object   = pad_string(c("one", "two", "three"), pad = "/pad"),
        expected = c("one/pad/pad", "two/pad/pad", "three")
    )

    # Test if an error is returned when arguments are not character values.
    testthat::expect_error(pad_string(c(1L, 1L), pad = 1))
    testthat::expect_error(pad_string(c("one", "two", "three"), pad = 1))
    testthat::expect_error(pad_string(c(NA_character_, "one"), pad = " "))
})


testthat::test_that("inject() works on recursive structures",
{
    # Create simple list to be updated by inject().
    simplelist <- list(opt1 = "test", opt2 = "test")

    # Test normal usage.
    testthat::expect_identical(
        inject(simplelist,  opt2 = "succeed", opt3 = "new"),
        list(opt1 = "test", opt2 = "succeed", opt3 = "new")
    )

    # Test injection of equal values (it should work).
    testthat::expect_identical(
        inject(simplelist,  opt1 = "test", opt2 = "new"),
        list(opt1 = "test", opt2 = "new")
    )

    # Test injection failures with repeated names.
    testthat::expect_error(
        inject(simplelist, opt2 = "test1", opt2 = "test2"),
        regexp = "passed to '\\.\\.\\.'"
    )
    testthat::expect_error(
        inject(
            list(opt1 = "contains", opt1 = "errors"),
            opt1 = "test1", opt2 = "test2"
        ),
        regexp = "passed to '\\.Obj'"
    )

    # Test injection failures with unnamed values.
    testthat::expect_error(
        inject(simplelist, 1L),
        regexp = "passed to '\\.\\.\\.'"
    )
    testthat::expect_error(
        inject(list(1L, opt2 = "test"), opt1 = "test1"),
        regexp = "passed to '\\.Obj'"
    )
})


# This set of tests is directly derived from the tests performed on
# inject() for recursive structures.
testthat::test_that("inject() works on atomic structures",
{
    # Create simple vector to be updated by inject().
    simplevec <- c(opt1 = "test", opt2 = "test")

    # Test normal usage.
    testthat::expect_identical(
        inject(simplevec,  opt2 = "succeed", opt3 = "new"),
        c(opt1 = "test", opt2 = "succeed", opt3 = "new")
    )

    # Test injection of equal values (it should work).
    testthat::expect_identical(
        inject(simplevec,  opt1 = "test", opt2 = "new"),
        c(opt1 = "test", opt2 = "new")
    )

    # Test injection failures with repeated names.
    testthat::expect_error(
        inject(simplevec, opt2 = "test1", opt2 = "test2"),
        regexp = "passed to '\\.\\.\\.'"
    )
    testthat::expect_error(
        inject(
            c(opt1 = "contains", opt1 = "errors"),
            opt1 = "test1", opt2 = "test2"
        ),
        regexp = "passed to '\\.Obj'"
    )

    # Test injection failures with unnamed values.
    testthat::expect_error(
        inject(simplevec, 1L),
        regexp = "passed to '\\.\\.\\.'"
    )
    testthat::expect_error(
        inject(c(1L, opt2 = "test"), opt1 = "test1"),
        regexp = "passed to '\\.Obj'"
    )
})
