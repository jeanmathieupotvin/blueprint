test_that("valid_r6_instance() works",
{
    # Create a dummy validate() function.
    validate <- function(out = c("true", "err", "warn"))
    {
        out <- match.arg(out)

        if (out == "true") {
            return(TRUE)
        } else if (out == "err") {
            stop("this is an error.", call. = FALSE)
        } else {
            warning("this is a warning.", call. = FALSE)
            return(NULL)
        }
    }

    # Create dummy R6 object generators.
    TrueValidator <- R6::R6Class("TrueValidator",
        public = list(validate = function() { return(validate(out = "true")) })
    )
    ErrValidator <- R6::R6Class("ErrValidator",
        public = list(validate = function() { return(validate(out = "err")) })
    )
    WarnValidator <- R6::R6Class("WarnValidator",
        public = list(validate = function() { return(validate(out = "warn")) })
    )
    NoValidator <- R6::R6Class("NoValidator")

    # Test normal usage.
    expect_true(valid_r6_instance(TrueValidator$new()))
    expect_false(valid_r6_instance(ErrValidator$new()))
    expect_false(valid_r6_instance(WarnValidator$new()))

    # Test argument checks.
    expect_error(valid_r6_instance(1L),                "not a R6 object")
    expect_error(valid_r6_instance(NoValidator$new()), "\\$validate\\(\\) method")
})
