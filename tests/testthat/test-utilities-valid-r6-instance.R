test_that("valid_r6_instance() works",
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
    expect_true(valid_r6_instance(TestWithValidator$new()))

    # Test if an error is returned when argument is not a R6 object.
    expect_error(valid_r6_instance(1L), regexp = "not a R6 object")

    # Test if an error is returned when class has no $validate() method.
    expect_error(
        valid_r6_instance(TestWithoutValidator$new()),
        regexp = "not possess a \\$validate\\(\\) method"
    )
})
