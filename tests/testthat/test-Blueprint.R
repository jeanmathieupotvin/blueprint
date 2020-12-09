testthat::test_that("method $new() works", {
    # Verify general structure.
    testthat::expect_type(b, "environment")
    testthat::expect_type(b$.__enclos_env__$self, "environment")
    testthat::expect_type(b$.__enclos_env__$private, "NULL")

    # Verify classes.
    testthat::expect_s3_class(b, "Blueprint")
    testthat::expect_s3_class(b, "R6")

    # Verify public fields.
    testthat::expect_true(b$is_blueprint)
    testthat::expect_identical(
        object   = b$blueprint_version,
        expected = as.character(utils::packageVersion("blueprint"))
    )
})


testthat::test_that("method $validate() works", {
    testthat::expect_identical(b$validate(), b)
    testthat::expect_error(b_wrong_vec$validate(), regexp = "errors detected")
    testthat::expect_error(b_wrong_scalar$validate(), regexp = "errors detected")
})


testthat::test_that("method $print() works", {
    testthat::expect_identical(quiet(b$print()), b)
    testthat::expect_output(b$print())
})


testthat::test_that("method $format() works", {
    testthat::expect_identical(b$format(), "<Blueprint>")
})


# We only test if wrapper effectively returns a Blueprint object.
# Its actual structure is tested in the context of $new().
testthat::test_that("constructor new_blueprint() function works", {
    testthat::expect_s3_class(new_blueprint(), "Blueprint")
})


testthat::test_that("introspector is_blueprint() function works", {
    testthat::expect_true(is_blueprint(b))
    testthat::expect_false(is_blueprint(b_wrong_scalar))
    testthat::expect_false(is_blueprint(b_wrong_vec))
})


testthat::test_that("test S3 internal methods dispatch", {
    testthat::expect_identical(format(b), "<Blueprint>")
})
