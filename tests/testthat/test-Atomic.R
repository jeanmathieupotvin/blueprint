test_that("instance has an appropriate structure",
{
    b <- Atomic$new(sample.int(10L), "test", 10L)

    # Test general structure.
    expect_type(b, "environment")

    # Test class inheritance.
    expect_s3_class(b, "Atomic")
    expect_s3_class(b, "Blueprint")
    expect_s3_class(b, "R6")

    # Test public fields.
    expect_identical(b$type, "integer")
    expect_identical(b$name, "test")
    expect_identical(b$length, 10L)
})


test_that("prototypes are correctly registered at initialization",
{
    # Test prototypes when (atomic) length == 0.
    expect_identical(proto(Atomic$new(NULL,        "test", NULL)), NULL)
    expect_identical(proto(Atomic$new(logical(),   "test", NULL)), NA)
    expect_identical(proto(Atomic$new(integer(),   "test", NULL)), NA_integer_)
    expect_identical(proto(Atomic$new(single(),    "test", NULL)), NA_single_)
    expect_identical(proto(Atomic$new(double(),    "test", NULL)), NA_real_)
    expect_identical(proto(Atomic$new(complex(),   "test", NULL)), NA_complex_)
    expect_identical(proto(Atomic$new(character(), "test", NULL)), NA_character_)
    expect_identical(proto(Atomic$new(raw(),       "test", NULL)), NA_raw_)

    # Test prototypes when (atomic) length > 0.
    # We pass random values to see if
    # prototypes are properly registered.
    expect_identical(proto(Atomic$new(NULL,         "test", 1L)), NULL)
    expect_identical(proto(Atomic$new(TRUE,         "test", 1L)), TRUE)
    expect_identical(proto(Atomic$new(9L,           "test", 1L)), 9L)
    expect_identical(proto(Atomic$new(as.single(9), "test", 1L)), as.single(9))
    expect_identical(proto(Atomic$new(9.5,          "test", 1L)), 9.5)
    expect_identical(proto(Atomic$new(1+3i,         "test", 1L)), 1+3i)
    expect_identical(proto(Atomic$new("chr",        "test", 1L)), "chr")
    expect_identical(proto(Atomic$new(as.raw("9"),  "test", 1L)), as.raw("9"))
})


test_that("$new()",
{
    # Test normal usage.
    expect_s3_class(Atomic$new(NULL,        "test"), "Atomic")
    expect_s3_class(Atomic$new(logical(),   "test"), "Atomic")
    expect_s3_class(Atomic$new(single(),    "test"), "Atomic")
    expect_s3_class(Atomic$new(double(),    "test"), "Atomic")
    expect_s3_class(Atomic$new(numeric(),   "test"), "Atomic")
    expect_s3_class(Atomic$new(complex(),   "test"), "Atomic")
    expect_s3_class(Atomic$new(character(), "test"), "Atomic")
    expect_s3_class(Atomic$new(raw(),       "test"), "Atomic")

    # Only strict atomic vectors can be passed to $new().
    expect_error(Atomic$new(list(),   "test"), "strict atomic")
    expect_error(Atomic$new(array(),  "test"), "strict atomic")
    expect_error(Atomic$new(matrix(), "test"), "strict atomic")

    # Names can only be scalar characters.
    expect_error(Atomic$new(raw(), 1L),                "scalar character")
    expect_error(Atomic$new(raw(), c("test", "test")), "scalar character")

    # Lengths can be NULL or scalar positive integers.
    expect_s3_class(Atomic$new(raw(), "test", 1),       "Atomic")
    expect_s3_class(Atomic$new(raw(), "test", 0L),      "Atomic")
    expect_s3_class(Atomic$new(raw(), "test", NULL),    "Atomic")
    expect_s3_class(Atomic$new(raw(), "test", c(1, 2)), "Atomic")

    expect_error(Atomic$new(raw(), "test", "1L"),    "positive integer scalar")
    expect_error(Atomic$new(raw(), "test", -1L),     "positive integer scalar")
})


test_that("$validate()",
{
    # Test if output is returned invisibly.
    b <- Atomic$new(sample.int(10L), "test", 10L)
    expect_invisible(b$validate())
    expect_identical(b$validate(), b)

    # Test validation of $type.
    # To do so, the Blueprint$set() method must be used,
    # else we generate errors stemming from Atomic$set().
    # We access this function by using helper function
    # superset(). We proceed like this to avoid using
    # multiple calls to `<-`, which would be cumbersome.
    b <- Atomic$new(sample.int(10L), "test", 10L)
    expect_error(superset(b, "type", 1,             FALSE), "scalar character")
    expect_error(superset(b, "type", c("t1", "t2"), FALSE), "scalar character")
    expect_error(superset(b, "type", "list",        FALSE), "strict atomic")
    expect_error(superset(b, "type", "numeric",     FALSE), "strict atomic")

    b <- Atomic$new(sample.int(10L), "test", 10L)
    expect_s3_class(superset(b, "type", "NULL"),      "Atomic")
    expect_s3_class(superset(b, "type", "logical"),   "Atomic")
    expect_s3_class(superset(b, "type", "single"),    "Atomic")
    expect_s3_class(superset(b, "type", "double"),    "Atomic")
    expect_s3_class(superset(b, "type", "complex"),   "Atomic")
    expect_s3_class(superset(b, "type", "character"), "Atomic")
    expect_s3_class(superset(b, "type", "raw"),       "Atomic")

    # Test validation of $name.
    b <- Atomic$new(sample.int(10L), "test", 10L)
    expect_error(b$set("name", 1),             "scalar character")
    expect_error(b$set("name", c("n1", "n2")), "scalar character")

    # Test validation of $length.
    b <- Atomic$new(sample.int(10L), "test")
    expect_error(b$set("length", -1L), "positive scalar integer")
    expect_error(b$set("length", 1.2), "positive scalar integer")
    expect_error(b$set("length", "1"), "positive scalar integer")

    b <- Atomic$new(sample.int(10L), "test")
    expect_s3_class(b$set("length", 0L),   "Atomic")
    expect_s3_class(b$set("length", NULL), "Atomic")
})


test_that("$format()",
{
    # Test normal usage when length is not NULL (>= 0).
    b <- Atomic$new(raw(10L), "test", 10L)
    expect_identical(b$format(), "<name:test type:raw length:10>")

    # Test normal usage when length is NULL.
    b <- Atomic$new(raw(10L), "test", NULL)
    expect_identical(b$format(), "<name:test type:raw>")

    # Test deactivation of $validate().
    # We inject an error into object to do so.
    b$type <- "test"
    expect_identical(b$format(.validate = FALSE), "<name:test type:test>")
})


test_that("$compare()",
{
    # Test normal usage when length is not NULL (>= 0).
    b <- Atomic$new(raw(10L), "test", 10L)
    expect_true(b$compare(raw(10L)))
    expect_false(b$compare(raw(1L)))
    expect_false(b$compare(double(1L)))
    expect_false(b$compare(character(10L)))

    # Test normal usage when length is NULL.
    b <- Atomic$new(raw(10L), "test", NULL)
    expect_true(b$compare(raw(10L)))
    expect_true(b$compare(raw(1L)))
    expect_false(b$compare(double(1L)))

    # Test specific single case.
    b <- Atomic$new(single(10L), "test", 10L)
    expect_true(b$compare(single(10L)))
    expect_false(b$compare(single(1L)))
    expect_false(b$compare(double(1L)))
    expect_false(b$compare(character(10L)))

    # Test deactivation of $validate().
    # We inject an error into object to do so.
    b$type <- "test"
    expect_false(b$compare(single(10L), .validate = FALSE))
})


test_that("$generate()",
{
    # We don't check all prototypes, they
    # are tested in another test chunk.

    # Test normal usage when length is not NULL (>= 0).
    expect_identical(Atomic$new(NULL,      "test", 2L)$generate(), NULL)
    expect_identical(Atomic$new(logical(), "test", 2L)$generate(), c(NA, NA))

    # Test normal usage when length is NULL.
    expect_identical(Atomic$new(NULL,      "test", NULL)$generate(), NULL)
    expect_identical(Atomic$new(logical(), "test", NULL)$generate(), NA)

    # Test normal usage when type is single.
    expect_identical(
        Atomic$new(single(), "single", 10L)$generate(),
        as.single(rep.int(NA_real_, 10L))
    )

    # Test deactivation of $validate().
    # We inject an error into object to do so.
    b <- Atomic$new(logical(), "test")
    b$type <- "test"
    expect_identical(b$generate(.validate = FALSE), NA)
})


test_that("$set()",
{
    b <- Atomic$new(double(), "test")

    # Test normal usage.
    expect_identical(b$set("length", 1L)$get("length"), 1L)
    expect_identical(b$set("name", "new-name")$get("name"), "new-name")

    # Test that $type cannot be changed.
    expect_error(b$set("type", "character"), "generate a new")

    # Test arguments checks.
    expect_error(b$set(1L),          "scalar character")
    expect_error(b$set(c("1", "2")), "scalar character")
})


test_that("$as_list()",
{
    b   <- Atomic$new(c(10L, 8L, 9L, 5L, 1L), "test", 5L)
    ref <- list(
        name      = "test",
        type      = "integer",
        length    = 5L,
        prototype = 10L
    )
    expect_identical(b$as_list(), ref)

    # Test deactivation of $validate().
    # We inject an error into object to do so.
    b$type   <- "test"
    ref$type <- "test"
    expect_identical(b$as_list(.validate = FALSE), ref)
})


test_that("$as_character()",
{
     # Test normal usage when length is not NULL (>= 0).
    b   <- Atomic$new(c(10L, 8L, 9L, 5L, 1L), "test", 5L)
    ref <- c(name = "test", type = "integer", length = "5")
    expect_identical(b$as_character(), ref)

    # Test normal usage when length is NULL.
    b   <- Atomic$new(c(10L, 8L, 9L, 5L, 1L), "test")
    ref <- c(name = "test", type = "integer", length = "NULL")
    expect_identical(b$as_character(), ref)

    # Test deactivation of $validate().
    # We inject an error into object to do so.
    b$type <- "test"
    ref[["type"]] <- "test"
    expect_identical(b$as_character(.validate = FALSE), ref)
})


test_that("$as_yaml()",
{
    b <- Atomic$new(raw(), "test", 5L)

    # Test return value when file is missing.
    out <- b$as_yaml(headers = list(`UTF-8 char` = "é"))
    expect_type(out, "character")
    expect_identical(Encoding(out), "UTF-8")

    # Test return value when file is not missing.
    expect_identical(b$as_yaml(tempfile()), b)

    # Test arguments checks.
    # Argument headers is validated by add_headers().
    expect_error(b$as_yaml(file = 1),             "scalar character")
    expect_error(b$as_yaml(file = c("f1", "f2")), "scalar character")
    expect_error(b$as_yaml(file = ""),            "non-empty")

    expect_error(b$as_yaml(source_header = 1L),               "scalar logical")
    expect_error(b$as_yaml(source_header = c(TRUE, FALSE)),   "scalar logical")

    # Test deactivation of $validate().
    # We inject an error into object to do so.
    b$type <- "test"
    expect_true(grepl("(\n  type: test)", b$as_yaml(.validate = FALSE)))
})


test_that("$as_json()",
{
    b <- Atomic$new(raw(), "test", 5L)

    # Test return value when file is missing.
    out <- b$as_json(headers = list(`UTF-8 char` = "é"))
    expect_s3_class(out, "json")
    expect_identical(Encoding(out), "UTF-8")

    # Test return value when file is not missing.
    expect_identical(b$as_json(tempfile()), b)

    # Test arguments checks.
    # Argument headers is validated by add_headers().
    expect_error(b$as_json(file = 1),             "scalar character")
    expect_error(b$as_json(file = c("f1", "f2")), "scalar character")
    expect_error(b$as_json(file = ""),            "non-empty")

    expect_error(b$as_json(source_header = 1L),               "scalar logical")
    expect_error(b$as_json(source_header = c(TRUE, FALSE)),   "scalar logical")

    # Test deactivation of $validate().
    # We inject an error into object to do so.
    b$type <- "test"
    expect_true(grepl("(\"type\": \"test\")", b$as_json(.validate = FALSE)))
})


test_that("is_atomic()",
{
    expect_true(is_atomic(Atomic$new(raw(), "test")))
    expect_false(is_atomic(1L))
})


test_that("valid_atomic()",
{
    expect_true(valid_atomic(Atomic$new(raw(), "test")))
    expect_error(valid_atomic(1L), "not an 'Atomic'")
})


test_that("as.list.Atomic()",
{
    b   <- Atomic$new(c(10L, 8L, 9L, 5L, 1L), "test", 5L)
    ref <- list(
        name      = "test",
        type      = "integer",
        length    = 5L,
        prototype = 10L
    )
    expect_identical(as.list(b), ref)
})


test_that("as.character.Atomic()",
{
    b   <- Atomic$new(c(10L, 8L, 9L, 5L, 1L), "test", 5L)
    ref <- c(name = "test", type = "integer", length = "5")
    expect_identical(as.character(b), ref)
})
