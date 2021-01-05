test_that("$print() output when length > 0L",
{
    # $print() is partially tested, because part of the string
    # is tested through $format(). We only test if the output
    # of $format() is included by looking for a keyword.

    b <- Atomic$new(sample.int(10L), "test", 10L)
    expect_snapshot_output(b$print())
})


test_that("$print() output when length is NULL",
{
    b <- Atomic$new(sample.int(10L), "test", NULL)
    expect_snapshot_output(b$print())
})


test_that("$as_yaml() output with headers; using default handlers",
{
    b <- Atomic$new(raw(), "test", 5L)

    # Test console output.
    out <- b$as_yaml(
        headers       = list("UTF-8 char" = "é"),
        source_header = TRUE
    )
    expect_snapshot_output(cat(out))

    # Test file output.
    path <- tempfile(fileext = ".yaml")
    out  <- b$as_yaml(
        file          = path,
        headers       = list("UTF-8 char" = "é"),
        source_header = TRUE
    )
    expect_identical(out, b)
    expect_snapshot_file(path, "as-yaml-w-headers-w-default-handlers.yaml")
})


test_that("$as_yaml() output without headers; using default handlers",
{
    b <- Atomic$new(raw(), "test", 5L)

    # Test console output.
    out  <- b$as_yaml(source_header = FALSE)
    expect_snapshot_output(cat(out))

    # Test file output.
    path <- tempfile(fileext = ".yaml")
    out  <- b$as_yaml(file = path, source_header = FALSE)
    expect_identical(out, b)
    expect_snapshot_file(path, "as-yaml-no-headers-w-default-handlers.yaml")
})


test_that("$as_yaml() output with custom parameters; using custom handlers",
{
    # To test custom parameters, we change indentation from 2 to 4 spaces.
    # To test custom handlers, we coerce raw values to characters instead
    # of encoding them to a base64 string.

    b <- Atomic$new(raw(), "test", 5L)

    # Test console output.
    out <- b$as_yaml(
        handlers = list(raw = function(x) { return(as.character(x)) }),
        indent   = 4L
    )
    expect_snapshot_output(cat(out))

    # Test file output.
    path <- tempfile(fileext = ".yaml")
    out  <- b$as_yaml(
        file     = path,
        handlers = list(raw = function(x) { return(as.character(x)) }),
        indent   = 4L
    )
    expect_identical(out, b)
    expect_snapshot_file(path, "as-yaml-w-headers-w-custom-params-handlers.yaml")
})


test_that("$as_json() output with headers",
{
    b <- Atomic$new(raw(), "test", 5L)

    # Test console output.
    out <- b$as_json(
        headers       = list("UTF-8 char" = "é"),
        source_header = TRUE
    )
    expect_snapshot_output(cat(out))

    # Test file output.
    path <- tempfile(fileext = ".json")
    out  <- b$as_json(
        file          = path,
        headers       = list("UTF-8 char" = "é"),
        source_header = TRUE
    )
    expect_identical(out, b)
    expect_snapshot_file(path, "as-json-w-headers.json")
})


test_that("$as_json() output without headers",
{
    b <- Atomic$new(raw(), "test", 5L)

    # Test console output.
    out  <- b$as_json(source_header = FALSE)
    expect_snapshot_output(cat(out))

    # Test file output.
    path <- tempfile(fileext = ".json")
    out  <- b$as_json(file = path, source_header = FALSE)
    expect_identical(out, b)
    expect_snapshot_file(path, "as-json-no-headers.json")
})


test_that("$as_json() output with custom parameters",
{
    # To test custom parameters, we coerce raw values to
    # integers instead of encoding them to a base64 string.

    b <- Atomic$new(raw(), "test", 5L)

    # Test console output.
    out <- b$as_json(raw = "int")
    expect_snapshot_output(cat(out))

    # Test file output.
    path <- tempfile(fileext = ".json")
    out  <- b$as_json(file = path, raw  = "int")
    expect_identical(out, b)
    expect_snapshot_file(path, "as-json-w-headers-w-custom-params.json")
})
