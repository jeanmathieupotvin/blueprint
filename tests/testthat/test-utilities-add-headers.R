test_that("create_source_header()",
{
    src <- create_source_header("test", "test_that")

    # Test string format.
    # It should be R[vX.Y.Z]::blueprint[vX.Y.Z]::type$caller().
    expect_true(grepl("(R\\[v\\d.\\d.\\d\\])", src))
    expect_true(grepl("(blueprint\\[v\\d.\\d.\\d(.\\d+)?\\])", src))
    expect_true(grepl("test", src))
    expect_true(grepl("test_that", src))

    # Test arguments checks.
    expect_error(create_source_header(1,             "test_that"),   "type")
    expect_error(create_source_header(c("t1", "t2"), "test_that"),   "type")
    expect_error(create_source_header("test",        1L),            "caller")
    expect_error(create_source_header("test",        c("t1", "t2")), "caller")
})


test_that("add_headers()",
{
    msg <- add_headers(
        body   = list(this = "is", a = "body"),
        type   = "test",
        caller = "test_that",
        embed  = FALSE,
        source_header = TRUE
    )

    # Test structure.
    expect_type(msg, "list")
    expect_length(msg, 3L)
    expect_named(msg,  c("source", "this", "a"))

    # Test arguments checks.
    ## body
    expect_error(add_headers(c(a = 1L), "t", "c"), "body")
    expect_error(add_headers(list(1L),  "t", "c"), "body")

    ## type
    expect_error(add_headers(list(a = 1L), 1L,            "c"), "type")
    expect_error(add_headers(list(a = 1L), c("t1", "t2"), "c"), "type")

    ## caller
    expect_error(add_headers(list(a = 1L), "t", 1L),            "caller")
    expect_error(add_headers(list(a = 1L), "t", c("c1", "c2")), "caller")

    ## headers
    expect_error(add_headers(list(a = 1L), "t", "c", 1L),         "headers")
    expect_error(add_headers(list(a = 1L), "t", "c", list(1L)),   "headers")

    ## embed
    expect_error(add_headers(list(a = 1L), "t", "c", list(b = 1L), "f"),            "embed")
    expect_error(add_headers(list(a = 1L), "t", "c", list(b = 1L), c(TRUE, FALSE)), "embed")

    ## source_header
    expect_error(add_headers(list(a = 1L), "t", "c", list(b = 1L), TRUE, "f"),            "source_header")
    expect_error(add_headers(list(a = 1L), "t", "c", list(b = 1L), TRUE, c(TRUE, FALSE)), "source_header")

    # Some names are forbidden in argument headers.
    expect_error(add_headers(list(a = 1L), "t",    "c", list(source = 1L)), "source")
    expect_error(add_headers(list(a = 1L), "test", "c", list(test   = 1L)), "test")
})


test_that("adding headers to body",
{
    msg <- add_headers(
        body    = list(this = "is", a = "body"),
        type    = "test",
        caller  = "test_that",
        headers = list(new = 1, headers = 2L, to = NULL, set = "yes"),
        embed   = FALSE,
        source_header = FALSE
    )

    expect_type(msg, "list")
    expect_length(msg, 6L)
    expect_named(msg, c("new", "headers", "to", "set", "this", "a"))
})


test_that("option embed",
{
    msg <- add_headers(
        body   = list(this = "is", a = "body"),
        type   = "test",
        caller = "test_that",
        embed  = TRUE,
        source_header = FALSE
    )

    embedmsg <- msg$test

    expect_type(msg, "list")
    expect_length(msg, 1L)
    expect_named(msg, "test")

    expect_type(embedmsg, "list")
    expect_length(embedmsg, 2L)
    expect_named(embedmsg, c("this", "a"))
})


test_that("option source_header",
{
    msg_no_source <- add_headers(
        body   = list(this = "is", a = "body"),
        type   = "test",
        caller = "test_that",
        embed  = TRUE,
        source_header = FALSE
    )
    msg_with_source <- add_headers(
        body   = list(this = "is", a = "body"),
        type   = "test",
        caller = "test_that",
        embed  = TRUE,
        source_header = TRUE
    )

    expect_equal(match("source", names(msg_with_source), 0L), 1L)
    expect_equal(match("source", names(msg_no_source),   0L), 0L)
})
