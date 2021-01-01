test_that("add_headers() works",
{
    msg <- add_headers(
        body   = list(this = "is", a = "body"),
        type   = "test",
        caller = "test_that",
        embed  = FALSE
    )

    # Test structure.
    expect_type(msg, "list")
    expect_length(msg, 3L)
    expect_named(msg,  c("source", "this", "a"))

    # Test arguments checks.
    expect_error(add_headers(c(a = 1L),    "test", "caller"), "body")
    expect_error(add_headers(list(1L),     "test", "caller"), "body")
    expect_error(add_headers(list(a = 1L), 1L,     "caller"), "type")
    expect_error(add_headers(list(a = 1L), "test",  1L),      "caller")

    expect_error(add_headers(list(a = 1L), "test", "caller", list(1L)), "headers")
    expect_error(add_headers(list(a = 1L), "test", "caller", NULL, 1L), "embed")

    # Some names are forbidden in argument headers.
    expect_error(add_headers(list(a = 1L), "test", "caller", list(source = 1L)), "source")
    expect_error(add_headers(list(a = 1L), "test", "caller", list(test   = 1L)), "test")
})


test_that("source value in header is correct",
{
    msg <- add_headers(
        body   = list(this = "is", a = "body"),
        type   = "test",
        caller = "test_that",
        embed  = FALSE
    )

    srcheader <- msg$source

    expect_true(grepl("(R\\[v\\d.\\d.\\d\\])", srcheader))
    expect_true(grepl("(blueprint\\[v\\d.\\d.\\d(.\\d+)?\\])", srcheader))
    expect_true(grepl("test", srcheader))
    expect_true(grepl("test_that", srcheader))
})


test_that("adding headers to body works",
{
    msg <- add_headers(
        body    = list(this = "is", a = "body"),
        type    = "test",
        caller  = "test_that",
        headers = list(new = 1, headers = 2L, to = NULL, set = "yes"),
        embed   = FALSE
    )

    expect_type(msg, "list")
    expect_length(msg, 7L)
    expect_named(msg, c("source", "new", "headers", "to", "set", "this", "a"))
})


test_that("option embed works",
{
    msg <- add_headers(
        body   = list(this = "is", a = "body"),
        type   = "test",
        caller = "test_that",
        embed  = TRUE
    )

    embedmsg <- msg$test

    expect_type(msg, "list")
    expect_length(msg, 2L)
    expect_named(msg, c("source", "test"))

    expect_type(embedmsg, "list")
    expect_length(embedmsg, 2L)
    expect_named(embedmsg, c("this", "a"))
})
