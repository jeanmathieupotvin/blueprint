test_that("add_headers() works",
{
    # Create a body. Define elements used to construct header.
    body   <- list(this = "is", a = "body")
    type   <- "test"
    caller <- "test_that"

    # Create minimal messages.
    msg <- add_headers(body, type = type, caller = caller, embed = FALSE)

    # Test structure.
    expect_type(msg, "list")
    expect_length(msg, length(body) + 1L)
    expect_named(msg, c("source", names(body)))

    # Test arguments checks.
    expect_error(add_headers(c(a = 1L),    type = "t", caller = "c"))
    expect_error(add_headers(list(1L),     type = "t", caller = "c"))
    expect_error(add_headers(list(a = 1L), type = 1L,  caller = "c"))
    expect_error(add_headers(list(a = 1L), type = "t", caller = 1L))
    expect_error(add_headers(list(a = 1L), type = 1L,  caller = "c", embed = 1L))
    expect_error(
        add_headers(list(a = 1L), list(1L), "t", "c"),
        regexp = "'headers' must be a list"
    )

    # Test if errors are returned when forbidden
    # names are used in argument headers.
    expect_error(
        add_headers(list(a = 1L), list(source = 1L), "t", "c"),
        regexp = "source"
    )
    expect_error(
        add_headers(list(a = 1L), list(test = 1L), "test", "c"),
        regexp = "test"
    )
})


test_that("source value in header is correct",
{
    # Create a body. Define elements used to construct header.
    body   <- list(this = "is", a = "body")
    type   <- "test"
    caller <- "test_that"

    # Create minimal messages.
    msg <- add_headers(body, type = type, caller = caller, embed = FALSE)

    expect_true(grepl("(R\\[v\\d.\\d.\\d\\])", msg$source))
    expect_true(grepl("(blueprint\\[v\\d.\\d.\\d(.\\d+)?\\])", msg$source))
    expect_true(grepl(type, msg$source))
    expect_true(grepl(caller, msg$source))
})


test_that("option embed works",
{
    # Create a body. Define elements used to construct header.
    body   <- list(this = "is", a = "body")
    type   <- "test"
    caller <- "test_that"

    # Add standard header to body and create minimal messages.
    msg_embed <- add_headers(body, type = type, caller = caller, embed = TRUE)

    # Test if embed option/flag works.
    expect_type(msg_embed, "list")
    expect_length(msg_embed, 2L)
    expect_named(msg_embed, c("source", type))

    expect_type(msg_embed$test, "list")
    expect_length(msg_embed$test, length(body))
    expect_named(msg_embed$test, names(body))
})


test_that("adding headers to body works",
{
    # Create a body. Define elements used to construct header.
    body   <- list(this = "is", a = "body")
    type   <- "test"
    caller <- "test_that"

    # Define additional headers to be added to body.
    headers <- list(new = 1, headers = 2L, to = NULL, set = "yes")

    # Add standard header to body and create minimal messages.
    msg_headers  <- add_headers(body, headers, type, caller, embed = FALSE)

    # Test if adding additional headers works fine.
    expect_type(msg_headers, "list")
    expect_length(msg_headers, length(body) + length(headers) + 1L)
    expect_named(msg_headers, c("source", names(headers), names(body)))
})
