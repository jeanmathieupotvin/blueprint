#' @include assertions.R
NULL


# Default jsonlite options used by method Atomic$as_json().
opts_json_atomic <- function(...)
{
    defaults <- list(
        auto_unbox = TRUE,
        pretty     = TRUE,
        force      = FALSE,
        complex    = "string",
        raw        = "base64",
        null       = "null",
        na         = "string"
    )

    if (...length()) {

        # This message is conveyed to the user.
        if (!is_named_list(customs <- list(...))) {
            stop("all arguments passed to '...' must be a named. Names must be unique.",
                 call. = FALSE)
        }

        return(inject(defaults, customs))
    } else {
        return(defaults)
    }
}


# Default YAML handler functions. We encapsulate these into
# a list itself encapsulated into a function. Custom handlers
# can be passed to this function. They will be injected into
# our default list.
opts_yaml_handlers <- function(handlers = list())
{
    defaults <- list(raw = function(x) { return(jsonlite::base64_enc(x)) })

    if (length(handlers)) {

        # This message is conveyed to the user.
        if (!is_named_list(handlers) ||
            !all(vapply_1l(handlers, is.function))) {
            stop("'handlers' must be a named list of functions. Names must be unique.",
                 call. = FALSE)
        }

        return(inject(defaults, handlers))
    } else {
        return(defaults)
    }
}
