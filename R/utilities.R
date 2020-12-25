#' @include assertions.R
NULL


# Unexported internal utility functions. Use minimal argument checks and
# make them as fast as possible. Favor stopifnot() for concise code, unless
# the message is intended to be passed to users.


# A safe wrapper to validate functions of R6 objects. It first checks if
# the underlying object inherits the R6 class, then checks the existence
# of a $validate() method. If so, the latter is called and a logical
# TRUE/FALSE is returned indicating whether the instance is valid or not.
valid_r6_instance <- function(x)
{
    if (!inherits(x, "R6") || !exists("validate", envir = x)) {
        stop("'x' is not a R6 object or does not possess a $validate() method.",
             call. = FALSE)
    }

    return(
        tryCatch(
            { x$validate() },
            error   = function() { return(FALSE) },
            Warning = function() { return(FALSE) }
        )
    )
}


# Standardize how Blueprint instances return errors stemming from their
# $validate() method. It is not useful to the user, so we do not export it.
validate_blueprint <- function(...)
{
    errs <- c(...)

    if (is.null(errs)) {
        return(TRUE)
    } else if (!is.character(errs)) {
        stop("error messages ('errs') should be passed as a character vector.",
             call. = FALSE)
    } else {
        stop("errors detected in object.\n",
             sprintf("  %i. %s\n", seq_along(errs), errs),
             call. = FALSE)
    }
}


# Wrapper to vapply() with a predetermined scalar logical return value.
vapply_1l <- function(x, names = TRUE, fun, ...)
{
    return(vapply(x, fun, NA, ..., USE.NAMES = names))
}


# Wrapper to vapply() with a predetermined scalar character return value.
vapply_1c <- function(x, names = TRUE, fun, ...)
{
    return(vapply(x, fun, NA_character_, ..., USE.NAMES = names))
}


# Pad a character vector with a repeated character. This outputs
# a padded character vector of strings with equal widths.
pad_string <- function(x, pad = " ")
{
    stopifnot(is.character(x), is.character(pad), !anyNA(x))
    nchars <- nchar(x)
    return(sprintf("%s%s", x, strrep(pad, max(nchars) - nchars)))
}


# Inject named arguments into an existing named atomic or recursive
# structure. Injection works by first updating .Obj with values
# stemming from matching names passed to ... and by appending the
# other name/value pairs to .Obj.
inject <- function(.Obj, ...)
{
    if (...length()) {
        dots     <- if (is.recursive(.Obj)) list(...) else c(...)
        dotnames <- names(dots)
        objnames <- names(.Obj)

        stopifnot(
            !is.null(objnames), all(nzchar(objnames)), !anyDuplicated(objnames),
            !is.null(dotnames), all(nzchar(dotnames)), !anyDuplicated(dotnames)
        )

        matches <- match(dotnames, objnames, 0L)
        updates <- objnames[matches]
        injects <- dotnames[matches == 0L]
        .Obj[updates] <- dots[updates]

        return(c(.Obj, dots[injects]))
    } else {
        return(.Obj)
    }
}


# Encode strings contained in objects to UTF-8. This is
# required when converting object to YAML and JSON formats.
# The default method is useful for recursive structures.
as_utf8           <- function(x, ...) { UseMethod("as_utf8") }
as_utf8.list      <- function(x, ...) { return(lapply(x, as_utf8)) }
as_utf8.character <- function(x, ...) { return(base::enc2utf8(x)) }
as_utf8.default   <- function(x, ...) { return(x) }


# Create a suitable list of elements to be passed to I/O functions
# such as $as_yaml() and $as_json(). The function mimics HTTP
# messages: it takes a body (a named list), some additional
# headers (another named list) and constructs a new message (a bigger
# list) from them. Other arguments type and caller are included in
# the header automatically. We enforce certain standardized headers
# that are derived from the underlying class/method that calls this
# function. Finally, the embed flag controls whether the body should
# be embedded into another list of length 1 under a name given by type.
add_headers <- function(body, headers, type, caller, embed = TRUE)
{
    stopifnot(
        is.list(body),
        !is.null(names(body)) && all(nzchar(names(body))),
        is_scalar_character(type),
        is_scalar_character(caller),
        is_scalar_logical(embed)
    )

    head <- list(
        source = sprintf("R[v%s]::blueprint[v%s]::%s$%s()",
                         as.character(getRversion()),
                         as.character(utils::packageVersion("blueprint")),
                         type, caller)
    )

    if (!missing(headers)) {
        headernames <- names(headers)

        if (!is.list(headers) ||
            is.null(headernames) ||
            any(!nzchar(headernames))) {
            stop("'headers' must be a list only containing named elements.",
                 call. = FALSE)
        }
        if (match("source", tolower(headernames), 0L)) {
            stop("'headers' cannot contain an additional header named 'source'.",
                 call. = FALSE)
        }
        if (match(type, tolower(headernames), 0L)) {
            stop(
                "'headers' cannot contain an additional header",
                sprintf(" named after a parent class (here, '%s').", type),
                call. = FALSE
            )
        }

        head <- c(head, headers)
    }

    if (embed) {
        return(c(head, structure(list(body), names = type)))
    } else {
        return(c(head, body))
    }
}
