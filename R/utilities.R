#' @include assertions.R
NULL


# A safe wrapper to validate R6 objects. It checks if it inherits the R6
# class, then checks the existence of a $validate() method. If so, the
# latter is called and a logical is returned indicating whether the instance
# is valid or not. It uses the fact that a $validate() method should return
# the underlying object if it is valid.
valid_r6_instance <- function(x)
{
    if (!inherits(x, "R6") || !exists("validate", envir = x)) {
        stop("'x' is not a R6 object or does not possess a $validate() method.",
             call. = FALSE)
    }

    tryCatch(
        { return(if (R6::is.R6(x$validate())) TRUE else FALSE) },
        error   = function(e) { return(FALSE) },
        warning = function(w) { return(FALSE) }
    )
}


# Standardize how Blueprint instances return errors stemming from their
# $validate() method. It is not useful to the user, so we do not export it.
report_errors <- function(...)
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


# Wrapper functions to vapply() with a predetermined values.
vapply_1l <- function(x, fun, names = FALSE, ...)
{
    return(vapply(x, fun, NA, ..., USE.NAMES = names))
}
vapply_1c <- function(x, fun, names = FALSE, ...)
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


# Encode strings contained in objects to UTF-8. This is
# required when converting object to YAML and JSON formats.
# The default method is useful for recursive structures.
# Important! Following R 4.0 changes, S3 methods must always
# be exported (and included in NAMESPACE), even if the underlying
# generic function is to be kept private/unexported.
# See https://github.com/r-lib/devtools/issues/2293.
# Else, R CMD CHECK and examples will fail.
as_utf8 <- function(x, ...)
{
    UseMethod("as_utf8")
}


#' @export
as_utf8.list <- function(x)
{
    return(lapply(x, as_utf8))
}


#' @export
as_utf8.character <- function(x)
{
    return(base::enc2utf8(x))
}


#' @export
as_utf8.default <- function(x)
{
    return(x)
}


# Inject named arguments into an existing named atomic or recursive
# structure. Injection works by first updating .Obj with values
# stemming from matching names passed to ... and by appending the
# other name/value pairs to .Obj. If ..1 is a list or a vector of
# length greater than 1, then only its child elements will be used
# (as if they were passed to ...).
inject <- function(.Obj, ...)
{
    if (...length()) {
        dots <- if (is.recursive(.Obj)) {

            # If the first argument is a list, we consider that
            # the elements of this list should be used as if they
            # were passed to ...
            if (is.recursive(..1)) ..1 else list(...)
        } else {

            # If the first argument is a vector, we consider that
            # the elements of this list should be used as if they
            # were passed to ...
            if (is.vector(..1) && length(..1) > 1L) ..1 else c(...)
        }

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


# Create a suitable list of elements to be passed to I/O functions
# such as $as_yaml() and $as_json(). The function mimics HTTP
# messages: it takes a body (a named list), some additional
# headers (another named list) and constructs a new message (a bigger
# list) from them. Other arguments type and caller are included in
# the header automatically. We enforce certain standardized headers
# that are derived from the underlying class/method that calls this
# function. Finally, the embed flag controls whether the body should
# be embedded into another list of length 1 under a name given by type.
add_headers <- function(body, type, caller, headers, embed = TRUE)
{
    stopifnot(
        is_named_list(body),
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
        if (!is_named_list(headers)) {
            stop("'headers' must be a list only containing named elements.",
                 call. = FALSE)
        }

        headernames <- names(headers)

        if (match("source", tolower(headernames), 0L)) {
            stop("'headers' cannot contain an additional header named 'source'.",
                 call. = FALSE)
        }
        if (match(tolower(type), tolower(headernames), 0L)) {
            stop("'headers' cannot contain an additional header",
                 sprintf(" named after a parent class (here, '%s').", type),
                 call. = FALSE)
        }

        head <- c(head, headers)
    }

    if (embed) {
        return(c(head, structure(list(body), names = type)))
    } else {
        return(c(head, body))
    }
}
