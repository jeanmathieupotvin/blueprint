#' @include assertions.R
NULL


# Unexported internal utility functions.


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
        dots   <- if (is.recursive(.Obj)) list(...) else c(...)
        dotnames <- names(dots)
        objnames <- names(.Obj)

        if (is.null(objnames) ||
            any(!nzchar(objnames)) ||
            anyDuplicated(objnames)) {
            stop("all arguments passed to '.Obj' must have unique names.",
                 call. = FALSE)
        }
        if (is.null(dotnames) ||
            any(!nzchar(dotnames)) ||
            anyDuplicated(dotnames)) {
            stop("all arguments passed to '...' must have unique names.",
                 call. = FALSE)
        }

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
