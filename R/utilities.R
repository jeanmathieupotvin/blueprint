#' @include assertions.R
NULL


# All functions defined in this script are unexported. Preferably, they are
# also tested, but not always.


# The following function is a wrapper validator for R6 objects. It
# first checks if underlying object inherits the R6 class, then checks
# the existence of a $validate() method. If so, the latter is called
# and a logical TRUE/FALSE is returned indicating whether the instance
# is valid or not.
#
# TODO: is this function still needed in other branches? Investigate.
# valid_r6_instance <- function(x)
# {
#     if (!inherits(x, "R6") || !exists("validate", envir = x)) {
#         stop("'x' is not a R6 object or does not possess a $validate() method.",
#              call. = FALSE)
#     }
#
#     tryCatch(
#         { x$validate() },
#         error   = function() { return(FALSE) },
#         Warning = function() { return(FALSE) }
#     )
# }


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
        stop(
            "errors detected in object.\n",
            sprintf("  %i. %s\n", seq_along(errs), errs),
            call. = FALSE
        )
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
# structure. Injection works by first updating x with values stemming
# from matching names passed to ... and by appending the other name/
# value pairs to x. Beware of automatic coercion if x is a vector!
inject <- function(.Obj, ...)
{
    if (...length()) {
        dots   <- if (is.recursive(.Obj)) list(...) else c(...)
        nmdots <- names(dots)
        nmobjs <- names(.Obj)

        if (is.null(nmobjs) || any(!nzchar(nmobjs)) || anyDuplicated(nmobjs)) {
            stop("all arguments passed to '.Obj' must have unique names.",
                 call. = FALSE)
        }
        if (is.null(nmdots) || any(!nzchar(nmdots)) || anyDuplicated(nmdots)) {
            stop("all arguments passed to '...' must have unique names.",
                 call. = FALSE)
        }

        matches <- match(nmdots, nmobjs, 0L)
        updates <- nmobjs[matches]
        injects <- nmdots[matches == 0L]
        .Obj[updates] <- dots[updates]

        return(c(.Obj, dots[injects]))
    } else {
        return(.Obj)
    }
}
