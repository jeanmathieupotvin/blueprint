#' @rdname assertions
#'
#' @title Simple assertions
#'
#' @description
#' Check if an object is of a proper storage type, class and length. This
#' is useful when implementing simple checks. These simple assertions are
#' used extensively in \pkg{blueprint} and are available to the users for
#' convenience.
#'
#' @param x any \R object.
#'
#' @return All functions return a scalar logical. A `TRUE` implies that the
#' underlying tested object *is* something (that *something* is dictated by
#' the function's name).
#'
#' @author Jean-Mathieu Potvin (<jean-mathieu_potvin@@cooperators.ca>)
#'
#' @family assertions


#' @export
is_scalar_character <- function(x)
{
    return(if (missing(x)) FALSE else is.character(x) && length(x) == 1L)
}

#' @rdname assertions
#' @export
is_scalar_logical <- function(x)
{
    return(if (missing(x)) FALSE else is.logical(x) && length(x) == 1L)
}


#' @rdname assertions
#' @export
is_scalar_integer <- function(x)
{
    return(if (missing(x)) FALSE else is.integer(x) && length(x) == 1L)
}


#' @rdname assertions
#' @export
is_scalar_numeric <- function(x)
{
    return(if (missing(x)) FALSE else is.numeric(x) && length(x) == 1L)
}


# The following function standardizes how R6 instances return
# errors stemming from their $validate() method. It is not useful
# to the user, so we do not export it.

is_valid_r6_instance <- function(...)
{
    errs <- c(...)

    if (is.null(errs)) {
        return(TRUE)
    } else if (!is.character(errs)) {
        stop("error messages ('errs') should be passed as a character vector.",
             call. = FALSE)
    } else {
        stop(
            "errors detected in object:\n",
            sprintf("  %i. %s\n", seq_along(errs), errs),
            call. = FALSE
        )
    }
}
