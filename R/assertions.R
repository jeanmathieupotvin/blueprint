#' @rdname assertions
#'
#' @title Simple assertions
#'
#' @description
#' Check if an object is of a proper type, class and length. This is
#' useful when implementing simple checks. These simple assertions are
#' used extensively in \pkg{blueprint} and are available to the users
#' for convenience.
#'
#' @param x any \R object.
#'
#' @return All functions return a scalar logical. A `TRUE` implies that the
#' underlying tested object *is* something (that *something* is dictated by
#' the function's name).
#'
#' @section Strict atomic values:
#' In package \pkg{blueprint}, a *strict atomic value* is any \R value
#' (including `NULL`) that respects the following criteria.
#'
#' 1. It has an atomic \R type:
#' [`NULL`][base::NULL],
#' [logical][base::logical()],
#' [integer][base::integer()],
#' [single][base::single()],
#' [double][base::double()] (or [numeric][base::numeric()])
#' [complex][base::complex()],
#' [character][base::character()] or
#' [raw][base::raw()].
#' 1. It is a pure vector that has only one non-degenerate dimension.
#' 2. It does not possess any structural attributes (other than type-related
#' attributes), not even [names][base::names()].
#'
#' Matrices, arrays and other recursive structures are **not** strict atomic
#' values. Vectors such as [factors][base::factor()] that are described by
#' attributes (comments, levels, etc.) are **not** strict atomic values.
#' [Single][base::single()] values (technically, [numeric][base::numeric()]
#' values with a `Csingle` attribute) are considered as strict atomic values.
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
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


#' @rdname assertions
#' @export
is_strict_atomic <- function(x)
{
    if (missing(x)) {

        # By design, we return FALSE when x is missing.
        return(FALSE)
    } else if (is.object(x) || is.recursive(x) || is.array(x)) {

        # Fast catch of almost any S4 or S3 classes,
        # recursive structures and atomic arrays.
        return(FALSE)
    } else if (is.atomic(x)) {

        # Fast catch of atomic types, including NULL.
        # TODO(JMP): catch attributes.
        return(TRUE)
    } else {

        # Safety net.
        return(FALSE)
    }
}
