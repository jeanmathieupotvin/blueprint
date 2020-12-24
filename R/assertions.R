#' @rdname assertions
#'
#' @title Simple assertions
#'
#' @description
#' Check if an object is of a proper type, class and length. This is
#' useful when implementing simple checks. These simple assertions are
#' used extensively in package \pkg{blueprint} and are all available
#' to the users for convenience.
#'
#' @param x any \R object.
#'
#' @return All functions return a scalar logical. A `TRUE` implies that the
#' underlying tested object *is* something (that *something* is dictated by
#' the function's name).
#'
#' @section Numeric versus double values:
#' [Numeric][base::numeric()] is a (totally useless) synonym of
#' [double][base::double()]. The distinction stems from (stupid) historical
#' choices that are described [here][base::double()].
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
#' [double][base::double()] (or [numeric][base::numeric()]),
#' [complex][base::complex()],
#' [character][base::character()] or
#' [raw][base::raw()].
#' 1. It is a pure vector that has only one non-degenerate dimension.
#' 2. It does not possess any structural attributes (other than type-related
#' attributes), not even [names][base::names()].
#'
#' Matrices, arrays and other recursive structures are **not** strict atomic
#' values. Vectors such as [factors][base::factor()] that are described by
#' attributes (comments, levels, names, etc.) are **not** strict atomic values.
#' [Single][base::single()] values (see section Single values below) are
#' considered to be strict atomic values.
#'
#' @section Single values:
#' There is no [single][base::single()] values in \R. A [single][base::single()]
#' is a [double][base::double()] that has a unique attribute called `"Csingle"`
#' set equal to `TRUE`. Trying to use the old \R function
#' [is.single()][base::is.single()] will simply throw an error. This is because
#' so-called single values should only be used with arguments used in external
#' interfaces (such as [.C()][base::.C()] and [.Fortran()][base::.Fortran()]).
#' Therefore, function [is_single()] checks that the argument passed to it is a
#' [double][base::double()] with an attribute `"Csingle"` set equal to `TRUE`.
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @family assertions


#' @export
is_single <- function(x)
{
    if (missing(x)) {
        return(FALSE)
    } else if (is.double(x) && isTRUE(attr(x, "Csingle", TRUE))) {

        # There is no single values in R. A single is a double that
        # has a unique attribute called "Csingle" set equal to TRUE.
        return(TRUE)
    } else {
        return(FALSE)
    }
}


#' @rdname assertions
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
    } else if (is.object(x) || is.array(x)) {

        # Fast catch of almost any S4 or S3 classes,
        # recursive structures and atomic arrays.
        return(FALSE)
    } else if (!is_single(x) && !is.null(attributes(x))) {

        # An atomic vector that has attributes
        # is not a strict atomic vector.
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
