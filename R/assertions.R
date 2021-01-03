#' @rdname assertions
#'
#' @title Simple assertions
#'
#' @description
#' Check if an object is a proper type, mode, length, etc. This is
#' useful when implementing simple checks. These are used extensively in
#' package \pkg{blueprint} and are all available to the users for convenience.
#'
#' @param x Any \R object.
#'
#' @param accept_na A scalar logical. Should `NA` values be considered as valid
#' scalar and/or atomic values?
#'
#' @param unique_names A scalar logical. Should names be unique?
#'
#' @return All functions return a scalar logical. A `TRUE` implies that the
#' underlying tested object *is* something (that *something* is given by
#' the function's name).
#'
#' @section Numeric versus double values:
#' [Numeric][base::numeric()] is either an [integer][base::integer()] or a
#' [double][base::double()]. Is it **NOT** an atomic type, it is a
#' [mode][base::mode()]. In \pkg{blueprint}, modes are purposely avoided. The
#' focus is always on [types][base::typeof()] for consistency.
#'
#' @section Strict atomic values:
#' In package \pkg{blueprint}, a *strict atomic value* is any \R value
#' that respects the following criteria.
#'
#' 1. It is an atomic \R type:
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
#'
#' @examples
#' # You control whether NAs are valid.
#' is_scalar_character(NA_character_, accept_na = TRUE)  # TRUE
#' is_scalar_character(NA_character_, accept_na = FALSE) # FALSE


#' @export
is_single <- function(x)
{
    if (is.double(x) && isTRUE(attr(x, "Csingle", TRUE))) {

        # There is no single values in R. A single is a double that
        # has a unique attribute called "Csingle" set equal to TRUE.
        return(TRUE)
    } else {
        return(FALSE)
    }
}


#' @rdname assertions
#' @export
is_scalar_character <- function(x, accept_na = TRUE)
{
    stopifnot(is_scalar_logical(accept_na))
    if (accept_na) {
        return(is.character(x) && length(x) == 1L)
    } else {
        return(is.character(x) && length(x) == 1L && !is.na(x))
    }
}


#' @rdname assertions
#' @export
is_scalar_logical <- function(x, accept_na = TRUE)
{
    # We cannot check accept_na with is_scalar_logical() here.
    # This creates an infinite stack. Instead, we use isTRUE().
    if (isTRUE(accept_na)) {
        return(is.logical(x) && length(x) == 1L)
    } else {
        return(is.logical(x) && length(x) == 1L && !is.na(x))
    }
}


#' @rdname assertions
#' @export
is_scalar_integer <- function(x, accept_na = TRUE)
{
    stopifnot(is_scalar_logical(accept_na))
    if (accept_na) {
        return(is.integer(x) && length(x) == 1L)
    } else {
        return(is.integer(x) && length(x) == 1L && !is.na(x))
    }
}


#' @rdname assertions
#' @export
is_scalar_numeric <- function(x, accept_na = TRUE)
{
    stopifnot(is_scalar_logical(accept_na))
    if (accept_na) {
        return(is.numeric(x) && length(x) == 1L)
    } else {
        return(is.numeric(x) && length(x) == 1L && !is.na(x))
    }
}


#' @rdname assertions
#' @export
is_strict_atomic <- function(x, accept_na = TRUE)
{
    stopifnot(is_scalar_logical(accept_na))

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
        if (accept_na) {
            return(TRUE)
        } else {
            return(if (anyNA(x)) FALSE else TRUE)
        }
    } else {

        # Safety net.
        return(FALSE)
    }
}


#' @rdname assertions
#' @export
is_named_list <- function(x, unique_names = TRUE)
{
    stopifnot(is_scalar_logical(unique_names))
    xnames <- names(x)

    # If FALSE, we don't need to check names.
    # This is equivalent to setting are_unique to TRUE.
    are_unique <- if (unique_names) !anyDuplicated(xnames) else TRUE

    return(is.list(x) && !is.null(xnames) && all(nzchar(xnames)) && are_unique)
}


#' @rdname assertions
#' @export
is_named_vctr <- function(x, unique_names = TRUE)
{
    stopifnot(is_scalar_logical(unique_names))
    xnames <- names(x)

    # If FALSE, we don't need to check names.
    # This is equivalent to setting are_unique to TRUE.
    are_unique <- if (unique_names) !anyDuplicated(xnames) else TRUE

    return(is.atomic(x) && !is.null(xnames) && all(nzchar(xnames)) && are_unique)
}
