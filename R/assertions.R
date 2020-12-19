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
