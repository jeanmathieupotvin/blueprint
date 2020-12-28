#' @include assertions.R
#' @include Blueprint.R
#' @include Atomic.R
NULL


#' @rdname Blueprinter
#'
#' @aliases Blueprinter
#'
#' @title Concise interface to blueprint
#'
#' @description
#' Create instances of \pkg{blueprint}'s classes with a fast and concise
#' operator. The output's class is automatically deduced from argument
#' `object`.
#'
#' @param symbol A syntactic [name], also known as an *unquoted name*.
#'
#' @param object Any \R value.
#'
#' @return The [`%bp%`][Blueprinter] operator is a convenient wrapper to
#' \pkg{blueprint}'s constructor functions and always returns an appropriate
#' [R6][R6::R6] object. The output is always a [Blueprint] that corresponds
#' to the underlying type of `object`.
#'
#' * If `object` is a [strict atomic value][is_strict_atomic()],
#' an [Atomic] object is returned.
#'
#' If no [Blueprint] class corresponding to `object` is available, a
#' `NULL` value is returned with a warning.
#'
#' @export
`%bp%` <- function(symbol, object)
{
    if (is_strict_atomic(object)) {
        return(Atomic$new(object, deparse(substitute(symbol)), length(object)))
    } else {
        warning("No suitable blueprint class was found for argument 'object'.",
                call. = FALSE)

        return(NULL)
    }
}
