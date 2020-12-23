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
#' Create instances of \pkg{blueprint}'s classes faster and more easily.
#'
#' @param symbol A syntactic [name], also known as an *unquoted name*.
#'
#' @param object Any \R value.
#'
#' @return The [`%bp%`][Blueprinter] operator is a convenient wrapper to
#' \pkg{blueprint}'s constructors and returns an appropriate [R6][R6::R6]
#' object, a [Blueprint] that corresponds to the underlying type of `object`.
#'
#' * If `object` is a [strict atomic value][is_strict_atomic()],
#' an [Atomic] object is returned.
#'
#' @export
`%bp%` <- function(symbol, object)
{
    if (is_strict_atomic(object)) {
        return(Atomic$new(object, deparse(substitute(symbol)), length(object)))
    } else {
        return(NULL)
    }
}
