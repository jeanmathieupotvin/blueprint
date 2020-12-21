#' @include Blueprint.R
#' @include Atomic.R
NULL


#' @rdname Blueprinter
#'
#' @title Concise interface to blueprint
#'
#' @description
#' Create instances of \pkg{blueprint}'s classes faster and more easily.
#'
#' @param symbol A syntactic [name], also known as an *unquoted name*.
#'
#' @param value Any \R value.
#'
#' @return The [`%bp%`][Blueprinter] operator is a convenient wrapper to
#' \pkg{blueprint}'s constructors and returns an appropriate [R6][R6::R6]
#' object, a [Blueprint] that corresponds to the underlying type of `value`.
#'
#' * If value is of a *strict atomic* type (one of [NULL][base::NULL],
#' [logical][base::logical()], [integer][base::integer()],
#' [numeric][base::numeric()], [complex][base::complex()],
#' [character][base::character()] or [raw][base::raw()]), an
#' instance of class [Atomic] is returned.
#'
#' @export
`%bp%` <- function(symbol, object)
{
    if (is.atomic(object)) {
        return(Atomic$new(object, deparse(substitute(symbol)), length(object)))
    } else {
        return(NULL)
    }
}
