#' @include assertions.R
#' @include utilities.R
#' @include Blueprint.R
NULL


# Class ------------------------------------------------------------------------


#' @title VectorBlueprint class
#'
#' @description
#' [VectorBlueprint] is a simple class that holds useful metadata on
#' \R atomic vectors (usually, objects that can be inserted into
#' [`data.frame`][base::data.frame()] objects). An instance of class
#' [VectorBlueprint] registers the vector's underlying class and name.
#' Behind the scenes, it also records all itssuper-classes and a
#' prototype, which is set equal to `atom[[1L]]` (see below for what
#' `atom` means).
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @author Jean-Mathieu Potvin (<jean-mathieu_potvin@@cooperators.ca>)
#'
#' @family Blueprint classes
#'
#' @export
VectorBlueprint <- R6::R6Class("VectorBlueprint",
    class      = TRUE,
    portable   = TRUE,
    lock_class = TRUE,
    cloneable  = FALSE,
    inherit    = Blueprint,
    private    = list(

        # Record all classes of the vector.
        classes   = NA_character_,

        # Record a prototype of the vector.
        prototype = NULL
    ),
    public = list(

        #' @field name A scalar character. A name for the field / vector.
        name = NA_character_,

        #' @field type A scalar character. The class of the field / vector.
        type = NA_character_,

        #' @description Create a new [VectorBlueprint] object.
        #' @param atom any atomic \R vector.
        #' See [is.atomic()][base::is.atomic()] for more information.
        #' @param name A scalar character. The name of the vector passed to `x`.
        #' @return A [R6][R6::R6] object of class [VectorBlueprint].

        # Here, it is safer to use `[` than `[[` on atom, because extraction
        # will work on vectors of length 0. Result will be NA of the proper
        # type, which is fine for $prototype.
        initialize = function(atom, name)
        {
            if (!is.atomic(atom)) {
                stop("'atom' should be an atomic vector.",
                     " Consult ?is.atomic() for more information.",
                     call. = FALSE)
            }

            private$prototype <- atom[1L]
            private$classes   <- class(atom)

            self$name <- name
            self$type <- private$classes[[1L]]

            return(self$validate())
        },

        #' @description Validate a [VectorBlueprint] object.
        #' @return The [VectorBlueprint] object invisibly if the object is
        #' valid. Else, an error explaining what is wrong with the object.
        validate = function()
        {
            is_valid_r6_instance(
                if (!is_scalar_character(self$name))  {
                    "$name must be an character of length 1."
                },
                if (!is_scalar_character(self$type)) {
                    "$type must be a character of length 1."
                }
            )

            return(invisible(self))
        },

        #' @description Print a [VectorBlueprint] object.
        #' @return The [VectorBlueprint] object invisibly.
        print = function()
        {
            self$validate()
            cat("<VectorBlueprint>\n  ", self$format(), sep = "")
            return(invisible(self))
        },

        #' @description Format a [VectorBlueprint] object.
        #' @return A character scalar representing the formatted
        #' [VectorBlueprint] object.
        format = function()
        {
            self$validate()
            return(sprintf("<name:%s type:%s>", self$name, self$type))
        },

        #' @description Coerce a [VectorBlueprint] object into a list.
        #' @return A named list of three elements:
        #' \tabular{ll}{
        #' `name`      \tab A scalar character equal to `$name`.\cr
        #' `type`      \tab A scalar character equal to `$type`.\cr
        #' `prototype` \tab A scalar [atomic][base::is.atomic()] value with a class equal to `$type`.
        #' }
        as.list = function()
        {
            self$validate()
            return(
                list(name      = self$name,
                     type      = self$type,
                     prototype = private$prototype)
            )
        },

        #' @description Coerce a [VectorBlueprint] object into a character.
        #' @return A named character of length 2 with two elements:
        #' `name` and `type` that correspond to `$name` and `$type`.
        as.character = function()
        {
            self$validate()
            return(c(name = self$name, type = self$type))
        }
    )
)


# External helpers -------------------------------------------------------------


#' @rdname VectorBlueprint
#'
#' @usage
#' ## Constructor function
#' new_vector_blueprint(atom, name)
#'
#' @param atom,name Passed to [`VectorBlueprint$new()`][VectorBlueprint]
#' (see below to learn more).
#'
#' @return Constructor function [new_vector_blueprint()] is a wrapper to
#' [`VectorBlueprint$new()`][VectorBlueprint] and returns a [R6][R6::R6]
#' object of class [VectorBlueprint].
#'
#' @export
new_vector_blueprint <- function(atom, name)
{
    return(VectorBlueprint$new(atom, name))
}


#' @rdname VectorBlueprint
#'
#' @usage
#' ## Test if an object is a 'VectorBlueprint' object
#' is_vector_blueprint(x)
#'
#' @param x any \R object.
#'
#' @return External helper functions [is_vector_blueprint()] and
#' [valid_vector_blueprint()] return a logical scalar.
#'
#' @export
is_vector_blueprint <- function(x)
{
    return(inherits(x, "VectorBlueprint", FALSE) && is_blueprint(x))
}


#' @rdname VectorBlueprint
#'
#' @usage
#' ## Validate if an object is a proper 'VectorBlueprint' object
#' valid_vector_blueprint(x)
#'
#' @export
valid_vector_blueprint <- function(x)
{
    if (!is_vector_blueprint(x)) {
        stop("'x' is not a 'VectorBlueprint' object.", call. = FALSE)
    }

    return(valid_r6_instance(x))
}


# S3 methods dispatch ----------------------------------------------------------


#' @export
#' @keywords internal
format.VectorBlueprint <- function(x, ...)
{
    return(x$format())
}


#' @export
#' @keywords internal
as.list.VectorBlueprint <- function(x, ...)
{
    return(x$as.list())
}


#' @export
#' @keywords internal
as.character.VectorBlueprint <- function(x, ...)
{
    return(x$as.character())
}
