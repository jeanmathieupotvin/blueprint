#' @include assertions.R
#' @include utilities.R
#' @include Blueprint.R
#' @include VectorBlueprint.R
NULL


# Class ------------------------------------------------------------------------


#' @title VectorBlueprints class
#'
#' @description
#' [VectorBlueprints] is a class that holds many different [VectorBlueprint]
#' objects together. In other words, it is a *collection*. It incorporates
#' some internal features that treat this collection more efficiently than
#' just *looping over* its [VectorBlueprint] elements.
#'
#' You can construct a [VectorBlueprints] collection directly from *list-like*
#' objects ([data.frame][base::data.frame()], [data.table][data.table::data.table()],
#' [list][base::list()], etc.). You can also construct an empty
#' [VectorBlueprints] instance.
#'
#' The resulting [VectorBlueprints] collection must contain *unique*
#' [VectorBlueprint] objects (they must all have a unique `$name` value). In
#' other words, two [VectorBlueprint] instances in a [VectorBlueprints] object
#' cannot have the same `$name`.
#'
#' @param ... An arbitrary number of arguments. The behavior of the
#' method changes according to the class of the **first element** passed
#' to `...`
#'
#' * If the first element is `NULL` or if nothing is passed to `...`,
#' an empty [VectorBlueprints] object is returned.
#'
#' * If the first element is a [data.frame][base::data.frame()],
#' a [data.table][data.table::data.table()] or a [list][base::list()],
#' then the underlying columns / elements of this object are coerced
#' to [VectorBlueprint] objects that are later used to create a
#' [VectorBlueprints] collection. All further arguments passed to `...`
#' are ignored.
#'
#' * If the first element is a [VectorBlueprint] object, then an arbitrary
#' number of [VectorBlueprint] objects can be passed to `...` They will all
#' be used to create a [VectorBlueprints] collection.
#'
#' Any other type of objects supplied as the first element will generate
#' an error.
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
VectorBlueprints <- R6::R6Class("VectorBlueprints",
    class      = TRUE,
    portable   = TRUE,
    lock_class = TRUE,
    cloneable  = FALSE,
    inherit    = Blueprint,
    private    = list(

        # Detect the appropriate method (NULL/VectorBlueprint/list-like
        # objects) that should be used to initialize a VectorBlueprints
        # instance based on the first argument passed to $initialize().
        # All objects with a 'list' type / storage mode are bunched
        # because the strategy is to loop over their child elements with
        # a mapply(). This works as long as their storage type is 'list'.
        detect_method = function(x)
        {
            if (missing(x) || is.null(x)) {
                return("NULL")
            } else if (is_field(x)) {
                return("VectorBlueprint")
            } else if (typeof(x) == "list") {
                return("list")
            } else {
                stop("this method does not work when the first element passed",
                     " to '...' is of class ", class(x), ".\n",
                     "Pass a NULL value, a 'VectorBlueprint' object or any other objet",
                     " with a 'list' storage type.\n",
                     "Consult documentation for more information.",
                     call. = FALSE)
            }
        }
    ),
    active = list(

        #' @field count A scalar integer giving the length of the collection
        #' stored in `$blueprints`.
        count = function() { return(length(self$blueprints)) }
    ),
    public = list(

        #' @field blueprints A named list strictly containing
        #' [VectorBlueprint] objects.
        blueprints = NULL,

        #' @description Create a new [VectorBlueprints] object.
        #' @param .method A scalar character equal to `"NULL"`, `"list"` or
        #' `"VectorBlueprint"` and used internally by \pkg{blueprint} to
        #' obtain a faster initialization process when [VectorBlueprints] is a
        #' super-class.
        #' **This is an internal parameter that should not be used
        #' by regular users. Using it can lead to undefined behavior.**
        #' @return A [R6][R6::R6] object of class [VectorBlueprints].
        initialize = function(..., .method)
        {
            method <- if (missing(.method)) {
                private$detect_method(..1)
            } else {
                match.arg(.method, c("NULL", "list", "VectorBlueprint"))
            }

            if (method == "NULL") {

                self$blueprints <- list()

            } else if (method == "list") {

                self$blueprints <- structure(
                    mapply(VectorBlueprint$new, ..1, names(..1),
                        SIMPLIFY  = FALSE,
                        USE.NAMES = FALSE
                    ),
                    names = names(..1)
                )

            } else {

                blueprints <- list(...)

                if (!all(vapply_1l(blueprints, FALSE, is_field))) {
                    stop("when the first element passed to $new() is of class",
                         " 'VectorBlueprint',\nall other arguments passed to",
                         " $new() must also be of class 'VectorBlueprint'.\n",
                         "Consult ?VectorBlueprints for more information.",
                         call. = FALSE)
                }

                self$blueprints <- structure(
                    blueprints,
                    names = vapply_1c(blueprints, FALSE, `[[`, i = "name")
                )
            }

            return(self$validate())
        },

        #' @description Validate a [VectorBlueprints] object.
        #' @return The [VectorBlueprints] object invisibly if the object
        #' is valid. Else, an error explaining what is wrong with the object.

        # Here, $blueprints must be a list for $validate() to work
        # properly. So we divide the validation step in two
        # is_valid_r6_instance() calls. Also, $get_names() cannot
        # be used in $validate() because $validate() is itself
        # called in $get_names(). This leads to an infinite loop.
        # Finally, $blueprints is validated only if it has a positive
        # length.
        validate = function()
        {
            is_valid_r6_instance(
                if (!is.list(self$blueprints)) {
                    "$blueprints must be a list."
                }
            )

            if (self$count) {
                field_names <- vapply_1c(self$blueprints, FALSE, `[[`, i = "name")

                is_valid_r6_instance(
                    if (is.null(names(self$blueprints))) {
                        "$blueprints must be a named list."
                    },
                    if (!all(vapply_1l(self$blueprints, FALSE, is_field))) {
                        "$blueprints can only contain 'VectorBlueprint' objects."
                    },
                    if (anyDuplicated(field_names) > 0L) {
                        "$blueprints must contain 'VectorBlueprint' objects with unique names."
                    },
                    if (!identical(names(self$blueprints), field_names)) {
                        "$blueprints' names must be equal to the names stored in 'VectorBlueprint' objects."
                    }
                )
            }

            return(invisible(self))
        },

        #' @description Print a [VectorBlueprints] object.
        #' @return The [VectorBlueprints] object invisibly.
        print = function()
        {
            self$validate()
            cat("<VectorBlueprints>", self$format(), sep = "\n  ")
            return(invisible(self))
        },

        #' @description Format a [VectorBlueprints] object.
        #' @return A character scalar representing the formatted
        #' [VectorBlueprints] object.
        format = function()
        {
            self$validate()
            if (self$count) {
                return(
                    sprintf(
                        "<name:%s type:%s>",
                        pad_string(self$get_names()),
                        self$get_types()
                    )
                )
            } else {
                return("<empty>")
            }
        },

        #' @description Get all [VectorBlueprint] underlying names.
        #' @return A named character vector containing all `$name` slots
        #' of the [VectorBlueprint] objects stored in `$blueprints`.
        get_names = function()
        {
            self$validate()
            return(vapply_1c(self$blueprints, TRUE, `[[`, i = "name"))
        },

        #' @description Get all [VectorBlueprint] underlying types / classes.
        #' @return A named character vector containing all `$type` slots
        #' of the [VectorBlueprint] objects stored in `$blueprints`.
        get_types = function()
        {
            self$validate()
            return(vapply_1c(self$blueprints, TRUE, `[[`, i = "type"))
        },

        #' @description Coerce a [VectorBlueprints] object into a list.
        #' @return A named list of lists of length equal to the number of
        #' [VectorBlueprint] objects stored in `$blueprints`. Refer to the
        #' doc of [`VectorBlueprint$as.list()`][VectorBlueprint] for the
        #' structure of the child elements.
        as.list = function()
        {
            self$validate()
            return(lapply(self$blueprints, as.list))
        },

        #' @description Add [VectorBlueprint] objects to an existing
        #' [VectorBlueprints] object.
        #' @return The updated [VectorBlueprints] object invisibly.
        #' @details Internally, this creates a new [VectorBlueprints] object
        #' by leveraging the [`$new()`][VectorBlueprints] method and append
        #' the result to `$blueprints`.
        #'
        #' Method [`$push()`][VectorBlueprints] is the inverse operation of
        #' method [`$pop()`][VectorBlueprints].
        push = function(...)
        {
            new_blueprints  <- VectorBlueprints$new(...)
            self$blueprints <- c(self$blueprints, new_blueprints$blueprints)
            return(self$validate())
        },

        #' @description Remove [VectorBlueprint] objects from an existing
        #' [VectorBlueprints] object.
        #' @param names A character vector of names. They should correspond
        #' to existing `$name` slots of the [VectorBlueprint] objects stored
        #' in `$blueprints`.
        #' @return The updated [VectorBlueprints] object invisibly.
        #' @details Method [`$pop()`][VectorBlueprints] is the inverse
        #' operation of method [`$push()`][VectorBlueprints].
        pop = function(names)
        {
            if (!is.character(names)) {
                stop("argument 'names' must be a character vector.",
                     call. = FALSE)
            }

            i <- match(names, self$get_names(), 0L)
            if (length(i)) self$blueprints[i] <- NULL

            return(self$validate())
        },

        #' @description A familiar alias for `$push()`.
        #' @return The updated [VectorBlueprints] object invisibly.
        #' @details This method calls [`$push()`][VectorBlueprints] internally;
        #' *pushing* an object to a [VectorBlueprints] collection is equivalent
        #' to a *concatenation* of two [VectorBlueprints] objects.
        c = function(...)
        {
            return(self$push(...))
        }
    )
)


# External helpers -------------------------------------------------------------


#' @rdname VectorBlueprints
#'
#' @usage
#' ## Constructor function
#' new_vector_blueprints(...)
#'
#' @param ... An arbitrary number of elements passed to
#' [`$new()`][VectorBlueprints] (see below to learn more).
#'
#' @return Constructor function [new_vector_blueprints()] is a wrapper to
#' [`$new()`][VectorBlueprints] and returns a [R6][R6::R6] object of class
#' [VectorBlueprints].
#'
#' @export
new_vector_blueprints <- function(...)
{
    return(VectorBlueprints$new(...))
}


#' @rdname VectorBlueprints
#'
#' @usage
#' ## Test if an object is a 'VectorBlueprints' object
#' is_vector_blueprints(x)
#'
#' @param x any \R object.
#'
#' @return External helper functions [is_vector_blueprints()] and
#' [valid_vector_blueprints()] return a logical scalar.
#'
#' @export
is_vector_blueprints <- function(x)
{
    return(inherits(x, "VectorBlueprints", FALSE) && is_blueprint(x))
}


#' @rdname VectorBlueprints
#'
#' @usage
#' ## Validate if an object is a proper 'VectorBlueprints' object
#' valid_vector_blueprints(x)
#'
#' @export
valid_vector_blueprints <- function(x)
{
    if (!is_vector_blueprints(x)) {
        stop("'x' is not a 'VectorBlueprints' object.", call. = FALSE)
    }

    return(valid_r6_instance(x))
}


# S3 methods dispatch ----------------------------------------------------------


#' @export
#' @keywords internal
format.VectorBlueprints <- function(x, ...)
{
    return(x$format())
}


#' @export
#' @keywords internal
as.list.VectorBlueprints <- function(x, ...)
{
    return(x$as.list())
}


#' @export
#' @keywords internal
c.VectorBlueprints <- function(x, ...)
{
    return(x$push(...))
}
