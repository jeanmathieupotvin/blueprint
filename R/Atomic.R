#' @include assertions.R
#' @include utilities.R
#' @include options.R
#' @include Blueprint.R
NULL


# Class ------------------------------------------------------------------------


#' @title Atomic class
#'
#' @description
#' [Atomic] is a simple class that holds useful metadata on
#' \R atomic vectors (usually, objects that can be inserted into
#' [`data.frame`][base::data.frame()] objects). An instance of class
#' [Atomic] registers the vector's underlying class and name.
#' Behind the scenes, it also records all its super-classes and its
#' prototype, which is set equal to `atomic[1L]` (see arguments below to
#' learn what `atomic` means).
#'
#' @param validate A scalar logical. Validate the object before calling
#' the method? This argument is `TRUE` by default.
#'
#' @param file A scalar character. The name of a file to be created.
#'
#' @section Self-validation:
#' The contents of a [Atomic] object is checked each time a method
#' (including [`$new()`][Atomic]). The performance loss is negligible
#' and favored over errors introduced by a *broken* [Atomic] instance.
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @author Jean-Mathieu Potvin (<jm@@potvin.xyz>)
#'
#' @family Blueprint classes
#'
#' @export
Atomic <- R6::R6Class("Atomic",
    class      = TRUE,
    portable   = TRUE,
    lock_class = TRUE,
    cloneable  = FALSE,
    inherit    = Blueprint,
    private    = list(

        # Record all classes of the vector.
        classes   = NA_character_,

        # Record a prototype of the vector.
        prototype = NULL,

        # A method identical to $as_list() but that also re-encodes
        # its character arguments to UTF-8, even its prototype if
        # applicable. This is required when converting object to
        # YAML and JSON formats.
        as_utf8_list = function()
        {
            return(
                list(
                    name      = enc2utf8(self$name),
                    type      = enc2utf8(self$type),
                    length    = self$length,
                    prototype = if (self$type == "character") {
                        enc2utf8(private$prototype)
                    } else {
                        private$prototype
                    }
                )
            )
        }
    ),
    public = list(

        #' @field name A scalar character. A name for the vector.
        name = NA_character_,

        #' @field type A scalar character. The class of the vector.
        type = NA_character_,

        #' @field length A scalar integer. The length of the vector.
        length = NULL,

        #' @description Create a new [Atomic] object.
        #' @param atomic any strict atomic \R vector.
        #' See [is_strict_atomic()] for more information.
        #' @param name A scalar character. The name of the vector passed
        #' to `atomic`.
        #' @param length A scalar integer. This argument is flexible. If
        #' `NULL`, `length` is ignored and not enforced.
        #' @return A [R6][R6::R6] object of class [Atomic].
        initialize = function(atomic, name, length = NULL)
        {
            if (!is_strict_atomic(atomic)) {
                stop("'atomic' must be an atomic vector.",
                     " Consult ?is_strict_atomic() for more information.",
                     call. = FALSE)
            }
            if (!is.null(length)) {
                if (!is.integer(length)) {
                    length <- as.integer(length[[1L]])
                }
                if (is.na(length) || length < 0L) {
                    stop("when supplied, 'length' must be a positive integer scalar.",
                         call. = FALSE)
                }
                self$length <- length
            }

            # Here, it is safer to use `[` than `[[`, because
            # extraction will work on vectors of length 0.
            # Result will be NA of the proper type, which is
            # fine for $prototype.
            private$prototype <- atomic[1L]
            private$classes   <- class(atomic)

            self$name <- name
            self$type <- private$classes[[1L]]

            return(self$validate())
        },

        #' @description Validate a [Atomic] object.
        #' @return The [Atomic] object invisibly if the object is
        #' valid. Else, an error explaining what is wrong with the object.
        validate = function()
        {
            super$validate()

            validate_blueprint(
                if (!is_scalar_character(self$name))  {
                    "$name must be an scalar character."
                },
                if (!is_scalar_character(self$type)) {
                    "$type must be a scalar character."
                },
                if (!is.null(self$length) &&
                    (!is_scalar_integer(self$length) || self$length < 0L)) {
                    "$length must be a positive scalar integer or NULL."
                }
            )

            return(invisible(self))
        },

        #' @description Print a [Atomic] object.
        #' @return The [Atomic] object invisibly.
        print = function()
        {
            cat(sprintf("<Atomic blueprint [%s]>\n  ", self$blueprint_version),
                self$format(),
                sep = ""
            )
            return(invisible(self))
        },

        #' @description Format a [Atomic] object.
        #' @return A character scalar representing the formatted
        #' [Atomic] object.
        format = function(validate = TRUE)
        {
            if (validate) {
                self$validate()
            }

            out <- if (is.null(self$length)) {
                sprintf("<name:%s type:%s>",
                        self$name, self$type)
            } else {
                sprintf("<name:%s type:%s length:%s>",
                        self$name, self$type, self$length)
            }

            return(out)
        },

        #' @description Compare a vector against an [Atomic].
        #' @param object Any [atomic] \R object.
        #' @return A scalar logical. A `TRUE` means that `object` is in
        #' compliance with the underlying [Atomic]:
        #' 1. it has the same `$type` and
        #' 2. if `$length` is **not** `NULL`, it has the same prescribed length.
        compare = function(object, validate = TRUE)
        {
            if (!is_strict_atomic(object)) {
                stop("'object' must be an atomic vector.",
                     " Consult ?is_strict_atomic() for more information.",
                     call. = FALSE)
            }
            if (validate) {
                self$validate()
            }

            return(
                class(object) == self$type &&
                if (!is.null(self$length)) length(object) == self$length else TRUE
            )
        },

        #' @description Coerce a [Atomic] object into a list.
        #' @return A named list of three elements:
        #' \tabular{ll}{
        #' `name`      \tab A scalar character equal to `$name`.\cr
        #' `type`      \tab A scalar character equal to `$type`.\cr
        #' `length`    \tab A scalar integer equal to `$length`.\cr
        #' `prototype` \tab A scalar [strict atomic][is_strict_atomic()] value with a class attribute equal to `$type`.
        #' }
        as_list = function(validate = TRUE)
        {
            if (validate) {
                self$validate()
            }

            return(
                list(
                    name      = self$name,
                    type      = self$type,
                    length    = self$length,
                    prototype = private$prototype
                )
            )
        },

        #' @description Coerce a [Atomic] object into a character.
        #' @return A named character of three elements:
        #' \tabular{ll}{
        #' `name`   \tab A scalar character equal to `$name`.\cr
        #' `type`   \tab A scalar character equal to `$type`.\cr
        #' `length` \tab A scalar character equal to the coerced value of `$length` (from integer to character).
        #' }
        as_character = function(validate = TRUE)
        {
            if (validate) {
                self$validate()
            }

            return(
                c(name   = self$name,
                  type   = self$type,
                  length = as.character(self$length)
                )
            )
        },

        #' @description Convert a [Atomic] object to a YAML text
        #' based format.
        #' @param ... further arguments passed to [yaml::as.yaml()].
        #' @return A scalar character holding a YAML string derived from
        #' method [`$as_list()`][Atomic].
        #' @details
        #' YAML is a human friendly data serialization standard for all
        #' programming languages. The acronym stands for *YAML Ain't Markup
        #' Language* (it is a *recursive* name). It is also designed to be a
        #' strict super-set of JSON (see [`$as_json()`][Atomic]).
        #' To learn more, visit [yaml.org](https://yaml.org/).
        #'
        #' Method [`$as_list()`][Atomic] re-encodes its fields to
        #' UTF-8 (if applicable) before returning or writing the YAML output.
        as_yaml = function(validate = TRUE, file, ...)
        {
            if (validate) {
                self$validate()
            }

            utf8list <- private$as_utf8_list()

            if (missing(file)) {
                return(yaml::as.yaml(utf8list, ...))
            } else {
                if (!is_scalar_character(file)) {
                    stop("'file' must be a scalar character.",
                         call. = FALSE)
                }

                return(yaml::write_yaml(utf8list, file, "UTF-8", ...))
            }
        },

        #' @description Convert a [Atomic] object to a JSON text
        #' based format.
        #' @param ... further arguments passed to [jsonlite::toJSON()].
        #' @return A scalar character holding a JSON string derived from
        #' method [`$as_list()`][Atomic]. Technically, the output
        #' is encapsulated into an object of class `json`. This class is
        #' internally defined in package \pkg{jsonlite} and behave like a
        #' normal character.
        #' @details
        #' JSON.
        as_json = function(validate = TRUE, file, ...)
        {
            if (validate) {
                self$validate()
            }

            if (missing(file)) {
                args <- inject(
                    jsonlite_atomic_opts(),
                    x = private$as_utf8_list()
                )
                return(do.call(jsonlite::toJSON, args))
            } else {
                if (!is_scalar_character(file)) {
                    stop("'file' must be a scalar character.",
                         call. = FALSE)
                }

                args <- inject(
                    jsonlite_atomic_opts(),
                    x    = private$as_utf8_list(),
                    path = file
                )
                return(do.call(jsonlite::write_json, args))
            }
        }
    )
)


# External helpers -------------------------------------------------------------


#' @rdname Atomic
#'
#' @usage
#' ## Test if an object is an 'Atomic' object
#' is_atomic(x)
#'
#' @param x any \R object.
#'
#' @return External helper functions [is_atomic()] and
#' [valid_atomic()] return a logical scalar.
#'
#' @export
is_atomic <- function(x)
{
    return(inherits(x, "Atomic", FALSE) && is_blueprint(x))
}


#' @rdname Atomic
#'
#' @usage
#' ## Validate if an object is a proper 'Atomic' object
#' valid_atomic(x)
#'
#' @export
valid_atomic <- function(x)
{
    if (!is_atomic(x)) {
        stop("'x' is not an 'Atomic' object.", call. = FALSE)
    }

    return(valid_r6_instance(x))
}


# S3 methods dispatch ----------------------------------------------------------


#' @export
#' @keywords internal
format.Atomic <- function(x, validate = TRUE, ...)
{
    return(x$format(validate))
}


#' @export
#' @keywords internal
as.list.Atomic <- function(x, validate = TRUE, ...)
{
    return(x$as_list(validate))
}


#' @export
#' @keywords internal
as.character.Atomic <- function(x, validate = TRUE, ...)
{
    return(x$as_character(validate))
}
