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
#' @param .validate A scalar logical. Validate the object before calling
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

        # Record a prototype of the vector.
        prototype = NULL,

        # Record (valid) strict atomic types.
        valid_types = c(
            "NULL",
            "logical",
            "integer",
            "single",
            "double",
            "complex",
            "character",
            "raw"
        )
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
                stop("'atomic' must be a strict atomic vector.",
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

            self$name <- name
            self$type <- if (is_single(atomic)) "single" else typeof(atomic)

            # Here, it is safer to use `[` than `[[`, because
            # extraction will work on vectors of length 0.
            # Result will be NA of the proper type, which is
            # fine for $prototype. We only need to watch out
            # for single values of length 0 passed to atomic.
            # `[` drops the Csingle attribute, but it must be kept.
            private$prototype <- if (is_single(atomic) && !length(atomic)) {
                single(1L)
            } else {
                atomic[1L]
            }

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
                if (is.na(match(self$type, private$valid_types))) {
                    "$type should be a strict atomic type."
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
        #' @return A character scalar representing the formatted [Atomic] object.
        format = function(.validate = TRUE)
        {
            if (.validate) {
                self$validate()
            }

            if (is.null(self$length)) {
                return(sprintf("<name:%s type:%s>", self$name, self$type))
            } else {
                return(
                    sprintf("<name:%s type:%s length:%s>",
                            self$name, self$type, self$length)
                )
            }
        },

        #' @description Compare a vector against an [Atomic].
        #' @param object Any [atomic] \R object.
        #' @return A scalar logical. A `TRUE` means that `object` is in
        #' compliance with the underlying [Atomic]:
        #' 1. it has the same `$type` and
        #' 2. if `$length` is **not** `NULL`, it has the same prescribed length.
        compare = function(object, .validate = TRUE)
        {
            if (.validate) {
                self$validate()
            }

            # We compare lengths only if $length is not NULL.
            is_same_length <- (!is.null(self$length) &&
                                   length(object) == self$length) || TRUE

            if (self$type == "single") {

                # We only have to check if object is a single
                # with our convenient is_single() function if
                # $type is single. We also put is_same_length
                # in first since it is already computed. Makes
                # the function faster if it is FALSE (the rest
                # is not evaluated).
                return(is_same_length && is_single(object))
            } else {

                # For other atomic types, we check lengths,
                # strictness and types.
                return(
                    is_same_length &&
                        is_strict_atomic(object) &&
                        typeof(object) == self$type
                )
            }
        },

        #' @description Create a strict atomic vector from an [Atomic] object.
        #' @return A vector. Its underlying `type` and `length` is given by
        #' the fields `$type` and `$length`. It is initialized with values based
        #' on an internal prototype.
        generate = function(.validate = TRUE)
        {
            if (.validate) {
                self$validate()
            }

            if (is.null(self$length)) {

                # If $length is NULL, just return the prototype.
                return(private$prototype)
            } else if (self$type == "NULL") {

                # If $type is NULL, just return NULL, no
                # matter the length. A NULL is always a
                # scalar, never a vector.
                return(NULL)
            } else if (self$type == "single") {

                # If $type is single and $length is not NULL,
                # the Csingle attribute must be kept and passed
                # to generated vector.
                return(
                    structure(
                        rep.int(private$prototype, self$length),
                        Csingle = TRUE
                    )
                )
            } else {

                # If $type is single and $length is not NULL,
                # just replicate the prototype $length times.
                return(rep.int(private$prototype, self$length))
            }
        },

        #' @description Coerce a [Atomic] object into a list.
        #' @return A named list of three elements:
        #' \tabular{ll}{
        #' `name`      \tab A scalar character equal to `$name`.\cr
        #' `type`      \tab A scalar character equal to `$type`.\cr
        #' `length`    \tab A scalar integer equal to `$length`.\cr
        #' `prototype` \tab A scalar [strict atomic][is_strict_atomic()] value with a class attribute equal to `$type`.
        #' }
        as_list = function(.validate = TRUE)
        {
            if (.validate) {
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
        as_character = function(.validate = TRUE)
        {
            if (.validate) {
                self$validate()
            }

            return(
                c(name   = self$name,
                  type   = self$type,
                  length = as.character(self$length))
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
        as_yaml = function(file, headers, ..., .validate = TRUE)
        {
            if (.validate) {
                self$validate()
            }

            out <- as_utf8(
                add_headers(self$as_list(), headers, "Atomic", "as_yaml")
            )

            if (missing(file)) {
                return(yaml::as.yaml(out, ...))
            } else {
                if (!is_scalar_character(file)) {
                    stop("'file' must be a scalar character.",
                         call. = FALSE)
                }

                return(
                    yaml::write_yaml(out, file, "UTF-8", ...)
                )
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
        as_json = function(file, headers, ..., .validate = TRUE)
        {
            if (.validate) {
                self$validate()
            }

            out <- add_headers(self$as_list(), headers, "Atomic", "as_json")

            if (missing(file)) {
                args <- inject(opts_jsonlite_atomic(), x = as_utf8(out))
                return(do.call(jsonlite::toJSON, args))
            } else {
                if (!is_scalar_character(file)) {
                    stop("'file' must be a scalar character.",
                         call. = FALSE)
                }

                args <- inject(
                    opts_jsonlite_atomic(),
                    x    = as_utf8(out),
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
as.list.Atomic <- function(x, ..., .validate = TRUE)
{
    return(x$as_list(.validate = .validate))
}


#' @export
#' @keywords internal
as.character.Atomic <- function(x, ..., .validate = TRUE)
{
    return(x$as_character(.validate = .validate))
}
