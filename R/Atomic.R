#' @include assertions.R
#' @include utilities.R
#' @include options.R
#' @include Blueprint.R
NULL


# Class ------------------------------------------------------------------------


#' @title Atomic: a class for strict atomic vectors
#'
#' @description
#' [Atomic] is an important building block of \pkg{blueprint}: it creates
#' blueprints for [strict atomic vectors][is_strict_atomic()] (vectors of
#' any \R atomic type, including [NULL], that has no attribute). Instances
#' of the [Atomic] class hold useful (derived) metadata on strict vectors:
#' their types, names, prototypes and optionally, their lengths.
#'
#' @param file A scalar character. The name of a file to be created. If missing,
#' a string will be returned to the console.
#'
#' @param headers A non-empty named list that holds additional key/value
#' pairs to include in text representations of the object (either JSON or YAML).
#' Ignore this argument if not needed.
#'
#' @template param-field
#'
#' @template param-value
#'
#' @template param-validate
#'
#' @template param-source-header
#'
#' @template section-self-validation
#'
#' @section Updating fields of the Atomic class:
#' Users should **never** manually change fields' values of an [Atomic] instance
#' manually. Instead, use [`$set()`][Atomic] to do it safely. Refer to the
#' documentation of this method (see below) for more information.
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
    active     = NULL,
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

        #' @field name A non-empty, non-NA scalar character. A name for the vector.
        name = NA_character_,

        #' @field type A non-empty, non-NA scalar character. The class of the vector.
        type = NA_character_,

        #' @field length A scalar integer. The length of the vector. If `NULL`,
        #' it is ignored by the class instance.
        length = NULL,

        #' @description Create an [Atomic] object.
        #'
        #' @param atomic any [strict atomic][is_strict_atomic()] \R vector.
        #'
        #' @param name A non-empty, non-NA scalar character. The name of the
        #' vector passed to `atomic`.
        #'
        #' @param length A scalar integer. This argument is flexible. If
        #' `NULL`, `$length` is ignored.
        #'
        #' @return A [R6][R6::R6] object of class [Atomic].
        #'
        #' @examples
        #' ## Create a blueprint and do not enforce a length.
        #' Atomic$new(1L, "myVectorName")
        #'
        #' ## Create a blueprint that enforces a specific length; here, 10.
        #' Atomic$new(1L, "myVectorName", 10L)
        initialize = function(atomic, name = character(), length = NULL)
        {
            if (!is_strict_atomic(atomic)) {
                stop("'atomic' must be a strict atomic vector.",
                     " Consult ?is_strict_atomic() for more information.",
                     call. = FALSE)
            }
            if (!is.null(length)) {
                if (!is.integer(length)) {
                    length <- suppressWarnings(as.integer(length[[1L]]))
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
            private$prototype <- if (is_single(atomic)) {
                if (length(atomic)) {

                    # Ensure that a single is returned.
                    structure(atomic[1L], Csingle = TRUE)
                } else {

                    # Create a NA_single_ value derived
                    # from single value definition.
                    structure(NA_real_, Csingle = TRUE)
                }
            } else {
                atomic[1L]
            }

            return(self$validate())
        },

        #' @description Validate an [Atomic] object.
        #'
        #' @return The [Atomic] object invisibly if the object is valid.
        #' Else, an error explaining what is wrong with the object.
        validate = function()
        {
            super$validate()

            # Errors are accumulated before being returned.
            # Therefore, $type needs to be a scalar for match(),
            # so we check its first element only.
            report_errors(
                if (!is_scalar_character(self$name, FALSE) || !nzchar(self$name)) {
                    "$name must be a non-empty scalar character."
                },
                if (!is_scalar_character(self$type, FALSE)) {
                    "$type must be a scalar character."
                },
                if (!match(self$type[[1L]], private$valid_types, 0L)) {
                    "$type must be a strict atomic type."
                },
                if (!is.null(self$length) &&
                    (!is_scalar_integer(self$length, FALSE) || self$length < 0L)) {
                    "$length must be a positive scalar integer or NULL."
                }
            )

            return(invisible(self))
        },

        #' @description Print an [Atomic] object.
        #'
        #' @return The [Atomic] object invisibly.
        #'
        #' @details
        #' The object is automatically validated before being printed.
        print = function()
        {
            # $print() does not call $validate() because
            # it is indirectly called through $format().
            fmt <- self$format()
            cat(sprintf("<Atomic blueprint [%s]>\n  ", self$version), fmt,
                sep = ""
            )
            return(invisible(self))
        },

        #' @description Format an [Atomic] object.
        #'
        #' @return A character scalar representing the formatted [Atomic]
        #' object.
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

        #' @description Compare an object against an [Atomic].
        #'
        #' @param object Any \R object.
        #'
        #' @return A scalar logical. A `TRUE` means that `object` is in
        #' compliance with the underlying [Atomic]:
        #' 1. it has the same `$type` and
        #' 2. if `$length` is **not** `NULL`, it has the same prescribed length.
        #'
        #' @examples
        #' ## Compare values against an Atomic blueprint that do not enforce a length.
        #' bp_no_length <- Atomic$new("hello", "myBlueprint")
        #'
        #' bp_no_length$compare(c("hi", "bye")) # TRUE
        #' bp_no_length$compare("bye")          # TRUE
        #' bp_no_length$compare(1)              # FALSE
        #'
        #' ## Compare values against an Atomic blueprint that enforces a length.
        #' bp_length <- Atomic$new("hello", "myBlueprint", 1L)
        #'
        #' bp_length$compare(c("hi", "bye")) # FALSE
        #' bp_length$compare("bye")          # TRUE
        #' bp_length$compare(1)              # FALSE
        compare = function(object, .validate = TRUE)
        {
            if (.validate) {
                self$validate()
            }

            # We compare lengths only if $length is not NULL.
            is_same_length <- if (is.null(self$length)) {
                TRUE
            } else {
                length(object) == self$length
            }

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

        #' @description
        #' Create (*spawn*) a strict atomic vector from an [Atomic] object.
        #'
        #' @return A [strict atomic vector][is_strict_atomic()]. Its underlying
        #' `type` and `length` is respectively given by fields `$type` and
        #' `$length`. It is initialized with a suitable prototype derived from
        #' argument `atomic` (see [`$new()`][Atomic]) and registered internally
        #' when the object is created.
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
                return(as.single(rep.int(private$prototype, self$length)))
            } else {

                # If $type is single and $length is not NULL,
                # just replicate the prototype $length times.
                return(rep.int(private$prototype, self$length))
            }
        },

        #' @description Create a strict atomic vector from an [Atomic] object
        #' and bind it to `$name` in a given environment or list-like object.
        #'
        #' **This is an experimental feature.**
        #'
        #' @param where An environment or a list-like object.
        #'
        #' @param lock A scalar logical. If `where` is an environment, should
        #' the binding be locked? If so, it cannot be changed until it is
        #' [unlocked][base::unlockBinding()].
        #'
        #' @return The [Atomic] object invisibly if the object is valid.
        #' Else, an error explaining what is wrong with the object.
        #'
        #' @examples
        #' ## Create an environment.
        #' env <- new.env()
        #'
        #' ## Spawn a vector from a blueprint and bind the result to $name
        #' ## in env. We also lock the binding.
        #' b <- Atomic$new(12.0, "vector", 1000L)$bind(env, TRUE)
        #'
        #' ## Trying to change value of a locked binding results in an error.
        #' \dontrun{env$vector <- 1.0}
        #'
        #' ## You can later unlock the binding.
        #' unlockBinding("vector", env)
        #' env$vector <- "new value"
        #'
        #' ## Bind a prototype to $name within a list.
        #' lst <- list(value = 1L)
        #' b$bind(lst)
        #' lst$vector
        bind = function(where = parent.frame(), lock = FALSE, .validate = TRUE)
        {
            if (!is_scalar_logical(lock, FALSE)) {
                stop("'lock' must be a scalar logical.", call. = FALSE)
            }
            if (.validate) {
                self$validate()
            }

            if (is.environment(where)) {

                # Assign the generated prototype in the given
                # environment and lock the binding if required.
                assign(
                    self$name,
                    self$generate(.validate = FALSE),
                    envir = where
                )
                if (lock) lockBinding(self$name, where)
            } else if (is.recursive(where)) {

                # Generate a prototype and bind it to a key
                # named after $name in the 'where' list. Bind
                # the updated list to the underlying name of
                # 'where' in the parent environment. This is
                # a rather hacky way to achieve this. It could
                # also be unsafe and requires further testing.
                # Because of that, we consider this feature to
                # be experimental.
                wname <- deparse(substitute(where))
                where[[self$name]] <- self$generate(.validate = FALSE)
                assign(wname, where, envir = parent.frame())
            } else {
                stop("'where' must be an environment or an object that has a recursive structure, like a list, a data.frame, etc.",
                     call. = FALSE)
            }

            return(if (.validate) self$validate() else invisible(self))
        },

        #' @description Coerce an [Atomic] object to a list.
        #'
        #' @return A named list of four elements:
        #' \tabular{ll}{
        #' `name`      \tab A scalar character equal to `$name`.\cr
        #' `type`      \tab A scalar character equal to `$type`.\cr
        #' `length`    \tab A scalar integer equal to `$length` or `NULL`.\cr
        #' `prototype` \tab A scalar [strict atomic value][is_strict_atomic()]
        #' with a [class][base::class()] and a [type][base::typeof()] attribute
        #' equal to `$type`.
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

        #' @description Coerce an [Atomic] object to a character.
        #'
        #' @return A named character of three elements:
        #' \tabular{ll}{
        #' `name`   \tab A scalar character equal to `$name`.\cr
        #' `type`   \tab A scalar character equal to `$type`.\cr
        #' `length` \tab A scalar character equal to the coerced
        #' value of `$length` (from integer to character). If
        #' `$length` is `NULL`, the string `"NULL"` is returned.
        #' }
        as_character = function(.validate = TRUE)
        {
            if (.validate) {
                self$validate()
            }

            return(
                c(name   = self$name,
                  type   = self$type,
                  length = if (is.null(self$length)) {
                      "NULL"
                  } else {
                      as.character(self$length)
                  }
                )
            )
        },

        #' @description Convert an [Atomic] object to a YAML text
        #' based format.
        #'
        #' @param handlers A named list of custom functions passed
        #' to [yaml::as.yaml()]. These handle how objects should be
        #' converted into a YAML string. For more information, see
        #' argument `handlers` of [yaml::as.yaml()]. If missing,
        #' a default list of handlers is used. Users can overwrite
        #' them, but be advised that this should be reserved to
        #' expert users.
        #'
        #' @param ... further arguments passed to [yaml::as.yaml()]. You
        #' cannot pass argument `handlers`.
        #'
        #' @return If `file` is missing, a scalar character holding a YAML
        #' string derived from method [`as_list()`][Atomic]. Else, the output
        #' is written to `file` and the [Atomic] object is returned invisibly.
        #'
        #' @details
        #' YAML (*YAML Ain't Markup Language*, a recursive name) is a
        #' human-friendly data serialization standard usable in almost
        #' any programming language. It is designed to be a strict
        #' super-set of JSON (see [`as_json()`][Atomic]).
        #' To learn more, visit [yaml.org](https://yaml.org).
        #'
        #' Method [`as_yaml()`][Atomic] automatically re-encodes its
        #' fields to UTF-8 (if applicable) before returning or writing
        #' the YAML output. Additional headers passed to `headers` are
        #' also re-encoded, if applicable.
        #'
        #' By default, [raw][base::raw()] values will be encoded into
        #' a [base64][jsonlite::base64_enc()] string. This string can
        #' be decoded again into [raw][base::raw()] values by using
        #' function [jsonlite::base64_dec()]. Users can overwrite this
        #' default behavior by passing a custom `raw` handler function
        #' to argument `handlers`.
        #'
        #' @examples
        #' ## Create an Atomic object.
        #' ab <- Atomic$new(sample.int(10L), "randomValues", 10L)
        #'
        #' ## Create a simple YAML representation and print it.
        #' cat(ab$as_yaml())
        #'
        #' ## Add additional headers to output.
        #' myheaders <- list(author = "JM Potvin", date = "January 1st 2021")
        #' cat(ab$as_yaml(headers = myheaders))
        #'
        #' ## Write output to a file.
        #' path <- tempfile(fileext = ".yaml")
        #' ab$as_yaml(file = path, headers = myheaders)
        #'
        #' ## Output is always encoded to UTF-8.
        #' Encoding(ab$as_yaml(headers = list(utf8char = "`"))) == "UTF-8"
        #'
        #' ## You can pass additional parameters to yaml::as.yaml().
        #' cat(ab$as_yaml(indent = 4L))
        #'
        #' ## Use custom handlers.
        #' ## Here, we use another handler for raw values.
        #' handlers <- list(
        #'     raw = function(x) { return(as.character(x)) }
        #' )
        #' ab$as_yaml(handlers = handlers)
        as_yaml = function(
            file     = character(),
            headers  = list(),
            handlers = list(),
            source_header  = TRUE,
            ..., .validate = TRUE)
        {
            if (!is_scalar_logical(source_header, FALSE)) {
                stop("'source_header' must be a scalar logical.",
                     call. = FALSE)
            }
            if (.validate) {
                self$validate()
            }

            out <- as_utf8(
                add_headers(
                    self$as_list(.validate),
                    headers,
                    "Atomic",
                    "as_yaml",
                    TRUE,
                    source_header
                )
            )

            # Override our default handlers with users custom handlers.
            handlers <- opts_yaml_handlers(handlers)

            if (missing(file)) {
                return(yaml::as.yaml(out, handlers = handlers, ...))
            } else if (is_scalar_character(file, FALSE) && nzchar(file)) {
                yaml::write_yaml(out, file, "UTF-8", handlers = handlers, ...)
                return(if (.validate) self$validate() else invisible(self))
            } else {
                stop("'file' must be a non-empty scalar character.",
                     call. = FALSE)
            }
        },

        #' @description Convert an [Atomic] object to a JSON text
        #' based format.
        #'
        #' @param ... further arguments passed to [jsonlite::toJSON()]. You
        #' cannot passe arguments `x` and `path`.
        #'
        #' @return If `file` is missing, a scalar character holding a JSON
        #' string derived from method [`as_list()`][Atomic]. This string is
        #' encapsulated into an object of class `json`, which is internally
        #' defined in package \pkg{jsonlite} and behaves like a normal character.
        #' Else, the output is written to `file` and the [Atomic] object is
        #' returned invisibly.
        #'
        #' @details
        #' JSON (*JavaScript Object Notation*) is a lightweight and
        #' human-friendly data serialization format usable in almost
        #' any programming language. Its design is derived from
        #' JavaScript's objects notation. Compared to YAML, it is
        #' slightly more verbose. To learn more, visit
        #' [www.json.org](https://www.json.org).
        #'
        #' Method [`as_json()`][Atomic] automatically re-encodes its
        #' fields to UTF-8 (if applicable) before returning or writing
        #' the JSON output. Additional headers passed to `headers` are
        #' also re-encoded, if applicable.
        #'
        #' By default, [raw][base::raw()] values will be encoded into
        #' a [base64][jsonlite::base64_enc()] string. This string can
        #' be decoded again into [raw][base::raw()] values by using
        #' function [jsonlite::base64_dec()]. Users can overwrite this
        #' default behavior. See argument `raw` of [jsonlite::toJSON()].
        #'
        #' @examples
        #' ## Create an Atomic object.
        #' ab <- Atomic$new(sample.int(10L), "randomValues", 10L)
        #'
        #' ## Create a simple JSON representation and print it.
        #' cat(ab$as_json())
        #'
        #' ## Add additional headers to output.
        #' myheaders <- list(author = "JM Potvin", date = "January 1st 2021")
        #' cat(ab$as_json(headers = myheaders))
        #'
        #' ## Write output to a file.
        #' path <- tempfile(fileext = ".json")
        #' ab$as_json(file = path, headers = myheaders)
        #'
        #' ## Output is always encoded to UTF-8.
        #' Encoding(ab$as_json(headers = list(utf8char = "`"))) == "UTF-8"
        #'
        #' ## You can pass additional parameters to jsonlite::toJSON().
        #' cat(ab$as_json(headers = list(test = 1.23456789)))
        #' cat(ab$as_json(headers = list(test = 1.23456789), digits = 8L))
        as_json = function(
            file    = character(),
            headers = list(),
            source_header  = TRUE,
            ..., .validate = TRUE)
        {
            if (!is_scalar_logical(source_header, FALSE)) {
                stop("'source_header' must be a scalar logical.",
                     call. = FALSE)
            }
            if (.validate) {
                self$validate()
            }

            out <- as_utf8(
                add_headers(
                    self$as_list(.validate),
                    headers,
                    "Atomic",
                    "as_json",
                    TRUE,
                    source_header
                )
            )

            if (missing(file)) {
                return(do.call(jsonlite::toJSON, opts_json_atomic(x = out, ...)))
            } else if (is_scalar_character(file, FALSE) && nzchar(file)) {
                do.call(
                    jsonlite::write_json,
                    opts_json_atomic(x = out, path = file, ...)
                )

                return(if (.validate) self$validate() else invisible(self))
            } else {
                stop("'file' must be a non-empty scalar character.",
                     call. = FALSE)
            }
        },

        #' @description Update a field's value of an [Atomic] object.
        #'
        #' @return The [Atomic] object invisibly if the object is valid.
        #' Else, an error explaining what is wrong with the object.
        #'
        #' @details
        #' Changing the value of field `$type` is forbidden, because it could
        #' break internal prototyping mechanisms. Attempting to do so will
        #' result in an error. If `$type` must be changed, create a new [Atomic]
        #' object instead.
        #'
        #' It is worthwhile to note that [`Atomic$set()`][Atomic] is a safe
        #' wrapper that calls [`Blueprint$set()`][Blueprint].
        #'
        #' @examples
        #' ## Update $name. You could also update $length.
        #' b <- Atomic$new(double(10L), "wrong-name")
        #' b$set("name", "good-name")$print()
        #'
        #' ## Trying to update $type will throw an error.
        #' \dontrun{
        #' b$set("type", "raw")
        #' }
        #' b_new <- Atomic$new(raw(10L), "good-name")
        set = function(field = character(), value, .validate = TRUE)
        {
            if (identical(field, "type")) {
                stop("generate a new Atomic blueprint to change $type.",
                     call. = FALSE)
            }

            return(super$set(field, value, .validate))
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
#' @return
#' * External helper function [is_atomic()] returns a logical scalar.
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
#' @return
#' * External helper function [valid_atomic()] returns a logical scalar if
#' the object is valid. Else, an error explaining what is wrong is returned.
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


# @param ... Ignored.
#' @export
#' @keywords internal
as.list.Atomic <- function(x, .validate = TRUE, ...)
{
    return(x$as_list(.validate = .validate))
}


# @param ... Ignored.
#' @export
#' @keywords internal
as.character.Atomic <- function(x, .validate = TRUE, ...)
{
    return(x$as_character(.validate = .validate))
}
