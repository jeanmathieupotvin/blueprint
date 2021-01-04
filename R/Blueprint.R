#' @include assertions.R
#' @include utilities.R
NULL


# Class ------------------------------------------------------------------------


#' @title The Blueprint (root) super-class
#'
#' @description
#' [Blueprint] is the root super-class of all [R6][R6::R6] classes of package
#' \pkg{blueprint} and provides important low-level mechanisms. All classes of
#' \pkg{blueprint} inherit class [Blueprint].
#'
#' **This class is definitely not useful for typical users**. You should
#' consider it a virtual class.
#'
#' @template param-field
#'
#' @template param-value
#'
#' @template param-validate
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
Blueprint <- R6::R6Class("Blueprint",
    class        = TRUE,
    portable     = TRUE,
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = FALSE,
    inherit      = NULL,
    private      = list(

        # Register package's version whenever an object is created.
        # We hide this value from users and make it accessible via
        # an active field. This is safer and faster.
        pkg_ver = as.character(utils::packageVersion("blueprint"))
    ),
    active = list(

        #' @field is_blueprint A scalar logical always equal to TRUE.
        is_blueprint = function() { return(TRUE) },

        #' @field version A scalar character that registers
        #' \pkg{blueprint}'s version at the moment the object is created.
        version = function() { return(private$pkg_ver) }
    ),
    public = list(

        #' @description Create a new [Blueprint] object.
        #'
        #' @return A [R6][R6::R6] object of class [Blueprint].
        initialize = function()
        {
            return(self$validate())
        },

        #' @description Validate a [Blueprint] object.
        #'
        #' @return The [Blueprint] object invisibly if the object is valid.
        #' Else, an error explaining what is wrong with the object.
        validate = function()
        {
            return(invisible(self))
        },

        #' @description Print a [Blueprint] object.
        #'
        #' @return The [Blueprint] object invisibly.
        #'
        #' @details
        #' The object is automatically validated before being printed.
        print = function()
        {
            self$validate()
            cat(sprintf("<Blueprint [%s]>", self$version))
            return(invisible(self))
        },

        #' @description Format a [Blueprint] object.
        #'
        #' @return A character scalar representing the formatted
        #' [Blueprint] object.
        format = function()
        {
            return("<Blueprint>")
        },

        #' @description Extract a field's value from a [Blueprint] object.
        #'
        #' @return The value corresponding to the chosen `field`. If it
        #' does not exist, `NULL` is returned. If `field` is missing,
        #' the underlying [Blueprint] object is returned invisibly.
        #'
        #' @examples
        #' ## Get a field, here 'version'.
        #' Blueprint$new()$get("version")
        #'
        #' ## Extract a non-existent field, here 'nope'. NULL is returned.
        #' Blueprint$new()$get("nope")
        #'
        #' ## Just return the whole object (is a functional style).
        #' ## The output is invisible, we must print it to see it.
        #' Blueprint$new()$get()$print()
        get = function(field = character(), .validate = TRUE)
        {
            if (.validate) {
                self$validate()
            }

            if (length(field)) {
                if (!is_scalar_character(field, FALSE)) {
                    stop("'field' must be a scalar character.", call. = FALSE)
                }
                return(self[[field]])
            } else {
                return(invisible(self))
            }
        },

        #' @description Update a field's value of a [Blueprint] object.
        #'
        #' @return The [Blueprint] object invisibly if the object is valid.
        #' Else, an error explaining what is wrong with the object.
        #'
        #' @details
        #' Beware! Class [Blueprint] has no modifiable fields.
        #'
        #' @examples
        #' ## Trying to update a non-public field throws an error!
        #' \dontrun{
        #' Blueprint$new()$set("version", "1.0.0")
        #' }
        set = function(field = character(), value, .validate = TRUE)
        {
            if (!is_scalar_character(field, FALSE)) {
                stop("'field' must be a scalar character.", call. = FALSE)
            }
            if (.validate) {
                self$validate()
            }

            # By design, classes in blueprint refers to
            # the underlying object generator and we can
            # get fields easily from these generators.
            r6gen  <- get(class(self)[[1L]])
            fields <- names(r6gen$public_fields)

            if (length(fields)) {
                field <- match.arg(field, fields)
                self[[field]] <- value
                return(self$validate())
            } else {
                stop("the object has no modifiable fields.", call. = FALSE)
            }
        }
    )
)


# External helpers -------------------------------------------------------------


#' @rdname Blueprint
#'
#' @usage
#' ## Test if an object is a 'Blueprint' object
#' is_blueprint(x)
#'
#' @param x any \R object.
#'
#' @return
#' * External helper function [is_blueprint()] returns a logical scalar.
#'
#' @export
is_blueprint <- function(x)
{
    return(inherits(x, "Blueprint", FALSE) && x$is_blueprint)
}


#' @rdname Blueprint
#'
#' @usage
#' ## Validate if an object is a proper 'Blueprint' object
#' valid_blueprint(x)
#'
#' @return
#' * External helper function [valid_blueprint()] returns a logical scalar if
#' the object is valid. Else, an error explaining what is wrong is returned.
#'
#' @export
valid_blueprint <- function(x)
{
    if (!is_blueprint(x)) {
        stop("'x' is not a 'Blueprint' object.", call. = FALSE)
    }

    return(valid_r6_instance(x))
}
