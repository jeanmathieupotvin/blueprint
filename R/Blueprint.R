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
#' @param field A scalar character. The name of a field. If missing,
#' the whole object is returned invisibly.
#'
#' @param value Any \R value. The value to use when updating a field. Its
#' type must match the field's underlying structure.
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
        #' @return The underlying value corresponding to the chosen field.
        #' If it does not exist, `NULL` is returned.
        get = function(field)
        {
            if (missing(field)) {
                return(self$validate())
            } else {
                return(self[[field]])
            }
        },

        #' @description Update a field's value of a [Blueprint] object.
        #'
        #' @return The [Blueprint] object invisibly if the object is valid.
        #' Else, an error explaining what is wrong with the object.
        #'
        #' @details
        #' Beware! Class [Blueprint] has no modifiable fields.
        set = function(field, value)
        {
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
                stop("the object has no modifiable (public) fields.",
                     call. = FALSE)
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
