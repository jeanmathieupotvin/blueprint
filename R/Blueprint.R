#' @include assertions.R
#' @include utilities.R
NULL


# Class ------------------------------------------------------------------------


#' @title The Blueprint (root) super-class
#'
#' @description
#' [Blueprint] is the root super-class of all [R6][R6::R6] classes of package
#' \pkg{blueprint}. In other words, all classes defined in package
#' \pkg{blueprint} inherit class [Blueprint]. This class is **definitely not
#' useful to the user** and is mostly used has a safeguard against probable
#' class names collisions.
#'
#' You should consider it as a virtual class.
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
Blueprint <- R6::R6Class("Blueprint",
    class        = TRUE,
    portable     = TRUE,
    lock_class   = TRUE,
    lock_objects = TRUE,
    cloneable    = FALSE,
    inherit      = NULL,
    private      = NULL,
    active       = list(

        #' @field blueprint_version A scalar character that holds the
        #' \pkg{blueprint} package's version when an object is created.
        blueprint_version = function()
        {
            return(as.character(utils::packageVersion("Blueprint")))
        }
    ),
    public = list(

        #' @field is_blueprint A scalar logical always equal to `TRUE`.
        is_blueprint = TRUE,

        #' @description Create a new [Blueprint] object.
        #' @return A [R6][R6::R6] object of class [Blueprint].
        initialize = function()
        {
            return(self$validate())
        },

        #' @description Validate a [Blueprint] object.
        #' @return The [Blueprint] object invisibly if the object is valid.
        #' Else, an error explaining what is wrong with the object.
        #' @details A [Blueprint] instance is valid if `$is_blueprint` is
        #' `TRUE`.
        validate = function()
        {
            is_valid_r6_instance(
                if (!self$is_blueprint) {
                    "$is_blueprint is FALSE. It should be TRUE. Investigate."
                }
            )

            return(invisible(self))
        },

        #' @description Print a [Blueprint] object.
        #' @return The [Blueprint] object invisibly.
        print = function()
        {
            cat(self$format())
            return(invisible(self))
        },

        #' @description Format a [Blueprint] object.
        #' @return A character scalar representing the formatted
        #' [Blueprint] object.
        format = function()
        {
            return("<Blueprint>")
        }
    )
)


# External helpers -------------------------------------------------------------


#' @rdname Blueprint
#'
#' @usage
#' ## Constructor function
#' new_blueprint()
#'
#' @return Constructor function [new_blueprint()] is a wrapper to
#' [`$new()`][Blueprint] and returns a [R6][R6::R6] object of
#' class [Blueprint].
#'
#' @export
new_blueprint <- function()
{
    return(Blueprint$new())
}


#' @rdname Blueprint
#'
#' @usage
#' ## Test if an object is a 'Blueprint' object
#' is_blueprint(x)
#'
#' @param x any \R object.
#'
#' @return External helper functions [is_blueprint()] returns a logical scalar.
#'
#' @export
is_blueprint <- function(x)
{
    return(inherits(x, "Blueprint", FALSE) && x$is_blueprint)
}


# S3 methods dispatch ----------------------------------------------------------


#' @export
#' @keywords internal
format.Blueprint <- function(x, ...)
{
    return(x$format())
}