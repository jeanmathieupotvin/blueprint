#' @include assertions.R
NULL


# Internal options and values.


# Default jsonlite options used by method Atomic$as_json().
opts_jsonlite_atomic <- function()
{
    return(
        list(
            auto_unbox = TRUE,
            pretty     = TRUE,
            force      = FALSE,
            complex    = "string",
            raw        = "base64",
            null       = "null",
            na         = "string"
        )
    )
}
