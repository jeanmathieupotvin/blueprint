# General objects used when testing the package are declared here.


# Function to suppress outputs stemming from calls to cat().
quiet <- function(x, file = tempfile())
{
    sink(file)
    on.exit({ sink(); file.remove(file) })
    return(invisible(x))
}


# Function to construct single vectors easily.
new_single <- function(x)
{
    return(structure(as.double(x), Csingle = TRUE))
}


# Pseudo NA values for atomic types.
NA_single_ <- new_single(NA_real_)
NA_raw_    <- as.raw("00")


# Function to get prototype (a private field) from class Atomic.
proto <- function(x) { return(x$.__enclos_env__$private$prototype) }
