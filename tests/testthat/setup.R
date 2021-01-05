# General objects used when testing the package are declared here.


# Pseudo NA values for atomic types.
NA_single_ <- as.single(NA_real_, Csingle = TRUE)
NA_raw_    <- as.raw("0")


# Function to get prototype easily (a private field) from class Atomic.
proto <- function(x)
{
    return(x$.__enclos_env__$private$prototype)
}


# Function to use $set() method of super-class.
superset <- function(x, field, value, .validate = TRUE)
{
    return(x$.__enclos_env__$super$set(field, value, .validate))
}
