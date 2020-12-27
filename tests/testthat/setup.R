# General objects used when testing the package are declared here.


# Function to suppress outputs stemming from calls to cat().
quiet <- function(x, file = tempfile()) {
    sink(file)
    on.exit({ sink(); file.remove(file) })
    return(invisible(x))
}
