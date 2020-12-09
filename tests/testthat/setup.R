# Load auxiliary function to suppress outputs stemming from calls
# to cat() during tests.
quiet <- function(x, file = tempfile()) {
    sink(file)
    on.exit({ sink(); file.remove(file) })
    invisible(force(x))
}
