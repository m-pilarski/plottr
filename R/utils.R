`%:::%` <- function(pkg, fun){
  get(fun, envir = asNamespace(pkg), inherits = FALSE)
}

which_bin <- "tinytex" %:::% "which_bin"
