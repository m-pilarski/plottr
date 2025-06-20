`%:::%` <- function(pkg, fun){
  get(fun, envir = asNamespace(pkg), inherits = FALSE)
}

which_bin <- "tinytex" %:::% "which_bin"

#'
#'
#'
#'
install_latex_deps <- function(){

  tinytex::install_tinytex()

  .tex_pkg_deps <- c(
    "amsmath", "context", "ec", "epstopdf-pkg", "etoolbox", "fontspec",
    "graphics", "graphics-cfg", "graphics-def", "grfext", "iftex", "infwarerr",
    "kvdefinekeys", "kvoptions", "kvsetkeys", "l3backend", "l3kernel",
    "l3packages", "latexconfig", "lm", "ltxcmds", "lualatex-math", "luatex",
    "luatex85", "microtype", "ragged2e", "oberdiek", "pdftexcmds", "pgf",
    "preview", "unicode-data", "unicode-math", "xcolor", "xunicode"
  )

  # optional
  .tex_pkg_deps <- c(.tex_pkg_deps, "tex-gyre")

  tinytex::tlmgr_install(c(
    .tex_pkg_deps[!tinytex::check_installed(.tex_pkg_deps)]
  ))

}
