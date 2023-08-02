#' try to install "preview" when using tinytex distro
#'
#' @param .plot_obj A number.
#' @param .plot_name ...
#' @param .figure_dir ...
#' @param .width ...
#' @param .height ...
#' @param .units ...
#' @param .png_dpi ...
#' @param .knit ...
#' @param .fonts ...
#' @return If .knit ...
#' @export
#' @examples
#' # plot_to_file(
#' #   ggplot2::ggplot(),
#' #   "test_plot",
#' #   .figure_dir=tempdir(),
#' #   .width=10,
#' #   .height=10,
#' #   .units="cm"
#' # )
plot_to_file <- function(
  .plot_obj, .plot_name=NULL, .figure_dir=".", .width=50, .height=50,
  .units="mm", .png_dpi=600, .knit=TRUE,
  .fonts=list(main=NULL, math=NULL, mono=NULL)
){

  `%0%` <- vctrs::`%0%`

  .width_in <- as.double(
    units::set_units(units::set_units(.width, .units, mode="standard"), "in")
  )
  .height_in <- as.double(
    units::set_units(units::set_units(.height, .units, mode="standard"), "in")
  )

  .options_backup <- options(
    "tikzLatex", "tikzLualatex", "tikzUnicodeMetricPackages"
  )

  options(tikzLatex=unname(which_bin("pdflatex")))
  options(tikzLualatex=unname(which_bin("lualatex")))
  options(tikzUnicodeMetricPackages="")

  if(is.null(.plot_name)){.plot_name <- deparse(substitute(.plot_obj))}
  .showtext_off <- is(try(showtext::showtext_end(), silent=TRUE), "try-error")

  stopifnot(
    ggplot2::is.ggplot(.plot_obj),
    fs::dir_exists(.figure_dir),
    .plot_name != ".",
    all(names(.fonts) %in% c("main", "math", "mono")),
    .showtext_off
  )

  .latex_packages <- stringr::str_c(
    "\\usepackage[T1]{fontenc}",
    "\\usepackage{tikz}",
    "\\IfFileExists{luatex85.sty}{\\usepackage{luatex85}}{}",
    "\\usetikzlibrary{calc}",
    "\\usepackage{fontspec}",
    "\\usepackage[active,tightpage,psfixbb]{preview}",
    "\\usepackage{microtype}",
    "\\usepackage{unicode-math}",
    purrr::imap_chr(purrr::compact(.fonts), function(..v, ..n){
      stringr::str_c("\\set", ..n, "font{", ..v, "}")
    }) %0% "",
    "\\PreviewEnvironment{pgfpicture}",
    "\\setlength\\PreviewBorder{0pt}",
    "",
    sep="\n"
  )

  .figure_path_noext <- fs::path(.figure_dir, .plot_name)
  .figure_path_tex <- fs::file_temp(ext="tex")
  # .figure_path_tex <- fs::path_ext_set(.figure_path_noext, "tex")
  .figure_path_pdf <- fs::path_ext_set(.figure_path_noext, "pdf")
  .figure_path_png <- fs::path_ext_set(.figure_path_noext, "png")

  tikzDevice::tikz(
    file=.figure_path_tex, width=.width_in, height=.height_in, engine="luatex",
    packages=.latex_packages,
    standAlone=TRUE, lwdUnit=72.27/96
  )

  print(.plot_obj)

  dev.off()

  fs::file_move(tinytex::lualatex(.figure_path_tex), .figure_path_pdf)

  sink(nullfile())
  tryCatch(
    suppressWarnings(pdftools::pdf_convert(
      pdf=.figure_path_pdf, filenames=.figure_path_png,
      format="png", pages=1, dpi=.png_dpi
    )),
    finally={sink()}
  )

  options(.options_backup)

  if(!isTRUE(.knit)){
    if(interactive()){magick::image_read(.figure_path_png)}
    return(.figure_path_png)
  }else{
    return(knitr::include_graphics(.figure_path_png, rel_path=FALSE))
  }

}

#'
#'
#'
#'
str_escape_tex <- function(.str){
  rlang::set_names(
    x=stringr::str_replace_all(.str, "[_%$#]", "\\\\\\0"), nm=names(.str)
  )
}

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
    "luatex85", "microtype", "ms", "oberdiek", "pdftexcmds", "pgf", "preview",
    "unicode-data", "unicode-math", "xcolor", "xunicode"
  )

  # optional
  .tex_pkg_deps <- c(.tex_pkg_deps, "tex-gyre")

  tinytex::tlmgr_install(c(
    .tex_pkg_deps[!tinytex::check_installed(.tex_pkg_deps)]
  ))

}
