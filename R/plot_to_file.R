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
  .plot_obj, .plot_name=NULL, .figure_dir=".", .width=NULL,
  .height=NULL, .units="mm", .png_dpi=600, .knit=TRUE,
  .fonts=list(main="TeX Gyre Termes", math=NULL, mono=NULL)
){

  if(is.null(.plot_name)){.plot_name <- deparse(substitute(.plot_obj))}
  .showtext_off <- is(try(showtext::showtext_end(), silent=TRUE), "try-error")

  stopifnot(
    ggplot2::is.ggplot(.plot_obj),
    fs::dir_exists(.figure_dir),
    .plot_name != ".",
    all(names(.fonts) %in% c("main", "math", "mono")),
    .showtext_off
  )

  .tex_packages <- stringr::str_c(
    "\\usepackage{tikz}",
    "\\IfFileExists{luatex85.sty}{\\usepackage{luatex85}}{}",
    "\\usepackage[active,tightpage,psfixbb]{preview}",
    "\\usepackage{microtype}",
    "\\usepackage{fontspec}",
    "\\usepackage{unicode-math}",
    purrr::imap_chr(purrr::compact(.fonts), \(..v, ..n){
      stringr::str_c("\\set", ..n, "font{", ..v, "}")
    }),
    "\\PreviewEnvironment{pgfpicture}",
    "\\setlength\\PreviewBorder{0pt}",
    sep="\n"
  )

  .figure_path_noext <- fs::path(.figure_dir, .plot_name)
  .figure_path_tex <- fs::file_temp(ext="tex")
  .figure_path_pdf <- fs::path_ext_set(.figure_path_noext, "pdf")
  .figure_path_png <- fs::path_ext_set(.figure_path_noext, "png")

  ggplot2::ggsave(
    filename=.figure_path_tex, plot=.plot_obj, device=tikzDevice::tikz,
    width=.width, height=.height, units=.units, engine="luatex",
    standAlone=TRUE, packages=.tex_packages, lwdUnit=72.27/96
  )

  fs::file_move(tinytex::lualatex(.figure_path_tex), .figure_path_pdf)

  sink(nullfile())
  tryCatch(
    suppressWarnings(pdftools::pdf_convert(
      pdf=.figure_path_pdf, filenames=.figure_path_png,
      format="png", pages=1, dpi=.png_dpi
    )),
    finally={sink()}
  )

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
