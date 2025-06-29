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
  .units="mm", .png_dpi=600, .knit=FALSE,
  .fonts=list(main=NULL, math=NULL, mono=NULL)
){

  `%0%` <- vctrs::`%0%`

  .dims_to_in_factor <- dplyr::case_when(
    .units == "mm" ~ 1 / 25.4,
    .units == "cm" ~ 1 / 25.4 * 10,
    TRUE ~ NA_real_
  )

  .width_in <- .width * .dims_to_in_factor
  .height_in <- .height * .dims_to_in_factor

  .options_backup <- options(
    "tikzLatex", "tikzLualatex", "tikzUnicodeMetricPackages",
    "tikzMetricsDictionary"
  )

  options(
    tikzLatex=unname(which_bin("pdflatex")),
    tikzLualatex=unname(which_bin("lualatex")),
    tikzUnicodeMetricPackages="",
    # tikzMetricsDictionary=fs::path(
    #   fs::path_temp(), digest::digest(environment()), ext="tikzDict"
    # )
    tikzMetricsDictionary=fs::path(
      fs::path_temp(), "plottr-cache", ext="tikzDict"
    )
  )

  if(is.null(.plot_name)){.plot_name <- deparse(substitute(.plot_obj))}
  .showtext_off <- is(try(showtext::showtext_end(), silent=TRUE), "try-error")

  stopifnot(
    ggplot2::is.ggplot(.plot_obj),
    fs::dir_exists(.figure_dir),
    .plot_name != ".",
    all(names(.fonts) %in% c("main", "math", "mono")),
    .showtext_off
  )

  .figure_path_noext <- fs::path(.figure_dir, .plot_name)
  .figure_dir_tex <- fs::dir_create(fs::file_temp())
  .figure_path_tex <- fs::file_temp(tmp_dir=.figure_dir_tex, ext="tex")
  .figure_path_pdf <- fs::path_ext_set(.figure_path_noext, "pdf")
  .figure_path_png <- fs::path_ext_set(.figure_path_noext, "png")

  .latex_packages <- stringr::str_c(
    "\\usepackage[T1]{fontenc}",
    "\\usepackage{graphicx}",
    stringr::str_c("\\graphicspath{{", .figure_dir_tex, "}}"),
    "\\usepackage{tikz}",
    "\\usetikzlibrary{calc}",
    "\\let\\pgfimage=\\includegraphics",
    "\\usepackage{luatex85}",
    "\\usepackage{fontspec}",
    "\\usepackage[active,tightpage,psfixbb]{preview}",
    "\\usepackage{microtype}",
    "\\usepackage{unicode-math}",
    purrr::imap_chr(purrr::compact(.fonts), function(..v, ..n){
      stringr::str_c("\\set", ..n, "font{", ..v, "}")
    }) %0% "",
    "\\usepackage{siunitx}",
    "\\sisetup{",
    "  locale = US,",
    "  detect-all,",
    "  detect-weight=true,",
    "  detect-family=true,",
    "  mode=text,",
    "  group-digits=integer, ",
    "  group-separator={,},",
    "  group-minimum-digits={3}",
    "}",
    "\\PreviewEnvironment{pgfpicture}",
    "\\setlength\\PreviewBorder{0pt}",
    "",
    sep="\n"
  )

  tikzDevice::tikz(
    file=.figure_path_tex, width=.width_in, height=.height_in, engine="luatex",
    packages=.latex_packages, standAlone=TRUE, lwdUnit=72.27/96
  )

  tryCatch(expr={print(.plot_obj)}, finally={dev.off()})

  fs::file_move(
    tinytex::lualatex(file=.figure_path_tex),
    .figure_path_pdf
  )

  fs::dir_delete(.figure_dir_tex)

  sink(nullfile())
  tryCatch(
    expr={
      suppressWarnings(pdftools::pdf_convert(
        pdf=.figure_path_pdf, filenames=.figure_path_png,
        format="png", pages=1, dpi=.png_dpi
      ))
    },
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
pltr_escape_tex <- function(.str){
  .str |>
    stringr::str_split(".(?=<tex>)|(?<=</tex>)") |>
    purrr::map_chr(function(..tok_vec){
      ..tok_vec |>
        purrr::map_chr(function(...tok){
          if(stringr::str_detect(...tok, "^<tex>.*</tex>$")){
            ...tok <- stringr::str_remove_all(...tok, "^<tex>|</tex>$")
          }else{
            ...tok <- stringr::str_replace_all(...tok, "[_%$#&{}]", "\\\\\\0")
          }
          return(...tok)
        }) |>
        stringr::str_c(collapse="")
    }) |>
    rlang::set_names(nm=names(.str))
}
