StatEccdf <- local({

  .StatEccdf <- ggplot2::ggproto(
    `_class`="StatEccdf",
    `_inherit`=ggplot2:::StatEcdf,
    default_aes=ggplot2::aes(
      x=ggplot2::after_stat(eccdf), y=ggplot2::after_stat(eccdf)
    ),
    compute_group=function(data, scales, n=NULL, pad=FALSE, flipped_aes=FALSE){
      data <- ggplot2::flip_data(data, flipped_aes)
      if(is.null(n)){
        x <- ggplot2:::unique0(data$x)
      }else{
        x <- seq(min(data$x), max(data$x), length.out=n)
      }
      if(pad){
        x <- c(-Inf, x, Inf)
      }
      data_eccdf <- 1 - ecdf(data$x)(x)
      ####
      x <- c(min(x), x)
      data_eccdf <- c(1, data_eccdf)
      ####
      df_eccdf <- ggplot2:::data_frame0(x=x, eccdf=data_eccdf, .size=length(x))
      df_eccdf$flipped_aes <- flipped_aes
      ggplot2::flip_data(df_eccdf, flipped_aes)
    }
  )

  class(.StatEccdf) <- class(.StatEccdf)[class(.StatEccdf) != "StatEcdf"]

  return(.StatEccdf)

})

#' stat_eccdf
#'
#' @param mapping ...
#' @param data ...
#' @param geom ...
#' @param position ...
#' @param ... ...
#' @param n ...
#' @param pad ...
#' @param na.rm ...
#' @param show.legend ...
#' @param inherit.aes ...
#'
#' @returns ...
#' @export
#'
#' @examples
#' NULL
stat_eccdf <- function (
  mapping=NULL, data=NULL, geom="step", position="identity", ...,
  n=NULL, pad=FALSE, na.rm=FALSE, show.legend=NA, inherit.aes=TRUE
){
  ggplot2::layer(
    data=data, mapping=mapping, stat=StatEccdf, geom=geom, position=position,
    show.legend=show.legend, inherit.aes=inherit.aes,
    params=rlang::list2(n=n, pad=pad, na.rm=na.rm, ...)
  )
}
