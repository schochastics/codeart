#' @title Plot Random Code Art
#' @param df result from running [codeart_random]
#' @param seg_size width of code segments
#' @param bg_col background color
#' @author David Schoch
#' @export

plot_codeart_rand <- function(df,seg_size=3,bg_col="grey25"){
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("The package 'ggplot2' is needed for this function.")
  }
  ggplot2::ggplot(df$data)+
    ggplot2::geom_segment(ggplot2::aes_(x=~x,y=~y,xend=~xend,yend=~yend),
                          col=df$data$col,size=seg_size,lineend = "round")+
    ggplot2::theme_void()+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill=bg_col))+
    ggplot2::scale_x_continuous(limits=c(0,df$params$w))+
    ggplot2::scale_y_reverse(limits=c(df$params$h,0))
}
