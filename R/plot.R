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
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill=bg_col),
      panel.background = ggplot2::element_rect(fill=bg_col))+
    ggplot2::scale_x_continuous(limits=c(0,df$params$w))+
    ggplot2::scale_y_reverse(limits=c(df$params$h,0))
}


#' @title Plot Code Art generated from an R script
#' @param df result from running [codeart_script]
#' @param seg_size width of code segments
#' @param bg_col background color
#' @param pipe_size size of pipes
#' @param caption caption
#' @param title title
#' @param title_size size of title
#' @param caption_size size of caption
#' @param font font family
#' @author David Schoch
#' @export

plot_codeart_script <- function(df,seg_size=3,bg_col="grey25",
                                pipe_size=8,caption="",title="",
                                title_size=62,caption_size=32,font="Arial"){
  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("The package 'ggplot2' is needed for this function.")
  }
  ggplot2::ggplot()+
    ggplot2::geom_segment(data=df[df$lines!="PIPE",],
                          ggplot2::aes_(y=~y,yend=~yend,x=~x,xend=~xend),size=seg_size,
                 lineend = "round",col=df$col[df$lines!="PIPE"])+
    ggplot2::geom_point(data=df[df$lines=="PIPE",],ggplot2::aes_(x=~x,y=~y),shape="\u25B6",
               size=pipe_size,col=df$col[df$lines=="PIPE"])+
    ggplot2::scale_y_reverse(limits=c(max(df$y),0))+
    ggplot2::theme_void()+
    ggplot2::labs(title=title,caption=caption)+
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill=bg_col),
      panel.background = ggplot2::element_rect(fill=bg_col),
      plot.title = ggplot2::element_text(colour="white",family=font,size=title_size,hjust=1),
      plot.caption = ggplot2::element_text(colour="white",family=font,size=caption_size))
}
