#' @title Create Random Code Art
#' @description Create Random Code Art
#' @author David Schoch
#' @details Inspired by [Eric Davidson](https://github.com/erdavids/Simulated-Code).
#' @export
codeart_random <- function(w = 1200, h = 1200,
                           code_lines = 60,
                           min_segments = 8,
                           max_segments = 20,
                           min_segment_length = 15,
                           max_segment_length = 60,
                           segment_sep = 20,
                           line_break_chance = 0.4,
                           indent_size = 50,
                           max_indents = 6,
                           indent_inc_chance = 0.4,
                           indent_dec_chance = 0.3,
                           change_chance = 0.4,
                           cols=NULL){

  # adjust parameters
  code_start <-  h/60
  code_end <-  h - h/100
  code_sep  <-  (code_end - code_start)/code_lines

  line_y <-  code_start
  indent <-  0
  if(is.null(cols)){
    cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
              "#A6761D", "#666666")
  }
  no_cols <- length(cols)
  cur_col <- cols[sample(1:no_cols,1)]

  res <- data.frame(x = numeric(0),
                    y = numeric(0),
                    xend = numeric(0),
                    yend = numeric(0),
                    col = numeric(0))

  for(i in 1:code_lines){
    if(!(stats::runif(1)<line_break_chance & indent==0)){
      line_x <- indent_size + (indent * indent_size)
      line_segments <- round(stats::runif(1,min=min_segments,max=max_segments))
      for(j in 1:line_segments){
        if(stats::runif(1)<change_chance){
          cur_col <- cols[sample(1:no_cols,1)]
        }
        segment_length <- stats::runif(1,min_segment_length,max_segment_length)
        res <- rbind(res,
                     data.frame(x=line_x,y=line_y,xend=line_x+segment_length,yend=line_y,col=cur_col))
        line_x <- line_x + segment_length + segment_sep
      }
      if(stats::runif(1) < indent_inc_chance & indent<max_indents){
        indent <- indent+1
      }
      else if(stats::runif(1)< indent_dec_chance & indent>0){
        indent <- indent - round(stats::runif(1,1,max_indents))
        if(indent<0){
          indent <- 0
        }
      }
    }
    line_y <- line_y + code_sep
  }
  params <-list("w" = w,
                "h" = h,
                "code_lines" = code_lines,
                "min_segments" = min_segments,
                "max_segments" = max_segments,
                "min_segment_length" = min_segment_length,
                "max_segment_length" = max_segment_length,
                "segment_sep" = segment_sep,
                "line_break_chance" = line_break_chance,
                "indent_size" = indent_size,
                "max_indents" = max_indents,
                "indent_inc_chance" = indent_inc_chance,
                "indent_dec_chance" = indent_dec_chance,
                "change_chance" = change_chance,
                "cols"=cols)
  list(data=res,params=params)
}
