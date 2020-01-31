#' @title Create Code Art from R script
#' @description Create Code Art from R script
#' @author David Schoch
#' @details Inspired by [Eric Davidson](https://github.com/erdavids/Simulated-Code).
#' @export

codeart_script <- function(path,char_size=5,segment_sep=20,indent_size=50,cols=NULL){
  if(is.null(cols)){
    col_palette <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
                     "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
                     "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
                     "#8A7C64", "#599861")

    cols <- sample(col_palette,14)

  }

  names(cols) <- c("COMMENT", "SYMBOL_FUNCTION_CALL", "SYMBOL", "FOR", "IF", "ELSE IF",
                   "ELSE", "ASSIGNBLOCK", "FORBLOCK", "IFBLOCK", "NUM_CONST", "STR_CONST",
                   "PIPE", "PARAMBLOCK")

  script <- readLines(path)

  parsed_script <- utils::getParseData((parse(text=script)))
  parsed_script <- parsed_script[parsed_script[["terminal"]],]

  del_lines <- c("EQ","EQ_SUB","GT","LT","EQ_ASSIGN","FUNCTION","NULL_CONST",
                 "SYMBOL_FORMALS","LBB","NEXT","{","}","GE","EQ_FORMALS","SPECIAL","SYMBOL_SUB")
  paramblock <- "\\s*SYMBOL_SUB EQ_SUB (SYMBOL|SYMBOL_FUNCTION_CALL NUM_CONST|NUM_CONST|STR_CONST) "

  df <- parsed_script
  df[["token"]][df[["text"]]=="%>%"] <- "PIPE"

  df_token <- stats::aggregate(df[,c("token")],by=list(df$line1),FUN=function(x) paste0(x,collapse=" "))
  df_chars <- stats::aggregate(df[,c("text")],by=list(df$line1),FUN=function(x) sum(nchar(as.character(x))))
  df_indent <- stats::aggregate(df[,c("col1")],by=list(df$line1),FUN=function(x) min(x))

  df <- data.frame(line1 = df_token[["Group.1"]],
                   line  = df_token[["x"]],
                   chars = df_chars[["x"]],
                   indent = df_indent[["x"]])

  df[["line"]] <- gsub("'","",df[["line"]])
  df[["line"]] <- gsub("\\+|\\-|/|\\*|\\(|\\)|\\]|\\[|\\$|~|AND |NE |! |: ","",df[["line"]])
  df[["line"]] <- gsub("NS_GET |SYMBOL_PACKAGE ","",df[["line"]])
  df[["line"]] <- gsub("\\s+"," ",df[["line"]])
  df[["line"]] <- gsub("^\\}$"," ",df[["line"]])
  df[["line"]] <- gsub("NUM_CONST : NUM_CONST","NUM_CONST",df[["line"]])
  df[["line"]] <- ifelse(grepl("^FOR",df[["line"]]),"FOR FORBLOCK",df[["line"]])
  df[["line"]] <- ifelse(grepl("^IF",df[["line"]]),"IF IFBLOCK",df[["line"]])
  df[["line"]] <- ifelse(grepl("^ELSE IF",df[["line"]]),"IF IFBLOCK",df[["line"]])
  df[["line"]] <- gsub(paramblock," PARAMBLOCK ",df[["line"]])
  df[["line"]] <- gsub(" SYMBOL_SUB EQ_SUB "," SYMBOL ",df[["line"]])
  df[["line"]] <- ifelse(grepl("^SYMBOL LEFT_ASSIGN",df[["line"]]) &
                           !grepl("SYMBOL_FUNCTION_CALL",df[["line"]]),
                         "SYMBOL ASSIGNBLOCK",df[["line"]])
  df[["line"]] <- gsub("LEFT_ASSIGN |,\\s*","",df[["line"]])
  df[["line"]] <- gsub("\\b(\\w+)(\\b\\W+\\b\\1\\b)*","\\1",df[["line"]])
  df[["line"]] <- gsub("\\s+"," ",df[["line"]])
  df[["line"]] <- gsub("^\\s+","",df[["line"]])
  df[["line"]] <- gsub("\\s+$","",df[["line"]])

  line_list <- strsplit(df[["line"]]," ")
  list_length <- sapply(line_list,length)
  line_list[list_length==0] <- ""
  list_length[list_length==0] <- 1

  df <- data.frame(line1  = rep(df[["line1"]],list_length),
                   lines   = as.character(unlist(line_list)),
                   chars  = rep(df[["chars"]],list_length),
                   indent = rep(df[["indent"]],list_length),stringsAsFactors = FALSE)

  df <- df[!df[["lines"]]%in%del_lines,]
  df[["col"]] <- cols[df[["lines"]]]

  tmp <- as.data.frame(table(df[["line1"]]))
  names(tmp)  <- c("line1","numsym")
  df <- merge(df,tmp,all.x = TRUE)
  df[["x"]] <- ifelse(df[["numsym"]]==1,50+(df[["indent"]]-1)*char_size,NA_real_)
  df[["x"]] <- ifelse(!duplicated(df[["line1"]]) & df[["numsym"]]!=1,50+(df[["indent"]]-1)*char_size,df[["x"]])
  df[["xend"]] <- ifelse(!is.na(df[["x"]]) & !df[["lines"]]%in%c("IF","FOR"),
                         df[["x"]]+df[["chars"]]*char_size/df[["numsym"]]+sample(-2*char_size:0,1),NA_real_)
  df[["xend"]] <- ifelse(!is.na(df[["x"]]) & df[["lines"]]%in%c("IF","FOR"),df[["x"]]+char_size*10,df[["xend"]])

  for(i in 2:nrow(df)){
    if(is.na(df[["x"]][i]) & df[["line1"]][i-1]==df[["line1"]][i]){
      df[["x"]][i] <- df[["xend"]][i-1]+segment_sep
      df[["xend"]][i] <- df[["x"]][i]+df[["chars"]][i]*char_size/df[["numsym"]][i]+sample(-2*char_size:0,1)
    }
    if(df[["lines"]][i]=="PIPE"){
      df[["x"]][i] <- df[["x"]][i]-segment_sep/2
      df[["xend"]][i] <- df[["x"]][i]
    }

  }

  #adjust param blocks ----
  for(i in 2:nrow(df)){
    if(df[["lines"]][i]=="PARAMBLOCK" & df[["lines"]][i-1]=="PARAMBLOCK" & df[["line1"]][i]!=df[["line1"]][i-1]){
      tmp <- df[["x"]][i]
      df[["x"]][i] <- df[["x"]][i-1]
      df[["xend"]][i] <- df[["xend"]][i]-(tmp-df[["x"]][i])
    }
  }

  df[["y"]] <- df[["yend"]] <- df[["line1"]]
  df
}
