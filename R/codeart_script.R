#' @title Create Code Art from R script
#' @description Create Code Art from R script
#' @author David Schoch
#' @details Inspired by [Eric Davidson](https://github.com/erdavids/Simulated-Code).
#' @export

codeart_script <- function(path,char_size=5,segment_sep=20,indent_size=50,cols){
  script <- readLines(path)

  parsed_script <- utils::getParseData((parse(text=script))) %>%
  parse_script <- parse_script[parse_script["terminal"],]

  del_lines <- c("EQ","EQ_SUB","GT","LT","EQ_ASSIGN","FUNCTION","NULL_CONST",
                 "SYMBOL_FORMALS","LBB","NEXT","{","}","GE","EQ_FORMALS","SPECIAL","SYMBOL_SUB")
  paramblock <- "\\s*SYMBOL_SUB EQ_SUB (SYMBOL|SYMBOL_FUNCTION_CALL NUM_CONST|NUM_CONST|STR_CONST) "

  df <- parsed_script
  df[["token"]][df[["text"]]=="%>%"] <- "PIPE"



  df <- parsed_script %>%
    # dplyr::filter(text!="%in%") %>%
    group_by(line1) %>%
    dplyr::summarise(line=paste0(token,collapse=" "),chars=sum(nchar(text)),
                     indent=min(col1)) %>%
    mutate(line=str_remove_all(line,"'")) %>%
    mutate(line=str_remove_all(line,"\\+|\\-|/|\\*|\\(|\\)|\\]|\\[|\\$|~|AND |NE |! |: ")) %>%
    mutate(line=str_remove_all(line,"NS_GET |SYMBOL_PACKAGE ")) %>%
    mutate(line=str_replace_all(line,"\\s+"," ")) %>%
    mutate(line=str_replace_all(line,"^\\}$"," ")) %>%
    mutate(line=str_replace_all(line,"NUM_CONST : NUM_CONST","NUM_CONST")) %>%
    mutate(line=if_else(str_detect(line,"^FOR"),"FOR FORBLOCK",line)) %>%
    mutate(line=if_else(str_detect(line,"^IF"),"IF IFBLOCK",line)) %>%
    mutate(line=if_else(str_detect(line,"^ELSE IF"),"IF IFBLOCK",line)) %>%
    mutate(line=str_replace_all(line,paramblock," PARAMBLOCK ")) %>%
    mutate(line=str_replace_all(line," SYMBOL_SUB EQ_SUB "," SYMBOL ")) %>%
    mutate(line=if_else(str_detect(line,"^SYMBOL LEFT_ASSIGN") &!str_detect(line,"SYMBOL_FUNCTION_CALL"),"SYMBOL ASSIGNBLOCK",line)) %>%
    mutate(line=str_remove_all(line,"LEFT_ASSIGN |,\\s*")) %>%
    mutate(line=str_replace_all(line,"\\b(\\w+)(\\b\\W+\\b\\1\\b)*","\\1")) %>%
    mutate(line=str_squish(line)) %>%
    # print(n=64)
    mutate(lines=str_split(line," ")) %>%
    unnest(lines) %>%
    dplyr::filter(!lines%in%del_lines) %>%
    mutate(col=cols[.$lines]) %>%
    group_by(line1) %>%
    mutate(numsym=n()) %>%
    ungroup() %>%
    mutate(x=if_else(numsym==1,50+(indent-1)*char_size,NA_real_)) %>%
    group_by(line1) %>%
    mutate(x=if_else(numsym!=1 & !duplicated(line1),50+(indent-1)*char_size,x)) %>%
    ungroup() %>%
    mutate(xend=ifelse(!is.na(x),case_when(!lines%in%c("IF","FOR")~x+chars*char_size/numsym+sample(-2*char_size:0,1),
                                           TRUE~x+char_size*10),NA_real_))
}
