#' @title Transliteration function.
#' @description This function transliterates expression from Cyrillic to Latin.
#' @export
#' @param v expression
#' @return Returns transliterated expression.
#' @examples
#' library(CECscraper)
#'
#' transliterate("enter cyrillic text here")

transliterate<-function(v){
                     gsub("\"", "'",
                           gsub("\u00B7|\\(|\\)", "",
                                stri_trans_general(
                                  stri_trans_general(v, "russian-latin/bgn"),"Latin-ASCII")))
                          }
