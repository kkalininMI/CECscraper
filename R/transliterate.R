#' @title Transliteration function.
#' @description This function transliterates expression from Cyrillic to Latin.
#' @export
#' @param v expression
#' @import stringi
#' @return Returns transliterated expression.
#' @examples
#' library(CECscraper)
#'
#' transliterate("enter cyrillic text here")

transliterate<-function(v){
                      gsub("'", "`",
                        gsub("\"", "'",
                           gsub("\u00B7|\\(|\\)", "",
                                stri_trans_general(
                                  stri_trans_general(v, "russian-latin/bgn"),"Latin-ASCII"))))
                          }
