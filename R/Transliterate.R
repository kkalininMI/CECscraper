#' @title Transliteration function.
#' @description This function transliterates expression from Cyrillic to Latin.
#' @export
#' @param v expression
#' @return Returns transliterated expression.
#' @examples
#' library(CECscraper)
#'
#' Transliterate("enter cyrillic text here")

Transliterate<-function(v){
  stri_trans_general(
    stri_trans_general(v, "russian-latin/bgn"),"Latin-ASCII")}
