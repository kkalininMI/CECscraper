#' @title listURLextractor function
#' @description This function extracts links and urls from the lists/menus.
#' @param x link, list of links or data frame with links
#' @param messages display progress messages (TRUE).
#' @export
#' @import dplyr
#' @return Returns the dataframe object with urls, links.
#' @examples
#' #Example
#' library(CECscraper)
#'
#' murl <- "http://notelections.online/region/izbirkom?action=show&vrn=27720002327736&region=77&prver=0&pronetvd=null"
#' oiks <- listURLextractor(murl)
#' rayons <- listURLextractor(listURLextractor(murl))
#' uiks <- listURLextractor(
#'             rowURLextractor(
#'                listURLextractor(
#'                    listURLextractor(murl))[1:5,], 'sayt izbiratel`noy komissii sub`yekta'))


listURLextractor<-function(x, messages = TRUE){

  if(isTRUE(messages)){
    cat("\n\nStarting listURLextractor()...\n\n")}

  if(is.data.frame(x)){
    httpadd<-substring(x$url[which.max(nchar(x$url))[1]], 1,regexpr("(?<=[[:alpha:]])/", x$url[which.max(nchar(x$url))[1]], perl=TRUE)[1]-1)
  }else{
    httpadd<-substring(x[1], 1,regexpr("(?<=[[:alpha:]])/", x[1], perl=TRUE)[1]-1)
  }

  regreg <- function(x){
    gsub("/region/region/", "/region/", x)
  }


  if("webscrape" %in% colnames(x)) {x <- x[x$webscrape,]}

  scrapFunction <- function (iter, a) {
    cat("scraping page N", iter, "\n")
    if(a==1){k<-tryCatch(data.frame(x[iter, level], oldli[iter],
                                    as.data.frame(scrapmenu(x$url[iter]))%>%filter(!is.na(url)), row.names = NULL), error = function(e) e)}
    if(a==2){k<-tryCatch(data.frame(oldli[iter],
                                    as.data.frame(scrapmenu(x$url[iter]))%>%filter(!is.na(url)), row.names = NULL), error = function(e) e)}

    if(inherits(k,  "error")) {warning("Menu is non-scrapable or missing from the webpage")}
    return(k)}


  if (is.character(x) & length(x)==1){
    links<-as.data.frame(scrapmenu(x))%>%filter(!is.na(url))
    links$link<-transliterate(links$link)
  }

  if (is.character(x) & length(x)>1){
    list.links<-lapply(1:length(x), function(iter) {level1<-scrapregion(x[iter])
    k<-data.frame(level1, as.data.frame(scrapmenu(x[iter])))%>%filter(!is.na(url))

    if(isTRUE(messages)){
    cat("scraping page N", iter, "\n")
    }

    return(k)})
    links <- do.call(rbind,list.links)
    links$link<-transliterate(links$link)
    names(links)[!grepl("url|link",names(links))] <- paste0("level",seq(1, length(names(links)[!grepl("url|link",names(links))]),1))
  }

  if (is.data.frame(x)){
    level <- grepl("level",names(x))
    oldli <- x$link
    if(any(level)){
      list.links<-lapply(1:dim(x)[1], scrapFunction, a=1)
      list.links<-list.links[!unlist(lapply(list.links, function(x) class(x)[1]=="simpleError"))]
    }else{
      list.links<-lapply(1:dim(x)[1], scrapFunction, a=2)
      list.links<-list.links[!unlist(lapply(list.links, function(x) class(x)[1]=="simpleError"))]
    }
    links <- do.call(rbind,list.links)
    links$link<-transliterate(links$link)
    names(links)[!grepl("url|link",names(links))] <- paste0("level", seq(1, length(names(links)[!grepl("url|link",names(links))]),1))
  }
  links=apply(links, 2, function(x) as.character(x))%>%as.data.frame(stringsAsFactors = FALSE)
  row.names(links) <- NULL

  links <- links[, order(names(links))]

  #if(relativepath){
  # links$url <- paste(substring(x$url, 1,regexpr("(?<=[[:alpha:]])/", root_url, perl=TRUE)[1]-1), links$url, sep="")}


  #if(!grepl("http://", links$url[1])){
  #    links$url <- paste(substring(x$url[1], 1,regexpr("(?<=[[:alpha:]])/", x$url[1], perl=TRUE)[1]-1), links$url, sep="")}

  links$url[!grepl("http://", links$url)] <- regreg(paste(httpadd, links$url[!grepl("http://", links$url)], sep=""))


  on.exit(closeAllConnections())
  invisible(gc())


  return(links)}
