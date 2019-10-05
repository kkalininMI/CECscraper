#' @title PageLinkExtractor function
#' @description This function extracts the links and urls from the page.
#' @param x the list of urls or MenuListExtractor object.
#' @param item  link/expression that contains the url of interest.
#' @param select select id of a single match among several identified by the algorithm.
#' @export
#' @import dplyr
#' @return Returns the data.frame object containing levels, links and urls.
#' @examples
#' library(CECscraper)
#'
#' murl<-"https://tinyurl.com/yy6roo3g"
#' oiks<-MenuLinkExtractor(murl)
#' rayons<-MenuLinkExtractor(MenuLinkExtractor(murl))
#' #rayons<-MenuLinkExtractor(murl)%>%MenuLinkExtractor()
#' uiks<-MenuLinkExtractor(MenuLinkExtractor(MenuLinkExtractor(murl))[1:5,])
#'
#' uiks_turnout<-PageLinkExtractor(uiks, "Dannyye ob otkrytii pomeshcheniy dlya golosovaniya")
#' uiks_voting<-PageLinkExtractor(uiks, "Rezul'taty vyborov|vyborov po odnomandatnomu \\(mnogomandatnomu\\) okrugu")

PageLinkExtractor<-function(x, item, select=1){
  if (is.character(x) & length(x)==1){
    links<-data.frame(scrapregion(x), scrapmenupage(x))%>%filter(!is.na(url))
    links$link<-Transliterate(links$link)
    nlink<-links[grepl(item,  links$link),][select,]
  }

  if (is.character(x) & length(x)>1){
    list.links<-lapply(1:length(x), function(iter) {
      k<-data.frame(scrapregion(x[iter]), scrapmenupage(x[iter]))%>%filter(!is.na(url))
      cat("scraping page N", iter, "\n")
      return(k)})
    list.links.sel<-lapply(list.links, function(x) x[grepl(item,  Transliterate(x$link)),][select,])
    links <- do.call(rbind,list.links.sel)
    links$link<-Transliterate(links$link)
    nlink=as.data.frame(links, stringsAsFactors = FALSE)
    row.names(nlink) <- NULL
  }


  if (is.data.frame(x)){
    list.links<-lapply(1:dim(x)[1], function(iter) {
      k<-as.data.frame(scrapmenupage(x$url[iter]))%>%filter(!is.na(url))
      cat("scraping page N", iter, "\n")
      return(k)})
    list.links.sel<-lapply(list.links, function(x) x[grepl(item,  Transliterate(x$link)),][select,])
    links <- do.call(rbind,list.links.sel)

    if(dim(links)[1]==0){
      stop('Item is not properly specified. The number of found matches is zero.')
    }

    level <- grepl("level|link",names(x))
    max_level <- suppressWarnings(max(as.numeric(unlist(regmatches(names(x)[level], gregexpr("[[:digit:]]+", names(x)[level]))))))
    colnames(x)[colnames(x) == 'link'] <- paste0("level", max_level+1)
    nlink=data.frame(x[,level], links)%>%apply(2, function(x) as.character(x))%>%as.data.frame(stringsAsFactors = FALSE)
    nlink$link<-Transliterate(links$link)
    row.names(nlink) <- NULL
  }
  on.exit(closeAllConnections())
  invisible(gc())
  return(nlink)}
