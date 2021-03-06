#' @title scrapeCandidates function
#' @description This function extracts the candidate-related data from the webpages.
#' @param x url, list of urls.
#' @param tabextract select the table number to extract in order to override the table selection algorithm.
#' @param savetodir save downloaded html files to directory, i.e. "C:/Documents".
#' @param messages display progress messages (TRUE).
#' @export
#' @import dplyr
#' @return Returns the list containing the data.frame object and retrieval date.
#' @examples
#' library(CECscraper)
#'
#' url.moscow.candidates="http://notelections.online/region/region/moscow_city?action=show&root=1&tvd=27720002327740&vrn=27720002327736&region=77&global=null&sub_region=0&prver=0&pronetvd=null&vibid=27720002327736&type=220"
#' moscow.candidates<-scrapeCandidates(url.moscow.candidates)

scrapeCandidates <- function(x, tabextract = NULL, savetodir = "", messages = TRUE){

  if(isTRUE(messages)){
    cat("\n\nStarting scrapeCandidates()...\n\n")}

  assign("filecounter", 1 , envir = .GlobalEnv)

  if(is.data.frame(x)){
    httpadd<-substring(x$url[1], 1,regexpr("(?<=[[:alpha:]])/", x$url[1], perl=TRUE)[1]-1)
    }else if(is.list(x)){
    httpadd<-substring(x$pipe.table$url[1], 1,regexpr("(?<=[[:alpha:]])/", x$pipe.table$url[1], perl=TRUE)[1]-1)
    }else{
    httpadd<-substring(x[1], 1,regexpr("(?<=[[:alpha:]])/", x[1], perl=TRUE)[1]-1)
    }

  regreg <- function(x){
    gsub("/region/region/", "/region/", x)
  }

  scraptable <- function(url, savetodir){
    webpage <- scraperf(url)
    if (savetodir!=""){
      file_name=paste0(savetodir, "/file", "_", filecounter, ".html", sep="")
      write_xml(webpage, file = file_name, encoding = "UTF-8" )
      assign("filecounter", filecounter+1 , envir = .GlobalEnv)
    }
    tbls <- webpage %>% html_nodes(xpath="//table")%>%html_table(fill = TRUE)

    t1<-unlist(lapply(tbls, function(x) sum(!is.na(x))/(dim(x)[1]*dim(x)[2])))
    t2<-unlist(lapply(tbls, function(x) dim(x)[1]))
    t3<-lapply(tbls, function(x) any(apply(x, 2, function(y) grepl("FIO", transliterate(y)))))

    if(!is.null(tabextract)){tabnum <- tabextract
    }else{
      tabnum <- which(t1>.9 & t2>10)}

    tbl <- tryCatch(tbls[[tabnum]], error = function(e) e)
    if(inherits(tbl,  "error")){
      tbl <- tryCatch(tbls[[which(unlist(t3))[length(which(unlist(t3)))]]], error = function(e) e)
      if(inherits(tbl,  "error")){
        tbl <- tryCatch(tbls[[length(tbls)]], error = function(e) e)
        if(inherits(tbl,  "error")){return("error")}
      }}
    transliterated_tab<-apply(tbl, 2, transliterate)
    return(transliterated_tab)}

  scrapcand<-function(x){
    list_all_candidates<-NULL
    list_all_candidates2.1<-NULL

    table1 <- scraptable(x, savetodir)
    print ("Extracting data...")
    links<-scrapmenupage(x)
    #links<-links[apply(links[,2], 2, function(x) nchar(x)>150),]
    links<-links[grepl("^/region/region", links$url),]

    links_candidates<-links[!grepl("[[:digit:]]+", unlist(links[,1])),]
    links_candidates$url<-regreg(paste(httpadd, links_candidates$url, sep=""))

    links_pages<-links[grepl("[[:digit:]]+", unlist(links[,1])),]

    if(nrow(links_pages)!=0){

      links_pages$url <- regreg(paste(httpadd, links_pages$url, sep=""))

      list_all_candidates <- do.call(rbind,
                                     do.call(c, list(list(table1),
                                                     lapply(links_pages$url, function(x) scraptable(x, savetodir)))))
      colnames(list_all_candidates)<-list_all_candidates[1,]
      list_all_candidates<-list_all_candidates[grepl("[[:digit:]]+", list_all_candidates[,1]),]
      list_all_candidates<-data.frame(list_all_candidates)
      list_all_candidates$ind<-paste(list_all_candidates[,2], list_all_candidates[,3], sep="_")

      links_candidates$url <- regreg(paste(httpadd, links_candidates$url, sep=""))

      print ("Extracting data...")

      links_candidates <- links_candidates[grepl("^[[:upper:]]",transliterate(links_candidates$link)),]

      #links_candidates$url <- gsub("/region/region/", "/region/", links_candidates$url)
      #links_pages$url <- gsub("/region/region/", "/region/", links_pages$url)

      list_all_lists_candidates <- do.call(rbind,
                                           do.call(c, list(list(links_candidates),
                                                           lapply(links_pages$url, function(x){
                                                             links<-scrapmenupage(x)
                                                             links<-links[apply(links[,2], 2, function(x) nchar(x)>150),]
                                                             links$url <- regreg(paste(httpadd, links$url, sep=""))
                                                             return(links[!grepl("[[:digit:]]+", unlist(links[,1])),])
                                                           }))))
    }else{list_all_lists_candidates<-links_candidates}

    list_all_lists_candidates<-list_all_lists_candidates[grepl("^[[:upper:]]",transliterate(list_all_lists_candidates$link)),]

    list_all_candidates2 <- lapply(list_all_lists_candidates$url, function(x){print(x);scraptable(x, savetodir)})
    list_all_candidates2[unlist(lapply(list_all_candidates2, function(x) is.null(dim(x))))]<-NULL
    list_all_candidates2.1 <- lapply(list_all_candidates2, function(x) x[,dim(list_all_candidates2[[1]])[2]])
    list_all_candidates2.1 <- data.frame(do.call(rbind, list_all_candidates2.1))
    colnames(list_all_candidates2.1)<-list_all_candidates2[[1]][,2]
    list_all_candidates2.1$ind<-paste(list_all_candidates2.1[,2], list_all_candidates2.1[,3], sep="_")

    if(!is.null(list_all_candidates)){
      tab_result <- merge(list_all_candidates, list_all_candidates2.1, by="ind", all=TRUE)
    }else{
      tab_result <- list_all_candidates2.1
    }
    return(tab_result)}

    if(is.list(x) & "elections"%in%names(x)){
      x_names <- apply(x$pipe.table[colnames(x$pipe.table)[grepl("level|link", colnames(x$pipe.table))]], 1, function(x) paste(x,  collapse=", "))
      x <- unlist(lapply(1:length(x$elections$extracted.res), function(o){
        x$elections$extracted.res[[o]]$url}))
      }


  if (is.character(x) & length(x)==1){
    tab_result<-scrapcand(x)
    }


  if (is.character(x) & length(x)>1){
    tab_result<-lapply(1:length(x), function(iter){
      scrapcand(x[iter])})
    names(tab_result) <- x_names
  }

  return_result <- list(data=tab_result, retreivaldate=Sys.time())
  return(return_result)}
