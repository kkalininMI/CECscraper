#' @title ScrapeCandidates function
#' @description This function extracts the candidate-related data information from the webpages.
#' @param x url, list of urls
#' @param savetodir save html downloaded data files in directory, i.e. "C:/Documents"
#' @export
#' @import dplyr
#' @return Returns the list with data.frame object and retrieval date.
#' @examples
#' library(CECscraper)
#'
#' url.moscow.candidates="https://tinyurl.com/yyhtfnwg"
#' moscow.candidates<-ScrapeCandidates(url.moscow.candidates)



ScrapeCandidates<-function(x, savetodir=""){
  assign("filecounter", 1 , envir = .GlobalEnv)

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
    t3<-lapply(tbls, function(x) any(apply(x, 2, function(y) grepl("FIO", Transliterate(y)))))
    tbl <- tryCatch(tbls[[which(t1>.9 & t2>10)]], error = function(e) e)
    if(inherits(tbl,  "error")){
      tbl <- tryCatch(tbls[[which(unlist(t3))[length(which(unlist(t3)))]]], error = function(e) e)
      if(inherits(tbl,  "error")){
        tbl <- tryCatch(tbls[[length(tbls)]], error = function(e) e)
        if(inherits(tbl,  "error")){return("error")}
      }}
    transliterated_tab<-apply(tbl, 2, Transliterate)
    return(transliterated_tab)}

  scrapcand<-function(x){
    list_all_candidates<-NULL
    list_all_candidates2.1<-NULL
    table1 <- scraptable(x, savetodir)
    print ("Extracting data...")
    links<-scrapmenupage(x)
    links<-links[apply(links[,2], 2, function(x) nchar(x)>150),]
    links_candidates<-links[!grepl("[[:digit:]]+", unlist(links[,1])),]
    links_pages<-links[grepl("[[:digit:]]+", unlist(links[,1])),]

    if(nrow(links_pages)!=0){
      list_all_candidates <- do.call(rbind,
                                     do.call(c, list(list(table1),
                                                     lapply(links_pages$url, function(x) scraptable(x, savetodir)))))
      colnames(list_all_candidates)<-list_all_candidates[1,]
      list_all_candidates<-list_all_candidates[grepl("[[:digit:]]+", list_all_candidates[,1]),]
      list_all_candidates<-data.frame(list_all_candidates)
      list_all_candidates$ind<-paste(list_all_candidates[,2], list_all_candidates[,3], sep="_")

      print ("Extracting data...")

      list_all_lists_candidates <- do.call(rbind,
                                           do.call(c, list(list(links_candidates),
                                                           lapply(links_pages$url, function(x){
                                                             links<-scrapmenupage(x)
                                                             links<-links[apply(links[,2], 2, function(x) nchar(x)>150),]
                                                             return(links[!grepl("[[:digit:]]+", unlist(links[,1])),])
                                                           }))))
    }else{list_all_lists_candidates<-links_candidates}


    list_all_candidates2 <- lapply(list_all_lists_candidates$url, function(x) scraptable(x, savetodir))
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

  if (is.character(x) & length(x)==1){
    tab_result<-scrapcand(x)
  }

  if (is.character(x) & length(x)>1){
    tab_result<-lapply(1:length(x), function(iter) {scrapcand(x[iter])})
  }

  return_result <- list(data=tab_result, retreivaldate=Sys.time())
  return(return_result)}
