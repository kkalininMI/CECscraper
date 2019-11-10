# @title scrapmenu internal function
# @description This function will extract links from the menu of links
# @param url url link
#' @import rvest
#' @import dplyr
#' @import stringi
#' @import xml2
# @return Returns the dataframe object with urls and links.
filecounter=1

scraperf <- function(url){
  webpage <- tryCatch(xml2::read_html(url, encoding="windows-1251"), error = function(e) e)
  if(inherits(webpage,  "error")){
    Sys.sleep(sample(seq(1, 5, by=0.001), 1))
    webpage <- tryCatch(xml2::read_html(url, encoding="windows-1251"), error = function(e) e)
    if(inherits(webpage,  "error")) return ("error")
  }
  return(webpage)}

scrapmenu <- function(url){
  webpage <- scraperf(url)
  url_ <- webpage %>%
    rvest::html_nodes("option") %>%
    rvest::html_attr("value")
  link_ <- webpage %>%
    rvest::html_nodes("option") %>%
    rvest::html_text()%>%
    { gsub('^\\s+|\\s+$','', .) }
  return(tibble(link = link_, url = url_))
}

scrapregion <- function(url){
  webpage <- scraperf(url)
  urlR <- webpage %>%html_nodes("a") %>% html_text( "href")%>%
    stri_trans_general("russian-latin/bgn")%>%
    { gsub('^\\s+|\\s+$','', .) }
  gr_expr="Respublik|respublik|oblast|Oblast|Kray|kray|Sankt|sankt|Mosk|mosk|Sevas|sevas"
  reg<-tibble(level1=urlR[grepl(gr_expr, urlR)])
  if(is.data.frame(reg) && nrow(reg)==0){reg="None"}
  return(reg)}


scrapmenupage <- function(url){
  webpage <- scraperf(url)
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()%>%
    { gsub('^\\s+|\\s+$','', .) }
  return(tibble(link = link_, url = url_))
}


scrappage <- function(x, ttime, dnames, savetodir, tabextract){

  webpage <- tryCatch(scraperf(x$url), error = function(e) e)
  if(inherits(webpage,  "error")){return("error")}


  if (savetodir!=""){
    file_name=paste0(savetodir, "/file", "_", filecounter, ".html", sep="")
    write_xml(webpage, file = file_name, encoding = "UTF-8" )
    assign("filecounter", filecounter+1 , envir = .GlobalEnv)
  }

  #Data Info
  data_info <- webpage %>% html_nodes("[class='w2']")%>%html_text(trim = TRUE)%>%Transliterate()
  data_info <- data_info[!data_info==""]
  if(length(data_info)>2){data_info <- data_info[1:2]}

  #Voting Info
  tab_key <- "Data golosovaniya"
  tbln <- webpage %>% html_nodes(xpath="//table")
  tbln_tk <- which(grepl(tab_key, Transliterate(as.character(tbln))))
  num_tbln_tk <- tbln_tk[length(tbln_tk)]
  tbls<- tryCatch(tbln[c(num_tbln_tk+1):length(tbln)]%>%html_table(fill = TRUE), error = function(e) e)
  if(inherits(tbls,  "error")){return("error")}



  if(ttime==FALSE){
    tab_key <- "Chislo izbiratel|Chislo byulleteney"
    t1<-unlist(lapply(tbls, function(x) sum(!is.na(x))/(dim(x)[1]*dim(x)[2])))
    t2<-unlist(lapply(tbls, function(x) dim(x)[1]))
    t3<-unlist(lapply(tbls, function(x) any(grepl(tab_key, Transliterate(as.character(x))))))

    if(!is.null(tabextract)){tabnum=tabextract}else{tabnum=which(t1>.2 & t2>10 & t3)}

    tbl <- tryCatch(tbls[[tabnum]], error = function(e) e)
    if(inherits(tbl,  "error")){
      tbl <- tryCatch(tbls[[t3]], error = function(e) e)
      if(inherits(tbl,  "error")){
        tbl <- tryCatch(tbls[[which(t1>.2 & t2>10)]], error = function(e) e)
        if(inherits(tbl,  "error")){return("error")}
      }
    }
    res<-suppressWarnings(as.numeric(gsub('\r.*','\\1',tbl[,3])))
  }else{
    ind = suppressWarnings(which(grepl("^[[:digit:]]+$", as.numeric(t(tbls[[2]])[,2]))))
    if(length(ind)==0){ind = suppressWarnings(which(grepl("^[[:digit:]]+$", as.numeric(t(tbls[[3]])[,2]))))}
    res <- tryCatch(c(tbls[[length(tbls)]][ind][3,]), error = function(e) e)
    res <- res[unlist(lapply(res, function(x) grepl("\\%", x)))]
    if(inherits(res,  "error")) return ("error")

    res <- suppressWarnings(as.numeric(gsub("%", "", res)))
    res[is.na(res)]<-"None"
    invisible(gc())
  }

  #working with names
  if(ttime==FALSE){
    if(dnames){
      res_names<-Transliterate(tbl[,2])
      res_names<-res_names[!res_names==""]
    }else{
      which(tbl[,2]=="")
      namesC<-paste0("C", 1:(which(tbl[,2]=="")-1), sep="")
      namesP<-paste0("P", 1:(length(tbl[,2])-(which(tbl[,2]==""))), sep="")
      res_names=c(namesC, namesP)
    }}else{
      res_names<-paste0("T",1:4)
    }

  #vector names
  if(any(is.na(res))) res=res[!is.na(res)]
  if(length(data_info)>0){data_info_names=paste0("info", seq(1, length(data_info)))}else{data_info_names<-NULL}
  names_x<-names(x)

  #reformatting
  x<- data.frame(lapply(x, as.character), stringsAsFactors=FALSE)
  names_vector<-c(data_info_names, names_x, res_names)

  res_return<-data.frame(t(c(data_info, unname(unlist(x)), res)))
  names_vector <- gsub("^\\s+|\\s+$","", names_vector)
  colnames(res_return)<-names_vector
  return(res_return)}


scrappage_fast <- function(x, ttime, dnames, savetodir, tabextract){
  if(is.character(x) & length(x)==1){
    webpage <- scraperf(x)
  }else{
    webpage <- scraperf(x$url)
  }

  if (savetodir!=""){
    file_name=paste0(savetodir, "/file", "_", filecounter, ".html", sep="")
    write_xml(webpage, file = file_name, encoding = "UTF-8" )
    assign("filecounter", filecounter+1 , envir = .GlobalEnv)
  }


  #Data Info
  data_info <- webpage %>% html_nodes(xpath="//table")%>%html_nodes("[class='w2']")%>%html_text(trim = TRUE)%>%Transliterate()
  data_info <- data_info[!data_info==""]
  if(length(data_info)>2){data_info <- data_info[1:2]}
  if(length(data_info)>0){data_info_names=paste0("info", seq(1, length(data_info)))}else{data_info_names<-NULL}

  #Voting Info
  tbln <- webpage %>% html_nodes(xpath="//table")

  if(ttime==FALSE){
    if(!is.null(tabextract)){tabnum=tabextract
    }else{
      tabnum <- which(unlist(lapply(tbln, function(x) grepl("overflow:scroll",x))))
      tabnum <- tabnum[length(tabnum)]
      tabnum=tabextract}
    list_results<-sapply(sapply(tbln[tabnum],
                                function(x) {x %>% html_nodes("tr")}),
                         function(x) {x%>%html_nodes("nobr")%>% html_text()})

    col_names<-unlist(lapply(sapply(sapply(tbln[tabnum-1],
                                           function(x) {x %>% html_nodes("tr")}),
                                    function(x) {x%>%html_nodes("nobr")%>% html_text()}),
                             function(x) {x[2]}))
    na_pos <- which(lapply(list_results, function(x) length(x))==0)
    list_results[[na_pos]]<-rep(NA,length(list_results[[1]]))

    tbl <- t(do.call(rbind,list_results))
    if(is.vector(tbl)){tbl<-as.data.frame(tbl, row.names = NULL); colnames<-"C"}
    tbl <- tbl[,-na_pos]
    if(is.vector(tbl)){tbl[1]<-Transliterate(as.character(tbl[1]))}else{
      tbl[,1] <- Transliterate(as.character(tbl[,1]))
    }

    if(dnames){
      res_names<-Transliterate(col_names)
      res_names<-res_names[!(res_names==""|is.na(res_names))]
    }else{
      namesC<-paste0("C", 1:(na_pos-2), sep="")
      namesP<-paste0("P", 1:(length(list_results)-na_pos), sep="")
      res_names=c(namesC, namesP)
    }

    x <- data.frame(lapply(x, as.character), stringsAsFactors=FALSE)

    if(is.vector(tbl)){
      res_return<-data.frame(cbind(data.frame(rbind(data_info, row.names = NULL)), x, t(tbl), row.names = NULL))}else{
        res_return<-data.frame(cbind(data.frame(rbind(data_info, row.names = NULL)), x, tbl, row.names = NULL))}

    level <- grepl("level|link|url",names(x))
    names_x <- names(x)[level]
    max_level <- suppressWarnings(max(as.numeric(unlist(regmatches(names_x, gregexpr("[[:digit:]]+", names_x))))))
    names_x[names_x=="link"]<-c(paste0("level",max_level+1,""))
    res_names=c("link", res_names)
    names_vector<-c(data_info_names, names_x, res_names)
    colnames(res_return)<-names_vector

  }else{
    if(!is.null(tabextract)){tabnum=tabextract}else{tabnum=length(tbln)}
    tbls<-tbln[tabnum]
    tbl<-tbls%>%html_table(fill = TRUE)
    tbl<-apply(tbl[[1]],2, function(x) gsub("\\%", "", x))
    tbl <- tbl[,-1]
    tbl[,1]=Transliterate(tbl[,1])
    res_names<-paste(Transliterate(unname(unlist(tbl[1,]))), Transliterate(unname(unlist(tbl[2,]))), sep="")
    tbl=tbl[-c(1,2),]

    if(dnames){
      res_names<-res_names[!(res_names==""|is.na(res_names))]
    }else{
      namesC<-paste0("C", 1:(length(tbl[1,])-4), sep="")
      namesP<-paste0("T", 1:4, sep="")
      res_names=c(namesC, namesP)
    }
    res_return<-data.frame(cbind(data.frame(rbind(data_info, row.names = NULL)), x, tbl, row.names = NULL))

    level <- grepl("level|link|url",names(x))
    names_x <- names(x)[level]
    max_level <- suppressWarnings(max(as.numeric(unlist(regmatches(names_x, gregexpr("[[:digit:]]+", names_x))))))
    names_x[names_x=="link"]<-c(paste0("level",max_level+1,""))
    res_names[1]<-"link"
  }

  names_vector <- c(data_info_names, names_x, res_names)
  names_vector <- gsub("^\\s+|\\s+$","", names_vector)
  colnames(res_return)<-names_vector

  invisible(gc())
  return(res_return)}


contentextractor<-function(x, uplevel, ttime, typedata, dnames, savetodir, tabextract){
  errorf<-function(y){
    max_el<-max(unlist(lapply(y, function(x){length(x[!is.na(x)])})))
    errorM<-lapply(y, function(x){
      x<-x[!is.na(x)];
      r<-if(any(x == "error")) rep("error", max_el) else unname(unlist(x))
      return(r)})
    names(errorM) <- colnames(y)
    return(errorM)}

  if (is.character(x) & typedata=="slow"){
    list.vote_content <- scrappage(x, ttime, savetodir, tabextract)}

  if (is.character(x) & typedata=="fast"){
    list.vote_content <- scrappage_fast(x, ttime, savetodir, tabextract)}

  #regular data frame
  if (is.data.frame(x) & typedata=="slow"){
    list.vote_content<-lapply(1:dim(x)[1], function(iter) {
      k=as.data.frame(scrappage(x[iter,], ttime, dnames, savetodir, tabextract));
      cat(paste("scraping page N", iter, "of", uplevel, sep=" "), "\n")
      return(k)})%>%errorf()

    el <- lapply(list.vote_content, length)
    if (length(unique(sapply(el, length)))>1) {warning('Rows sizes across units vary.')
    }

    return.content <- data.frame(do.call(rbind,list.vote_content))
    colnames(return.content)<-names(scrappage(x[1,], ttime, dnames, savetodir, tabextract))
  }
  #fast download
  if(is.data.frame(x) & typedata=="fast"){
    list.vote_content<-lapply(1:dim(x)[1], function(iter) {
      k=as.data.frame(scrappage_fast(x[iter,], ttime, dnames, savetodir, tabextract));
      cat(paste("scraping page N", iter, "of", uplevel, sep=" "), "\n")
      return(k)})

    el <- lapply(list.vote_content, length)
    if (length(unique(sapply(el, length)))>1) {warning('Rows sizes across units vary.')}

    return.content <- data.frame(do.call(rbind,list.vote_content))
    colnames(return.content)<-names(scrappage_fast(x[1,], ttime, dnames, savetodir, tabextract))}


  selcols<-colnames(return.content)[!grepl("info|level|link|url", colnames(return.content))]

  return.content[selcols] = apply(return.content[selcols], 2, function(x) suppressWarnings(as.numeric(as.character(x))))
  return(return.content)}
