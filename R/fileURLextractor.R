#' @title fileURLextractor function
#' @description This function extracts the links and urls from an html page of the Central Election Commission.
#' @param html_file html file object.
#' @param tabextract  a table number to extract in order to override the table selection algorithm (by default, 1).
#' @param hashid generate a unique md5 hash id for election (TRUE).
#' @param messages display progress messages (TRUE).
#' @export
#' @import rvest
#' @import digest
#' @return Returns the data.frame object containing levels, links and urls of elections.
#' @examples
#' library(CECscraper)
#' library(rvest)
#'
#' webpage <- read_html(system.file("elections_primorye.html", package="CECscraper"))
#'
#' wterrit <- webpage %>% fileURLextractor(hashid = TRUE)
#' wterrit$webscrape[1] <- FALSE
#' res_territ <- wterrit %>% rowURLextractor("Rezul`taty vyborov") %>%
#' listURLextractor() %>%
#' dataBuilder(ttime = FALSE, typedata = "slow", bylevel = "level3")

fileURLextractor <- function(html_file, tabextract = NULL, hashid = FALSE, messages = TRUE){

  if(isTRUE(messages)){
    cat("\n\nStarting fileURLextractor()...\n\n");
    }

  tbln <- webpage %>% html_nodes(xpath="//table")

  if(is.null(tabextract)){
    result <- tbln[8] %>% scrapwebpage
  }else{
    result <- tbln[tabextract] %>% scrapwebpage
  }

  if(hashid){
    vdigest <- Vectorize(digest)

    unique_id<-paste(result$level1, result$level2, result$link, sep="_")
    if(any(duplicated(unique_id))){
       which_dup_id <- unique_id[unique_id%in%unique_id[duplicated(unique_id)]]
       new_dup_id <- paste(which_dup_id, 1:length(which_dup_id), sep="_")
       unique_id[unique_id%in%unique_id[duplicated(unique_id)]] <- new_dup_id
    }

    result <- result %>% mutate(level3 = vdigest(unique_id))
  }

  result <- data.frame(result[ , order(names(result))])

  return(result)}
