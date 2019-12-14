#' @title fileURLextractor function
#' @description This function extracts the links and urls from the html page of the Central Election Commission.
#' @param html_file html file object.
#' @param tabextract  select the table number to extract in order to override the table selection algorithm.
#' @param hashid generate md5 hash id for an election.
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
#' res_territ <- wterrit %>% rowURLextractor("Rezul'taty vyborov") %>%
#' listURLextractor() %>%
#' dataBuilder(ttime = FALSE, typedata = "slow", bylevel = "level3")
#'

fileURLextractor <- function(html_file, tabextract = NULL, hashid = FALSE){

  cat("\n\nStarting fileURLextractor()...\n\n");

  tbln <- webpage %>% html_nodes(xpath="//table")

  if(is.null(tabextract)){
    result <- tbln[8] %>% scrapwebpage
  }else{
    result <- tbln[tabextract] %>% scrapwebpage
  }

  if(hashid){
    vdigest <- Vectorize(digest)
    result <- result %>% mutate(level3 = vdigest(paste(result$level1, result$level2, sep="_")))
  }

  result <- result[ , order(names(result))]

  return(result)}
