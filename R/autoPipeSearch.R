#' @title autoPipeSearch function
#' @description This function automatically constructs pipe for a given election.
#' @param x url, list of urls.
#' @param ttime checks if data covers reported turnout over election day (TRUE) or not (FALSE).  ttime is valid when blocks=NULL
#' @param blocks vector of blocks to be used to construct a pipe. blocks override ttime parameters. By default, c("listURLextractor()", "rowURLextractor('sayt izbiratel`noy komissii sub`yekta')", "rowURLextractor('Itogi golosovaniya|Rezul`taty vyborov')").
#' @param search.term once the stop "term" is detected the algorithm stops building the pipe (by default, UIK|uik|uchastok).
#' @param hits number of times the stop word is "hit" by the algorithm. Each time the stop word is "hit" extra block is added to the pipe (by default, hits=2).
#' @param extracturls urls are extracted using the pipe or the list of pipes (by default, extracturls=FALSE).
#' @param breakloop maximum number of iterations for optimal path search (by default, breakloop=100).
#' @param messages display progress messages (messages = FALSE).
#' @export
#' @import dplyr
#' @import rvest
#' @return Returns the list elections and pipe.table objects.
#' @examples
#' library(CECscraper)
#' library(dplyr)
#' library(rvest)
#'  webpage <- read_html(system.file("elections_primorye.html", package="CECscraper"))
#'
#'  #Example 1
#'  wterrit <- webpage %>% fileURLextractor(hashid = FALSE)
#'  #uik_url1 <- wterrit[1,]%>%autoPipeSearch(extracturls=TRUE, ttime=FALSE)
#'  uik_url1 <- wterrit[1,]%>%autoPipeSearch(ttime=TRUE)
#'  uik_url2 <- uik_url1%>%execPipeList()
#'  #uik_url3 <- uik_url2%>%dataBuilder(ttime=FALSE)
#'   uik_url3 <- uik_url2%>%dataBuilder(ttime=TRUE)
#'
#'  #Example 2
#'  uik_url1 <- wterrit[1,]%>%autoPipeSearch(blocks=c("listURLextractor()",
#'                                                   "rowURLextractor('sayt izbiratel`noy komissii sub`yekta')",
#'                                                   "rowURLextractor('Svodnaya tablitsa')"),  hits=3, search.term="Svodnaya tablitsa")
#'  uik_url2 <- uik_url1%>%
#'             execPipeList()%>%
#'             dataBuilder(typedata = "fast")
#'
#'  #Example 3
#'  uik_url1 <- wterrit[1,]%>%autoPipeSearch(blocks=c("listURLextractor()",
#'                                                   "rowURLextractor('sayt izbiratel`noy komissii sub`yekta')",
#'                                                   "rowURLextractor('Dannyye ob otkrytii pomeshcheniy')"),  hits=3, search.term="Dannyye ob otkrytii pomeshcheniy")
#'  uik_url2 <- uik_url1%>%
#'             execPipeList()%>%
#'             dataBuilder(ttime = TRUE, typedata = "fast")
#'
#'  #Example 4
#'  uik_url1 <- wterrit[1,]%>%autoPipeSearch(blocks=c("listURLextractor()",
#'                                                "rowURLextractor('sayt izbiratel`noy komissii sub`yekta')",
#'                                                "rowURLextractor('Svedeniya o kandidatakh')"),  hits=1, search.term="Svedeniya o kandidatakh")
#'  uik_url2 <- uik_url1%>%
#'              execPipeList()%>%
#'              scrapeCandidates(tabextract = NULL, savetodir = "")


autoPipeSearch<-function(x, ttime=FALSE, blocks=NULL, search.term=NULL, hits=2, extracturls=FALSE, breakloop=100, messages = FALSE){

   pipe.f=NULL
   pipe.table=NULL
   extracted.dat<-NULL

   if(is.null(blocks) & ttime==FALSE){
     blocks<-c("listURLextractor()",
               "rowURLextractor('sayt izbiratel`noy komissii sub`yekta')",
               "rowURLextractor('Itogi golosovaniya|Rezul`taty vyborov')")
    }

   if(is.null(blocks) & ttime==TRUE){
     blocks<-c("listURLextractor()",
               "rowURLextractor('sayt izbiratel`noy komissii sub`yekta')",
               "rowURLextractor('Dannyye ob otkrytii pomeshcheniy dlya golosovaniya')")
   }

   if("webscrape" %in% colnames(x)) {x <- x[x$webscrape,]}

   if (is.character(x) & length(x)==1){
     extracted.d <- pipefinder(x, blocks, search.term, hits, extracturls, breakloop, messages)
     extracted.dat$elections <- extracted.d
     if("pipe.formula"%in%names(extracted.dat$elections)){pipe.f <- extracted.dat$elections$pipe.formula}else{pipe.f <- NULL}
     pipe.table<-data.frame(url=x, pipe.formula=pipe.f, stringsAsFactors = FALSE)
     extracted.dat$pipe.table <- pipe.table
     }

   if (is.character(x) & length(x)>1){
     extracted.d <- lapply(1:length(x),
                           function(iter) {
                             cat("scraping election N", iter, "\n")
                             k<-tryCatch(pipefinder(x[iter], blocks, search.term, hits, extracturls, breakloop, messages), error = function(e) e)
                             if(inherits(k,  "error")) {
                               warning("Row URL non-scrapable or missing from the webpage");
                                       k="Row URL non-scrapable or missing from the webpage"}
                             return(k)})

     extracted.dat$elections <- extracted.d

     if("pipe.formula"%in%names(extracted.dat$elections[[1]])){
       pipe.f <- unlist(lapply(1:length(extracted.dat$elections),
                               function(o){extracted.dat$elections[[o]]$pipe.formula}))}else{pipe.f=NULL}

     pipe.table <- data.frame(url=x, pipe.formula=pipe.f)
     names(extracted.dat$elections) <- x
     extracted.dat$pipe.table <- pipe.table
    }

   if (is.data.frame(x)){
       extracted.urls <- lapply(1:dim(x)[1], function(iter) {
       cat("scraping election N", iter, "\n")
       k<-tryCatch(pipefinder(x$url[iter], blocks, search.term, hits, extracturls, breakloop, messages), error = function(e) e)
       if(inherits(k,  "error")) {warning("Row URL non-scrapable or missing from the webpage");
                                          k="Row URL non-scrapable or missing from the webpage"}
       return(k)})

     if(extracturls){
       extracted.d <- lapply(1:dim(x)[1], function(xx){
                                               ex.res <- extracted.urls[[xx]]$extracted.res
                                               level <- grepl("level|link",names(ex.res));
                                               max_level <- suppressWarnings(max(as.numeric
                                                                              (unlist(regmatches(names(ex.res)[level], gregexpr("[[:digit:]]+", names(ex.res)[level]))))))

                                               names(ex.res)[names(ex.res) == 'link'] <- paste0("level", max_level+1)

                                               x <- x[xx, !(names(x)%in%c("url", "webscrape"))]

                                               dat<-data.frame(do.call("rbind", replicate(dim(ex.res)[1], x, simplify = FALSE)), ex.res)

                                               level_vars<-colnames(dat)[grepl("level", colnames(dat))]
                                               nlevel_vars<-colnames(dat)[!grepl("level", colnames(dat))]
                                               max_level_upd<-length(level_vars)
                                               new_level_vars<-paste("level", 1:max_level_upd, sep="")
                                               colnames(dat)[grepl("level", colnames(dat))]<-new_level_vars

                                               dat<-subset(dat, select=c(new_level_vars, nlevel_vars))

                                               return(dat)})

         extracted.d <- lapply(1:length(extracted.d),
                               function(o){extracted.urls[[o]]$extracted.res <- extracted.d[[o]];
                               return(extracted.urls[[o]])})

         extracted.dat$elections <- extracted.d

        }else{
         extracted.dat$elections <- extracted.urls
        }

     names(extracted.dat$elections) <- apply(x[colnames(x)[grepl("level|link", colnames(x))]], 1, function(x) paste(x,  collapse=", "))

     if("pipe.formula"%in%names(extracted.dat$elections[[1]])){
       pipe.f<-unlist(lapply(1:length(extracted.dat$elections),
                             function(o){extracted.dat$elections[[o]]$pipe.formula}))
     }else{
       pipe.f=NULL}

     pipe.table <- data.frame(x, pipe.formula=pipe.f)
     extracted.dat$pipe.table <- pipe.table
      }

    results <- extracted.dat

    on.exit(closeAllConnections())
    invisible(gc())

    return(results)}
