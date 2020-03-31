#' @title execPipeList function
#' @description This function extracts URLs from the pipe saved in the autoPipeSearch object.
#' @param x autoPipeSearch object containing pipe.table that can be manually modified if desired.
#' @param messages display progress messages (messages = FALSE).
#' @export
#' @import dplyr
#' @import rvest
#' @return Extracts the data via a pipe or a list of pipes and saves it inside the elections object.
#' @examples
#' library(CECscraper)
#' library(dplyr)
#' library(rvest)
#'
#' webpage <- read_html(system.file("elections_primorye.html", package="CECscraper"))
#'
#'  wterrit <- webpage %>% fileURLextractor(hashid = FALSE)
#'  uik_url1 <- wterrit[1,]%>%autoPipeSearch(extracturls=TRUE, ttime=TRUE)
#'  uik_url2 <- uik_url1%>%execPipeList()
#'  uik_url3 <- uik_url2%>%dataBuilder(ttime=FALSE)

execPipeList <- function(x, messages=TRUE){

  if(isTRUE(messages)){
    cat("\n\nStarting execPipeList()...\n\n")
  }

  extracted.dat <- NULL
  x <- x$pipe.table
  x[] <- lapply(x, as.character)

  extracted.d <-
    lapply(1:dim(x)[1], function(xx){
      if(x$pipe.formula[xx]!="unidentified"){
        cat("scraping election N", xx, "\n")
        base.url <- x$url[xx]
        extracted.urls <- eval(parse(text=x$pipe.formula[xx]))
        level <- grepl("level|link",names(extracted.urls));
        max_level <- suppressWarnings(max(as.numeric
                                          (unlist(regmatches(names(extracted.urls)[level],
                                                             gregexpr("[[:digit:]]+", names(extracted.urls)[level]))))))

        names(extracted.urls)[names(extracted.urls) == 'link'] <- paste0("level", max_level+1)

        x <- x[xx, !(names(x)%in%c("url", "webscrape"))]

        dat<-data.frame(do.call("rbind", replicate(dim(extracted.urls)[1], x[xx], simplify = FALSE)), extracted.urls)
        level_vars<-colnames(dat)[grepl("level", colnames(dat))]
        nlevel_vars<-colnames(dat)[!(grepl("level", colnames(dat))|colnames(dat)%in%"webscrape"|
                                       colnames(dat)%in%"pipe.formula")]

        max_level_upd<-length(level_vars)
        new_level_vars<-paste("level", 1:max_level_upd, sep="")
        colnames(dat)[grepl("level", colnames(dat))]<-new_level_vars

        dat<-subset(dat, select=c(new_level_vars, nlevel_vars))

      }else{

        cat("scraping election N", xx, "\n")
        extracted.urls <- "No data could be extracted"}
    })

  extracted.dat$elections$extracted.res <- extracted.d

  names(extracted.dat$elections$extracted.res) <-
    apply(x[colnames(x)[grepl("level|link", colnames(x))]], 1, function(x) paste(x,  collapse=", "))

  extracted.dat$pipe.table<-x
  extracted.dat$retreivaldate=Sys.time()

  results <- extracted.dat
  on.exit(closeAllConnections())
  invisible(gc())

  return(results)}
