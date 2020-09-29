#' @title dataBuilder function
#' @description This function builds the data frame from the webpage or listURLextractor/rowURLextractor objects.
#' @param x link/list/data frame with links.
#' @param bylevel  define data subsets for an output (i.e. single-member districts); if bylevel=NULL the whole data set is returned.
#' @param ttime checks if extracted data covers reported turnout over election day (TRUE) or not (FALSE).
#' @param typedata "slow" if the data is extracted from "rezultaty vyborov" link (slow approach); "fast" if the data is extracted from "svodnya tablitsa" (pivot table) (fast approach).
#' @param dnames assign original labels to column names(TRUE).
#' @param tabextract select the table number to extract in order to override the table selection algorithm.
#' @param savetodir save html data files to specified directory, i.e. "C:/Documents".
#' @param messages display progress messages (TRUE).
#' @export
#' @import dplyr
#' @return The list containing the following parameters:
#' \itemize{
#'   \item data - data frame or the list of data frames
#'   \item ttime - input parameter
#'   \item dnames - input parameter
#'   \item bylevel - input parameter
#'   \item retreivaldate - time, date of data retrieval
#' }
#' @examples
#' library(CECscraper)
#'
#' #Example 1
#' murl<-"http://notelections.online/region/izbirkom?action=show&vrn=27720002327736&region=77&prver=0&pronetvd=null"
#' uiks <- listURLextractor(
#'             rowURLextractor(
#'                listURLextractor(
#'                    listURLextractor(murl))[1:5,], 'sayt izbiratel`noy komissii sub`yekta'))
#'
#' uiks_turnout<-rowURLextractor(uiks, "Dannyye ob otkrytii pomeshcheniy dlya golosovaniya")
#' uiks_voting<-rowURLextractor(uiks, "Rezul`taty vyborov|vyborov po odnomandatnomu \\(mnogomandatnomu\\) okrugu")
#'
#' uiks_turnout_data<-dataBuilder(uiks_turnout, bylevel="level1", ttime=TRUE)
#' uiks_voting_data<-dataBuilder(uiks_voting, bylevel="level1", ttime=FALSE)
#' uiks_merged<-dataMerger(list(uiks_voting_data,uiks_turnout_data), byrow=FALSE)
#'
#' #Example 2
#' # library(dplyr)
#' # murl="http://notelections.online/region/izbirkom?action=show&global=1&vrn=100100084849062&region=0&prver=0&pronetvd=null"
#' #fast_downloadT<-rowURLextractor(murl, "Predvaritel`nyye svedeniya ob uchastii izbirateley")%>%
#' #                 listURLextractor()%>%listURLextractor()%>%sample_n(100, replace = FALSE)%>%
#' #                 rowURLextractor("sayt izbiratel`noy komissii sub`yekta Rossiyskoy Federatsii")%>%
#' #                 dataBuilder(typedata="fast", bylevel="level2", ttime=TRUE)%>%dataMerger()


dataBuilder<-function(x, bylevel=NULL, ttime=FALSE,  typedata="slow", dnames=FALSE, tabextract=NULL, savetodir="", messages=TRUE){

  mdat <- NULL

  if(isTRUE(messages)){
    cat("\n\nStarting dataBuilder()...\n\n")
    }

  if("download" %in% colnames(x)) {x <- x[x$download,]}

  assign("filecounter", 1 , envir = .GlobalEnv)
  storage<-list()

  if(!is.null(bylevel)|is.list(x)){

    if(!is.null(bylevel)){
      assign("splvar", x[names(x)%in%bylevel])
      mdat <- split(x, splvar);}

    if(!is.data.frame(x) & is.null(mdat)){
      if("elections"%in%names(x) & dim(x$pipe.table)[1]>1 & all(x$elections$extracted.res!="No data could be extracted")){
        mdat <- lapply(x$elections, function(xx) xx$extracted.res)
          if (is.null(dim(mdat)))  mdat <- x$elections$extracted.res

          }else if(is.null(mdat)){
          mdat <- x$elections$extracted.res
    }
    }

    if(!is.data.frame(mdat)){

      for (iterN in names(mdat)){
        if(mdat[[iterN]]!="No data could be extracted"){
          storage[[iterN]] <- contentextractor(mdat[[iterN]], uplevel=iterN, ttime, typedata, dnames, savetodir, tabextract)
        }else{
          storage[[iterN]] <- "No data could be extracted"
          }}

      }else{
        if(mdat!="No data could be extracted"){
          storage <- contentextractor(mdat, uplevel="Full dataset", ttime, typedata, dnames, savetodir, tabextract)
          }else{
          storage <- "No data could be extracted"
        }
      }
    }else{
      mdat=x
      storage<-contentextractor(mdat, uplevel="Full dataset", ttime, typedata, dnames, savetodir, tabextract)
    }

  if("pipe.table"%in%names(x)){pipe.table <- x$pipe.table}else{pipe.table <-NULL}
  result=list(data=storage, ttime=ttime, dnames=dnames, bylevel=bylevel, retreivaldate=Sys.time(), pipe.table=x$pipe.table)

  return(result)}
