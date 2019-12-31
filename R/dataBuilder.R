#' @title dataBuilder function
#' @description This function builds the data frame from the webpage or listURLextractor/rowURLextractor objects.
#' @param x link/list/data frame with links.
#' @param bylevel  define data subsets for an output (i.e. single-member districts); if bylevel=NULL the whole data set is returned.
#' @param ttime checks if extracted data covers reported turnout over election day (TRUE) or not (FALSE).
#' @param typedata "slow" if the data is extracted from "rezultaty vyborov" link (slow approach); "fast" if the data is extracted from "svodnya tablitsa" (pivot table) (fast approach).
#' @param dnames assign original labels to column names(TRUE).
#' @param tabextract select the table number to extract in order to override the table selection algorithm.
#' @param savetodir save html data files to specified directory, i.e. "C:/Documents".
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
#' murl<-"https://tinyurl.com/yy6roo3g"
#' uiks<-listURLextractor(listURLextractor(listURLextractor(murl))[1:5,])
#'
#' uiks_turnout<-rowURLextractor(uiks, "Dannyye ob otkrytii pomeshcheniy dlya golosovaniya")
#' uiks_voting<-rowURLextractor(uiks, "Rezul'taty vyborov|vyborov po odnomandatnomu \\(mnogomandatnomu\\) okrugu")
#'
#' uiks_turnout_data<-dataBuilder(uiks_turnout, bylevel="level1", ttime=TRUE)
#' uiks_voting_data<-dataBuilder(uiks_voting, bylevel="level1", ttime=FALSE)
#' uiks_merged<-dataMerger(list(uiks_voting_data,uiks_turnout_data), byrow=FALSE)
#'
#' #Example 2
#' # library(dplyr)
#' # murl="https://tinyurl.com/y369jngp"
#' #fast_downloadT<-rowURLextractor(murl, "Predvaritel'nyye svedeniya ob uchastii izbirateley")%>%
#' #                 listURLextractor()%>%listURLextractor()%>%sample_n(100, replace = FALSE)%>%
#' #                 rowURLextractor("sayt izbiratel'noy komissii sub\"yekta Rossiyskoy Federatsii")%>%
#' #                 dataBuilder(typedata="fast", bylevel="level2", ttime=TRUE)%>%dataMerger()

dataBuilder<-function(x, bylevel=NULL, ttime=FALSE,  typedata="slow", dnames=FALSE, tabextract=NULL, savetodir=""){

  cat("\n\nStarting dataBuilder()...\n\n")

  if("download" %in% colnames(x)) {x <- x[x$download,]}

  assign("filecounter", 1 , envir = .GlobalEnv)
  storage<-list()

  if(!is.null(bylevel)){
    assign("splvar", x[names(x)%in%bylevel])
    mdat <- split(x, splvar);

    for (iterN in names(mdat)){
      storage[[iterN]]<-contentextractor(mdat[[iterN]], uplevel=iterN, ttime, typedata, dnames, savetodir, tabextract)
    }

  }else{
    mdat=x
    storage<-contentextractor(mdat, uplevel="Full dataset", ttime, typedata, dnames, savetodir, tabextract)
  }
  result=list(data=storage, ttime=ttime, dnames=dnames, bylevel=bylevel, retreivaldate=Sys.time())
  return(result)}
