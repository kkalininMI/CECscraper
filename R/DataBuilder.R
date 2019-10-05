#' @title DataBuilder function
#' @description This function builds the data frame from the webpage or MenuLinkExtractor/PageLinkExtractor objects.
#' @param x link/list/data frame with links.
#' @param bylevel  define data subsets for an output (i.e. single-member districts).
#' @param ttime checks if extracted data covers reported turnout over election day (TRUE) or not (FALSE).
#' @param typedata checks whether the data extracted from "svodnya tablitsa"(pivot table) link (fast approach) or "rezultaty vyborov" link (slow approach).
#' @param dnames assign to column name labels original labels (TRUE) or not (FALSE).
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
#' uiks<-MenuLinkExtractor(MenuLinkExtractor(MenuLinkExtractor(murl))[1:5,])
#'
#' uiks_turnout<-PageLinkExtractor(uiks, "Dannyye ob otkrytii pomeshcheniy dlya golosovaniya")
#' uiks_voting<-PageLinkExtractor(uiks, "Rezul'taty vyborov|vyborov po odnomandatnomu \\(mnogomandatnomu\\) okrugu")
#'
#' uiks_turnout_data<-DataBuilder(uiks_turnout, bylevel="level1", ttime=TRUE)
#' uiks_voting_data<-DataBuilder(uiks_voting, bylevel="level1", ttime=FALSE)
#' uiks_merged<-DataMerger(list(uiks_voting_data,uiks_turnout_data), byrow=FALSE)
#'
#' #Example 2
#' # library(dplyr)
#' # murl="https://tinyurl.com/y369jngp"
#' #fast_downloadT<-PageLinkExtractor(murl, "Predvaritel'nyye svedeniya ob uchastii izbirateley")%>%
#' #                 MenuLinkExtractor()%>%MenuLinkExtractor()%>%sample_n(100, replace = FALSE)%>%
#' #                 PageLinkExtractor("sayt izbiratel'noy komissii sub\"yekta Rossiyskoy Federatsii")%>%
#' #                 DataBuilder(typedata="fast", bylevel="level2", ttime=TRUE)%>%DataMerger()

DataBuilder<-function(x, bylevel=NULL, ttime=FALSE,  typedata="slow", dnames=FALSE, tabextract=NULL, savetodir=""){
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
  return_result=list(data=storage, ttime=ttime, dnames=dnames, bylevel=bylevel, retreivaldate=Sys.time())
  return(return_result)}
