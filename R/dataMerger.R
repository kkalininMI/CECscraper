#' @title dataMerger function
#' @description This function merges the list of data objects together.
#' @param x list of data objects.
#' @param byrow   list is merged by row or by column.  If byrow==FALSE only two objects are merged into a list.
#' @export
#' @import dplyr
#' @return Returns the data.frame object with urls and links.
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


dataMerger<-function(x, byrow = TRUE){

  cat("\n\nStarting dataMerger()...\n\n")

  storage<-list()
  if(!is.list(x)) stop('the list of scrapped objs is required')

  if(byrow==FALSE){
    list_merge <- list()
    levelvar<-names(x[[1]]$data[[1]])[grepl("level|link", names(x[[1]]$data[[1]]))]
    x1=x[[1]]$data[[1]][levelvar]
    x2=x[[2]]$data[[1]][levelvar]
    s<-sapply(1:length(x1), function(x) sum(match(x1[,x], x2[,x])>=1, na.rm=TRUE)/dim(x1)[1])
    levelvar_adj <- levelvar[s==1]

    for(interN in names(x[[1]]$data)){
      storage[interN]<-lapply(1:(length(x)-1), function(iter) {
        merge(x[[iter]]$data[[interN ]],x[[iter+1]]$data[[interN]],
              by=c(levelvar_adj), all=TRUE)})
    }
  }
  if(byrow==TRUE){
    storage <- tryCatch(do.call(rbind.data.frame, x$data), error = function(e) e)
    if(inherits(storage,  "error")){
      warning("The list sizes vary.")
      warning("Attempting to merge the lists into a single data frame.")

      cnames = NA
      fulldata <- data.frame(matrix(NA, ncol = 10000, nrow = 1000), stringsAsFactors = FALSE)
      acc=1
      print("Merging lists into a data frame...")
      for(i in 1:length(x$data)){    #length(list.vote_content)
        adat=x$data[[i]]
        n.dat=names(adat)
        for(j in 1:length(n.dat)){
          if(!n.dat[j]%in%cnames){
            cnames=c(cnames, n.dat[j])
            fulldata[acc:(acc+dim(adat)[1]-1),which(cnames%in%n.dat[j])]<-as.character(adat[,j])
          }else{
            fulldata[acc:(acc+dim(adat)[1]-1),which(cnames%in%n.dat[j])]<-as.character(adat[,j])
          }
        }
        acc=acc+dim(adat)[1]
      }
      storage <- fulldata[,-1]; cnames=cnames[-1]
      storage<-storage[1:(acc-1),1:(length(cnames))]
      names(storage)<-cnames}
    levelvar=NULL
  }

  rownames(storage) <- NULL
  return_result <- list(data=storage, bylevel=levelvar, mergedate=Sys.time())

  return(return_result)}
