#' @title eftFormatter function
#' @description This function reformats the data for its use with Election Forensics Toolkit.
#' @param x the object from Databuilder/DataMerger function.
#' @param Nvalid valid votes computed using either "CEC" (using the Central Election Comission's formula); "AllVotes" (total number of votes cast for all candidates).
#' @param levels add information on levels (TRUE) or not (FALSE) to the output.
#' @export
#' @import dplyr
#' @return Returns the data.frame object with three variables: "Votes" (number of ballots for the leading parties or candidates), "Nvalid" (number of casted ballots), "Voters" (number of eligible voters).
#' @examples
#' library(CECscraper)
#'
#'  murl <- "http://notelections.online/region/izbirkom?action=show&vrn=27720002327736&region=77&prver=0&pronetvd=null"
#' uiks <- listURLextractor(
#'             rowURLextractor(
#'                listURLextractor(
#'                    listURLextractor(murl))[1:5,], 'sayt izbiratel`noy komissii sub`yekta'))
#' uiks_voting <- rowURLextractor(uiks, "Rezul`taty vyborov|vyborov po odnomandatnomu \\(mnogomandatnomu\\) okrugu")
#' uiks_voting_data <- dataBuilder(uiks_voting, bylevel="level1", ttime=FALSE)
#' uiksEFT <- eftFormatter(uiks_voting_data)


eftFormatter<-function(x, Nvalid="CEC", levels=TRUE){

  cat("\n\nStarting eftFormatter()...\n\n");

  storage<-list()
  if(!is.list(x)) stop('the list of scrapped objs is required')

  storage<-lapply(1:length(x$data),
                  function(iter) { dat <- x$data[[iter]]
                  winner<-names(which.max(apply(dat[,names(dat)[grepl("P", names(dat))]],2,function(x) sum(x, na.rm=TRUE))))
                  Votes=dat[winner]
                  if (Nvalid=="AllVotes"){
                    Nvalid = apply(dat[,grepl("P", names(dat))],1, function(x) sum(x, na.rm=TRUE))
                  }else{
                    Nvalid = dat['C8'] + dat['C9']
                  }
                  Voters=dat['C1']
                  if(levels==FALSE){
                    output=data.frame(Votes, Nvalid, Voters)
                    colnames(output)<-c("Votes", "Nvalid", "Voters")
                  }else{
                    levelv=dat[,grepl("level|link|url",names(dat))]
                    output=data.frame(levelv, Votes, Nvalid, Voters)
                    colnames(output)<-c(names(levelv),"Votes", "Nvalid", "Voters")
                  }
                  return(output)})
  names(storage)<-names(x$data)
  return(storage)}
