#' Description readSpecification
#' @name readSpecification
#' @aliases readSpecification
#' @title readSpecification
#' @param  path
#' @param  type
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
readSpecification<-function(path,type=c("all","network","hobby")){
  type<-match.arg(type)

  if (type=="all"){
    network<-readSpecification(path,"network")
    hobby<-readSpecification(path,"hobby")

    if (!setequal(network$school_name,hobby$school_name))
      stop("School name are not the same between network and hobby!")

    return(list(school=network$school_name,network=network$type_name,hobby=hobby$type_name))
  }

  all_name<-dir(path%+%type)
  school_name<-unique(strtrim(all_name,4))

  # 1+nchar("kslo__") + nchar(type) == 7 + nchar(type)
  # nchar(".dta") ==4 
  type_name<-unique(sapply(all_name,function(x) substr(x,7 + nchar(type),nchar(x)-4)))

  # Check whether all the product exists
  start<- school_name%+%"_"%+%type%+%"_"
  end<-type_name
  check_name<-as.vector(outer(start,end,FUN="%+%")) %+%".dta"

  if (!setequal(all_name,check_name) )
    stop("Some combination of school and " %+% type %+%" is missing in the file.")

  list(school_name=school_name,type_name=type_name)
}

#' Description getVariableName
#' @name getVariableName
#' @aliases getVariableName
#' @title getVariableName
#' @param  type
#' @param  spec
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
getVariableName<-function(type=c("school","network","hobby"),spec){
  spec[[type]]
}

#' Description getFileName
#' @name getFileName
#' @aliases getFileName
#' @title getFileName
#' @param type
#' @param spec
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
getFileName<-function(type=c("network","hobby"),spec){
  start<-getVariableName("school",spec) %+% "_" %+% type %+% "_"
  end<-getVariableName(type,spec) 

  as.vector(outer(start,end,FUN="%+%"))%+% ".dta"
## Test
# path = "C:/Users/julian/Dropbox/Tom/SESHK/data clean/SESHK2011 - network - 0.6.0/"
# spec <- readSpecification(path,type="all")

# getVariableName(type="school",spec=spec)
# getVariableName(type="network",spec=spec)
# getVariableName(type="hobby",spec=spec)

# getFileName(type="network",spec)
# getFileName(type="hobby",spec)

# setequal(dir(path%+%"network"),getFileName(type="network",spec))
# setequal(dir(path%+%"hobby"),getFileName(type="hobby",spec))
}











