#' Description readSpecification
#' @name readSpecification
#' @aliases readSpecification
#' @title readSpecification
#' @param  path path
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export

readSpecification<-function(path){
  # type<-match.arg(type)

  # if (type=="all"){
  #   network<-readSpecification(path,"network")
  #   hobby<-readSpecification(path,"hobby")

  #   if (!setequal(network$school_name,hobby$school_name))
  #     stop("School name are not the same between network and hobby!")

  #   return(list(school=network$school_name,network=network$type_name,hobby=hobby$type_name))
  # }

  type = "network"
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

  list(school=school_name,network=type_name)
}

#' Description getVariableName
#' @name getVariableName
#' @aliases getVariableName
#' @title getVariableName
#' @param  type type
#' @param  spec spec
#' @return value value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
getVariableName<-function(type=c("school","network"),spec){
  spec[[type]]
}

#' Description getFileName
#' @name getFileName
#' @aliases getFileName
#' @title getFileName
#' @param type type
#' @param spec spec
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
getFileName<-function(type=c("network"),spec){
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


#' genPairwiseIndex 
#' @name genPairwiseIndex
#' @aliases genPairwiseIndex
#' @title genPairwiseIndex
#' @param n
#' @return data.frame
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal 
#' @export
genPairwiseIndex <- function(n){
  # i<- foreach( i=2:n,.combine=c ) %do% seq(from=i,to=n)
  i = unlist(lapply(2:n, seq, to=n))
  j = rep(1:(n-1),times=(n-1):1)
  cbind(i,j)
}


#' getPairwiseFriendshipData 
#' @name getPairwiseFriendshipData
#' @aliases getPairwiseFriendshipData
#' @title getPairwiseFriendshipData
#' @param network_data network_data
#' @param network_formation_formula network_formation_formula
#' @return A data.frame consist of the estimates.
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal 
#' @export
getPairwiseFriendshipData<-function(network_data,network_formation_formula){
  data<-network_data$data
  n <- nrow(data)
  require_variable <-unique(sub("friends_","",all.vars(network_formation_formula)))

  require_variable <- setdiff(require_variable,".")

  data<-data[require_variable]
  
  pairwise_index = genPairwiseIndex(n)

  i = pairwise_index[,1]
  j = pairwise_index[,2]
  
  name_of_self_data <- names(data)
  name_of_friends_data <- paste("friends_",name_of_self_data,sep="")
  
  self_data <- data.frame(data[i,])
  
  friends_data <- data.frame(data[j,])
  names(self_data)<-name_of_self_data
  names(friends_data)<-name_of_friends_data
  
  self_data_matrix <- model.matrix(network_formation_formula,cbind(self_data,friends_data))

  names(friends_data)<-name_of_self_data
  names(self_data)<-name_of_friends_data
  
  friends_data_matrix <- model.matrix(network_formation_formula,cbind(self_data,friends_data))

  D<-network_data$network_matrix_list
  response_self = sapply(D, function(x) x[lower.tri(x)] )
  response_friends = sapply(D, function(x) t(x)[lower.tri(t(x))] )

  response_self=!!response_self
  response_friends=!!response_friends

  list(response_self=response_self, response_friends=response_friends,self_data_matrix=self_data_matrix,friends_data_matrix=friends_data_matrix)
}

#' genPairwiseHobbyData 
#' @name genPairwiseHobbyData
#' @aliases genPairwiseHobbyData
#' @title genPairwiseHobbyData
#' @param H H
#' @return a vector
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @keywords internal 
#' @export
genPairwiseHobbyData<-function(H){
  tHH<-tcrossprod(H)
  # stopifnot(nrow(tHH)==ncol(tHH))
  n<-nrow(tHH)

  index<-genPairwiseIndex(n)
  index2= (index[,1]-1) * n + index[,2]

  tHH[index2]
}



