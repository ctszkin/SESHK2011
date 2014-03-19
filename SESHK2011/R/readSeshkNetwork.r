#' Description 
#' @name readSeshkNetwork
#' @aliases readSeshkNetwork
#' @title readSeshkNetwork
#' @param .data_source path
#' @param .version Version number. Default is "SESHK2011 - network - 0.6.2/"
#' @return a raw_data 
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
readSeshkNetwork<-function(.data_source,.version="SESHK2011 - network - 0.6.2/"){
	if (.version!="SESHK2011 - network - 0.6.2/")
		warning("Only tested under 0.6.2. " %+% .version %+% " is not tested. Error may occur.")

	path<-
	if (missing(.data_source))
		.version
	else
 		.data_source %+% "/" %+% .version

  	spec<-readSpecification(path)

	# read network data
	all_network_data<-  
	foreach(i = getFileName("network",spec),.combine=c ) %do% {
		out <- read.dta(path %+% "network/" %+% i )  
		out<-list(out)
		names(out)<-substr(i,1,nchar(i)-4)
		out
	}

	## store the matrix order and check it is same for all network data
	network_matrix_order<-   
	foreach(i = getVariableName("school",spec),.combine=c ) %do% {
		out<-
		foreach( j = getVariableName("network",spec),.combine=cbind ) %do% {
			all_network_data[[i %+% "_network_" %+% j ]]$case_id
		}
		if (!all(out[,1]==out))
			stop("Some order of case_id of network matrix is not the same. please check it out. e.g.",i," ",j,"\n")
		out<-list(out[,1])
		names(out)<- "network_matrix_order_"%+% i
		out
	}

	## drop all unrelated variables in network matrix
	all_network_data<-   
	foreach(i = all_network_data, j = names(all_network_data),.combine=c ) %do% {
		# drop case_id
		out <- subset(i,select= ! (names(i) == "case_id") )
		# transform to matrix
		out <- data.matrix(out)

		## replace all non-zero value to 1
		# out <- list( (out > 0) + 0 )
		out<-list(out)
		names(out) <- j 
		out
	}
	
	# read wide data
	data_wide <- read.dta(path %+% "data_widepart.dta")
	
	# foreach school, sort by network_matrix_order and merge the peer_evaluation data
	data_wide<-
	foreach(i = getVariableName("school",spec),.combine=c ) %do% {
		out<-subset(data_wide, subset = school_name==i )
		
		ID_order <- network_matrix_order[["network_matrix_order_"%+%i]]
		ID_order <- data.frame(index=1:length(ID_order),case_id=ID_order)
		
		# sort the data by network_matrix_order
		out <- merge(out,ID_order,by.x="case_id",by.y="case_id")
		out <- out[with(out,order(index)),]
		out$index <- NULL

		out<-list(out)
		names(out)<-i
		out
	}

	# read hobby data
	data_hobby<-   
	foreach( i = getFileName("hobby",spec),.combine=c )%do%{
		out <- read.dta(path %+% "hobby/" %+% i ) 

		school_name<-substr(i,1,4)

		ID_order <-network_matrix_order[["network_matrix_order_"%+%school_name]]
		ID_order <- data.frame(index=1:length(ID_order),case_id=ID_order)
		
		# sort the data by network_matrix_order
		out <- merge(out,ID_order,by.x="case_id",by.y="case_id")
		out <- out[with(out,order(index)),]
		out$index <- NULL
		out$case_id <- NULL

		out<-list(as.matrix(out))
		names(out)<-substr(i,1,nchar(i)-4)
		out
	}

	raw_data <- c(list(data_wide=data_wide),all_network_data,data_hobby,network_matrix_order,spec=list(spec))   
	return(raw_data) 
}

#' Description getNetworkMatrixOrder
#' @name getNetworkMatrixOrder
#' @aliases getNetworkMatrixOrder
#' @title getNetworkMatrixOrder
#' @param .raw_data
#' @param .school
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
getNetworkMatrixOrder<-function(.raw_data,.school){
	.raw_data[[ "network_matrix_order_" %+%  .school ]]
}

#' Description getDataWide
#' @name getDataWide
#' @aliases getDataWide
#' @title getDataWide
#' @param  .raw_data
#' @param  .school
#' @param  .drop_by_case_id
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
getDataWide<-function(.raw_data,.school,.drop_by_case_id){
	stopifnot(.school %in% getVariableName("school",.raw_data$spec))

	out<-.raw_data$data_wide[[.school]]


	case_id <- getNetworkMatrixOrder(.raw_data,.school)
	sort_by_order <- data.frame(index=1:length(case_id),case_id=case_id)

	out <- merge(out,sort_by_order,by.x="case_id",by.y="case_id")
	out <- out[with(out,order(index)),]
	out$index <- NULL
	
	# drop by case id
	if (!missing(.drop_by_case_id))
		out <- subset(out, subset = ! case_id %in% .drop_by_case_id)

	out
}

#' Description getNetwork
#' @name getNetwork
#' @aliases getNetwork
#' @title getNetwork
#' @param  .raw_data
#' @param  .school
#' @param  .network
#' @param  .drop_by_case_id
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
getNetwork<-function(.raw_data,.school,.network,.drop_by_case_id){
	stopifnot(.school %in% getVariableName("school",.raw_data$spec))
	stopifnot(.network %in% getVariableName("network",.raw_data$spec))
	
	out<-.raw_data[[.school %+% "_network_" %+%.network]]

	if (!missing(.drop_by_case_id)){
		case_id<-getNetworkMatrixOrder(.raw_data,.school)
		drop_index<-which(case_id %in% .drop_by_case_id )
		if (length(drop_index)>0)
			out<-out[-drop_index,-drop_index]
	}
	out
}	

#' Description getHobby
#' @name getHobby
#' @aliases getHobby
#' @title getHobby
#' @param .raw_data
#' @param .school
#' @param .hobby
#' @param .drop_by_case_id
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
getHobby<-function(.raw_data,.school,.hobby,.drop_by_case_id){
	stopifnot(.school %in% getVariableName("school",.raw_data$spec))
	stopifnot(.hobby %in% getVariableName("hobby",.raw_data$spec))

	out <- .raw_data[[.school %+% "_hobby_" %+%.hobby]]
	if (!missing(.drop_by_case_id)){
		case_id<-getNetworkMatrixOrder(.raw_data,.school)
		drop_index<-which(case_id %in% .drop_by_case_id )
		if (length(drop_index)>0)
			out<-out[-drop_index,]
	}
	out
}	

#' Description prepareDataprepareData
#' @name prepareData
#' @aliases prepareData
#' @title prepareData
#' @param  .raw_data
#' @param  .spec
#' @param .school
#' @param .hobby
#' @param .network_mode
#' @param .process_network
#' @param .drop_by_case_id
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
prepareData <- function (.raw_data, .spec, .school ){

    # .hobby <- match.arg(.hobby, several.ok = TRUE)
    # .school <- match.arg(.school, several.ok = TRUE)
    if ( missing(.school) ) {
      out <- foreach(i = .spec$school_name, .combine = c) %do% {
        prepareData(.raw_data=.raw_data, .school = i, .spec=.spec)
      }
      return(out)
    }

    data_wide <- getDataWide(.raw_data, .school)

    network_matrix_list = 
    lapply(.spec$network_info_list, function(x){
      network_matrix1 = getNetwork(.raw_data, .school, x$definition[1])
      network_matrix2 = getNetwork(.raw_data, .school, x$definition[2])
      out = x$process_network(network_matrix1, network_matrix2, data_wide)
      out
    })

    drop_case_id = .spec$findDropCaseID(data_wide, network_matrix_list)


    data_wide <- getDataWide(.raw_data, .school, drop_case_id)

    network_matrix_list = 
    lapply(.spec$network_info_list, function(x){
      network_matrix1 = getNetwork(.raw_data, .school, x$definition[1], .drop_by_case_id=drop_case_id)
      network_matrix2 = getNetwork(.raw_data, .school, x$definition[2], .drop_by_case_id=drop_case_id)
      # out = process_network_function_example(network_matrix1, network_matrix2, data_wide)
      out = x$process_network(network_matrix1, network_matrix2, data_wide)
      out
    })



    H <- foreach(i = .spec$hobby, .combine = c) %do% {
        out <- list(getHobby(.raw_data, .school, i, drop_case_id))
        names(out) <- i
        out
    }

    network_name = sapply(.spec$network_info_list, "[[", "name")

    group_index = list(id=which( .school == .spec$school_name ), all=length(.spec$school_name))

    # degree = sapply(network_matrix_list, rowSums)

    # colnames(degree) = paste0(    network_name = sapply(.spec$network_info_list, "[[", "name") , "_degree")

    # data_wide= cbind(data_wide, degree)


    data_wide = cbind(data_wide, .spec$genNetworkStatistics(network_matrix_list) )


    out <- list(network_matrix_list = network_matrix_list, data = data_wide, H = H, network_name=network_name,group_index=group_index)
    out <- list(out)
    names(out) <- .school
    return(out)
}





#' Description splitNetworkData
#' @name splitNetworkData
#' @aliases splitNetworkData
#' @title splitNetworkData
#' @param  network_data
#' @param  group
#' @param  network_only
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
splitNetworkData<-function(network_data, group,network_only=FALSE){
	data<-network_data$data
	group_name <- as.vector(unique(data[[group]]))
	
	if (network_only){
		foreach ( i = group_name ) %do% {
			index <- which(data[group]==i)
			network_data$D[index,index]
		}
	} else{
		D<-network_data$D
		data<-network_data$data
		
		foreach ( i = group_name ) %do% {
			index <- which(data[group]==i)
			list(data=data[index,],D = D[index,index])
		}
	}
}

#' Description ToUndirectedGraph
#' @name ToUndirectedGraph
#' @aliases ToUndirectedGraph
#' @title ToUndirectedGraph
#' @param  method
#' @param  network_matrix1
#' @param  network_matrix2
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 
ToUndirectedGraph<-function(method=c("undirected_and","undirected_or","directed","directed_inversed"),network_matrix1,network_matrix2){

	method<-match.arg(method)

	if (missing(network_matrix2))
		network_matrix2<-network_matrix1

	if (method=="undirected_and"){
		network_matrix <- t(network_matrix1) & network_matrix2
		network_matrix <- (network_matrix > 0) + 0
		diag(network_matrix)<-0
	} else if (method=="undirected_or"){
		network_matrix <- t(network_matrix1) + network_matrix2
		network_matrix <- (network_matrix > 0) + 0
		diag(network_matrix)<-0
	} else if (method=="directed"){
		network_matrix <- network_matrix1
	} else if (method=="directed_inversed"){
		network_matrix <- t(network_matrix1)
	}
	return(network_matrix)
}

