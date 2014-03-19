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



#' Description extract Data
#' @name extractData
#' @aliases extractData
#' @title extractData
#' @param  spec
#' @param  data
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 

extractData <- function(spec, data){
    
  # source("model_spec.r")
  formula = spec$formula
  network_formation_formula = Formula(spec$network_formation_formula)

  
  f1 = formula(network_formation_formula,rhs=1)

  ## if there is fixed effect, take out the intercept
  if ( !is.null(spec$network_formation_fixed_effect) && spec$network_formation_fixed_effect ){
    f1 = update(f1, ~.+-1)
  }

  other_network_variables<-NULL
  use_network_variable<-FALSE
  if (length(network_formation_formula)[2]==2){
    use_network_variable<-TRUE
    f2<-formula(network_formation_formula,rhs=2)
    other_network_variables<-attr(terms(f2),"term.labels")
  }
  H_name <-  attr(terms(f2),"term.labels")
  
  out<-getPairwiseFriendshipData(data,f1)
  out$formula = formula
  out$network_formation_formula = network_formation_formula


  if (length(network_formation_formula)[2]==2){
    H_pair<-sapply(data$H[H_name],genPairwiseHobbyData)
    out$self_data_matrix<-cbind(out$self_data_matrix,H_pair)
    out$friends_data_matrix<-cbind(out$friends_data_matrix,H_pair)
  }
  
  ## generate dummy matrix
  if ( !is.null(spec$network_formation_fixed_effect) && spec$network_formation_fixed_effect ){
    nn = NROW(out$self_data_matrix)
    dummy_matrix = matrix(0, nrow=nn, ncol = data$group_index$all)
    dummy_matrix[,data$group_index$id] = 1

    colnames(dummy_matrix) = spec$school_names

    out$self_data_matrix = cbind(dummy_matrix,out$self_data_matrix)
    out$friends_data_matrix = cbind(dummy_matrix,out$friends_data_matrix)
  }

  # TODO: add seat assignment here
  # out$response1 <- as.logical(out$response)
  # out$response <- list(response1=out$response1)
  
  # if(!single_network){
  #   out$response2 <- as.logical(out2$response)
  #   out$response$response2 = out$response2
  # }

# single network
  out$D_list = data$network_matrix_list
  out$W_list = lapply(out$D_list, generateWeighting)

  y_x_wx = getXandWX(formula,data)
  out$y = y_x_wx$y
  out$x = y_x_wx$x
  out$wx = y_x_wx$wx
  out$wy = y_x_wx$wy
  out$x_wx = y_x_wx$X

  out$n = length(out$y)
  out$k_x_wx = ncol(out$x_wx)
  out$k_gamma = ncol(out$self_data_matrix)

  out$group_index = genPairwiseIndex(length(out$y))
  out$network_name = data$network_name 

  return(out)
}



#' Description getXandWX
#' @name getXandWX
#' @aliases getXandWX
#' @title getXandWX
#' @param  formula
#' @param  data
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 


getXandWX <- function(formula,data){
  formula_F<-Formula(formula)
  f1<-formula(formula_F,rhs=1)
  
  f2<-
    if(length(formula_F)[2]==1) {
      f1
    } else {
      formula(formula_F,rhs=2)
    }
  
  var_f2 = sub("friends_","", attr( terms(f2),"term.labels"))
  var_f2 = paste0("friends_",var_f2)

  update_f2 = as.formula(".~.+" %+% paste(var_f2,collapse="+"))

  f1_new = update.formula(f1,update_f2)

  f1_new = update.formula(f1_new,.~.-1)

  require_variable = unique(sub("friends_","",all.vars(f1_new)))
  all_require_variable <- data$data[require_variable]

  number_of_network = length(data$network_matrix_list)

  wx_list = vector("list",number_of_network)

  for (i in 1:number_of_network){
    W =  generateWeighting(data$network_matrix_list[[i]]) 
    WX <- as.data.frame(W %*% as.matrix(all_require_variable))
    names(WX) <- paste0("friends_", names(WX) )
    X_and_WX <- cbind(all_require_variable,WX)
    y <- model.response(model.frame(f1_new,X_and_WX))
    X <- model.matrix(f1_new,X_and_WX)
    contain_friends_index <- grep("friends_",colnames(X))
    # replace the friends_ with network name
     wx = X[,contain_friends_index]
     colnames(wx) = gsub("friends", data$network_name[[i]], colnames(wx))
     wx_list[[i]] =wx 
  }

  x = X[,-contain_friends_index]

  wx = do.call(cbind,wx_list)

  wy = sapply(data$network_matrix_list, function(x){
    generateWeighting(x) %*% y
    }) 
  colnames(wy) = "lambda_" %+% colnames(wy)

  X = cbind(x,wx)

  y = demean(y)
  x = demean(x)
  wx = demean(wx)
  wy = demean(wy)


  w2x = demean(W%*%wx)
  return(list(y=y, wy=wy, X=cbind(x,wx), x=x,wx=wx))
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

#' Description genModelData
#' @name genModelData
#' @aliases genModelData
#' @title genModelData
#' @param  .spec
#' @param  save
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 

genModelData <- function(.spec, save=FALSE){
  if (class(.spec)!="SESHK_Spec")
    stop("spec must be an SESHK_Spec object")

   raw_data<-readSeshkNetwork(.version = "../../" %+% .spec$data_version %+% "/")

   data = prepareData(.raw_data=raw_data, .spec=.spec )

   out = lapply(data, extractData, spec=.spec)
   
  if (save){
    cat("Saving data to model_data.rData\n")
    save(out,file="model_data.rData")
  }
  return(out)
}


#' Description genDataMatrix
#' @name genDataMatrix
#' @aliases genDataMatrix
#' @title genDataMatrix
#' @param  data
#' @param  any_wx
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 

genDataMatrix <- function(data, any_wx = TRUE){    
  Y = unlist( sapply(data, function(z) z$y ) )
  WY = do.call( rbind, lapply(data, "[[", "wy") )

  if (any_wx){
    X = do.call(rbind, lapply(data, function(z) z$x_wx ))
  } else {
    X = do.call(rbind, lapply(data, function(z) z$x ))
  }

  n_vector = sapply(data, function(z) z$n )
  X = demean(X)
  Y = demean(Y)
  WY = demean(WY)

  W_list = lapply(data, "[[", "W_list")

  list(
    Y = Y,
    X = X,
    WY = WY,
    W_list = W_list,
    n_vector = n_vector
  )
}

#' Description genNetworkData
#' @name genNetworkData
#' @aliases genNetworkData
#' @title genNetworkData
#' @param  data
#' @return value
#' @author TszKin Julian Chan \email{ctszkin@@gmail.com}
#' @export
#' @examples  
#' \dontrun{
#' 
#' } 

genNetworkData <- function(data){
  lapply(data, function(x){
    location_index1 = lapply(1:x$n, function(z){ which(x$group_index[,1] == z) })
    location_index2 = lapply(1:x$n, function(z){ which(x$group_index[,2] == z) })
    location_index_all = lapply(1:x$n, function(z){ c(location_index1[[z]], location_index2[[z]])} )
    list(location_index1=location_index1,location_index2=location_index2,location_index_all=location_index_all)
  })
}

