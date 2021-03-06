## require SESHK2011_0.3.3
## Graph are generated by Erdős–Rényi model using the number of edges of observed graph. Only the total number of edges are preserved 

counterfactual_mod_erdos_renyi = function(network_matrix_list, data_wide, spec){
	i = NULL
	G = length(network_matrix_list)
	n = nrow(network_matrix_list[[1]])
	degree_table = lapply(network_matrix_list, rowSums )
	prob_table = lapply(degree_table, table)
	total_edges = sapply(degree_table , sum)

	new_network_matrix_list = 
		foreach(i = total_edges) %do% {
			as.matrix(igraph::get.adjacency( igraph::erdos.renyi.game(n=n, i, "gnm") ))
		}
	names(new_network_matrix_list) = names(network_matrix_list)
	new_network_matrix_list
}


## Using permutation to construct the network. Degree distribution and correlation of network are preserved

counterfactual_mod_permutation = function(network_matrix_list, data_wide, spec){
	G = length(network_matrix_list)
	n = nrow(network_matrix_list[[1]])

	M = as.matrix(as(sample(1:n),"pMatrix"))
	tM = t(M)
	new_network_matrix_list = network_matrix_list

	for(i in 1:G) {
		new_network_matrix_list[[i]] = M %*% network_matrix_list[[i]] %*% tM
	}
	new_network_matrix_list
}

counterfactual_mod_empty_network = function(network_matrix_list, data_wide, spec){
	G = length(network_matrix_list)
	n = nrow(network_matrix_list[[1]])

	new_network_matrix_list = network_matrix_list

	for(i in 1:G) {
		new_network_matrix_list[[i]] = matrix(0,n,n)
	}
	new_network_matrix_list
}
