library(SESHK2011)
library(Jmisc)
library(foreach)
library(foreign)

.spec = genSpecStandardMath()
data = genModelData(.spec)
colnames(data$kslo$self_data_matrix)

head(data$kslo$self_data_matrix )




# head(data$kslo$self_data_matrix[,27:35] )


# .version="SESHK2011 - network - 0.8.0"

# path<-    .version %+% "/"
# spec<-readSpecification(path)

# i=j=school_name=NULL

# # read network data
# all_network_data<-  
# foreach(i = getFileName("network",spec),.combine=c ) %do% {
#   out <- read.dta(path %+% "network/" %+% i )  
#   out<-list(out)
#   names(out)<-substr(i,1,nchar(i)-4)
#   out
# }

