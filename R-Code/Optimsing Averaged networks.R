library(network)
library(ggnet)
library(bnlearn)
library(ggplot2)
library(UpSetR)
library(networkDynamic)
library(igraph)
library(GGally)
library(brainconn)
library(tidyr)
library(cowplot)
library(svglite)

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_edges")
files1=list.files()
SE=grep("SE", files1, value=T)
SR=grep("SR", files1, value=T)
HE=grep("HE", files1, value=T)
HR=grep("HR", files1, value=T)

WG_CV_SE=data.frame()
WG_CV_SR=data.frame()
WG_CV_HE=data.frame()
WG_CV_HR=data.frame()

for(i in 1:length(SE)){
  CV=read.csv(SE[i], header=T)
  WG_CV_SE=rbind(WG_CV_SE, CV)
}

for(i in 1:length(SR)){
  CV=read.csv(SR[i], header=T)
  WG_CV_SR=rbind(WG_CV_SR, CV)
}

for(i in 1:length(HE)){
  CV=read.csv(HE[i], header=T)
  WG_CV_HE=rbind(WG_CV_HE, CV)
}

for(i in 1:length(HR)){
  CV=read.csv(HR[i], header=T)
  WG_CV_HR=rbind(WG_CV_HR, CV)
}

BSE=WG_CV_SE[,c(2,3)]
BSR=WG_CV_SR[,c(2,3)]
BHE=WG_CV_HE[,c(2,3)]
BHR=WG_CV_HR[,c(2,3)]
names(BSE)=c("from", "to")
names(BSR)=c("from", "to")
names(BHE)=c("from", "to")
names(BHR)=c("from", "to")
BSE$string=paste(BSE$from, BSE$to, sep=",")
BSR$string=paste(BSR$from, BSR$to, sep=",")
BHE$string=paste(BHE$from, BHE$to, sep=",")
BHR$string=paste(BHR$from, BHR$to, sep=",")

## Optimal Threshold
setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_edges")
nodes=read.csv(SE[1], header=T)
nodes2=unique(c(nodes$from, nodes$to))

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_edges")

dat_cob_fun=function(dat){
  CV=read.csv(dat, header=T)
  arcs1=as.matrix(data.frame(from=CV$from, to=CV$to))
  return(arcs1)
}

list1=lapply(SE,dat_cob_fun)
Finally1=custom.strength(list1, nodes2)
averaged.network(Finally1)

list2=lapply(SR,dat_cob_fun)
Finally2=custom.strength(list2, nodes2)
averaged.network(Finally2)

list3=lapply(HE,dat_cob_fun)
Finally3=custom.strength(list3, nodes2)
averaged.network(Finally3)

list4=lapply(HR,dat_cob_fun)
Finally4=custom.strength(list4, nodes2)
averaged.network(Finally4)

comdat=rbind.data.frame(BSE, BSR, BHE, BHR)
comdat$dataframe=c(rep("BSE", nrow(BSE)), rep("BSR", nrow(BSR)), rep("BHE", nrow(BHE)), rep("BHR", nrow(BHR)))

## Centrality and Network results

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data")
#save.image("230617_Imported_Edges.RData")

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data")
load("230617_Imported_Edges.RData")

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Manuscript_Document/PLoS ONE/241127_Revision")

SE0=return_dataset("BSE", 0)
SR0=return_dataset("BSR", 0)
HE0=return_dataset("BHE", 0)
HR0=return_dataset("BHR", 0)

write.csv(SE0, "Edge_List_Schizophrenia_Encoding.csv", row.names = F)
write.csv(SR0, "Edge_List_Schizophrenia_Retrieval.csv", row.names = F)
write.csv(HE0, "Edge_List_Healthy_Controls_Encoding.csv", row.names = F)
write.csv(HR0, "Edge_List_Healthy_Controls_Retrieval.csv", row.names = F)

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data")

## Network Statistics

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/R/R_Output/240527_Network_Metrics")

## Create dataframes for data entry

mat.1<-mat.4<-mat.5<-mat.6<-mat.7<-cbind.data.frame(dat=c(rep("BSE", 100), rep("BSR", 100), rep("BHE", 100), rep("BHR",100)),PosVal=rep(seq(0,99,1),4))
mat.2<-cbind.data.frame(dat.1=c(rep("BSE", 100), rep("BSR", 100), rep("BSE", 100), rep("BHE",100)),dat.2=c(rep("BHE", 100), rep("BHR", 100), rep("BSR", 100), rep("BHR",100)),PosVal=rep(seq(0,99,1),4))
mat.3<-cbind.data.frame(dat.1=c(rep("BSE", 100), rep("BSR", 100)),dat.2=c(rep("BHE", 100), rep("BHR", 100)),PosVal=rep(seq(0,99,1),2))

## Return_dataset with all edges at threshold, keep the strongest edge if edge is in both directions

return_dataset <- function(dat0, n) {
  dat <- comdat[comdat$dataframe == dat0, ]
  dat$weight=rep(1, nrow(dat))
  
   g <- graph_from_data_frame(cbind.data.frame(from = dat$from, to = dat$to), directed = TRUE)
  
# Convert the igraph object to a data frame of edges

     edge_df0 <- get.data.frame(g, what = "edges")
  
## Obtain edge weights

    edge_df01 <- aggregate(cbind(weight = from) ~ from + to, data = edge_df0, FUN = function(x) length(x))
    edge_df01$weight=as.numeric(edge_df01$weight)
    names(edge_df01)[3] <- "weight"
    
  
#  unique_edges <- retain_unique_edges(edge_df)
  
  filtered_edges <- edge_df01[edge_df01$weight > n, ]
  return(filtered_edges)
}

## Number of arcs

Number_Arcs_N=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  return(nrow(filtered_edges))
}

mat.1$Number_Arcs<-mapply(Number_Arcs_N, mat.1[,1], mat.1[,2])

write.csv(mat.1, "240617_Number_Arcs.csv")

## percent shared edges

Shared_arcs2=function(dat.1, dat.2,n){
  dat1=return_dataset(dat.1,n) 
  dat2=return_dataset(dat.2,n)
  t1=table(paste(dat1$from, dat1$to, sep="_") %in% paste(dat2$from, dat2$to, sep="_"))[["TRUE"]]
  t2=table(paste(dat1$from, dat1$to, sep="_") %in% paste(dat2$from, dat2$to, sep="_"))[["FALSE"]]
  t3=round((t1/(t1+t2))*100,2)
  return(t3)
}

mat.2$result=mapply(Shared_arcs2, mat.2$dat.1, mat.2$dat.2, mat.2$PosVal)

write.csv(mat.2, "240617_Proportion_of_shared_edges.csv")

## Symmetric Difference of edges

Total_Arcs_Not_Shared=function(n,dat.1, dat.2){
  dat1=return_dataset(dat.1,n) 
  dat2=return_dataset(dat.2,n)
  t2=table(paste(dat1$from, dat1$to, sep="_") %in% paste(dat2$from, dat2$to, sep="_"))[["TRUE"]]
  t1=table(paste(dat1$from, dat1$to, sep="_") %in% paste(dat2$from, dat2$to, sep="_"))[["FALSE"]]
  t3=round((t1/(t1+t2))*100,2)
  return(t3)
}

mat.3$result=mapply(Total_Arcs_Not_Shared, mat.3$PosVal, mat.3$dat.1, mat.3$dat.2)

write.csv(mat.3, "240617_Proportion_of_nonshared_edges.csv")

## Betweenness

Closeness_fun=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  filtered_edges2=cbind.data.frame(from=filtered_edges$from, to=filtered_edges$to)
  g <- graph_from_data_frame(filtered_edges2, directed = TRUE)
  return(mean(closeness(g, weights = NULL), na.rm=T))
}

mat.4$result=mapply(Closeness_fun, mat.4[,1], mat.4[,2])
write.csv(mat.4, "240617_Closeness.csv")

## Node degree

Degree_fun=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  filtered_edges2=cbind.data.frame(from=filtered_edges$from, to=filtered_edges$to)
  g <- graph_from_data_frame(filtered_edges2, directed = TRUE)
  return(mean(strength(g, weights = NULL)))
}

mat.5$result=mapply(Degree_fun, mat.5[,1], mat.5[,2])
write.csv(mat.5, "240617_Degree.csv")

## Clustering coefficient

Cluster_fun=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  filtered_edges2=cbind.data.frame(from=filtered_edges$from, to=filtered_edges$to)
  g <- graph_from_data_frame(filtered_edges2, directed = TRUE)
  return(transitivity(g, type = "global"))
}

mat.6$result=mapply(Cluster_fun, mat.6[,1], mat.6[,2])
write.csv(mat.6, "240617_Cluster_Coefficient.csv")

## Shortest Path Length (Unweighted)

Path_Length_fun=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  filtered_edges2=cbind.data.frame(from=filtered_edges$from, to=filtered_edges$to)
  g <- graph_from_data_frame(filtered_edges2, directed = TRUE)
  return(mean_distance(g, directed = T, weights=NULL))
}

mat.7$result=mapply(Path_Length_fun, mat.7[,1], mat.7[,2])
write.csv(mat.7, "240617_Path_Length.csv")

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data")
save.image("230617_Network_statistics.RData")

##Plots

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data")
load("230617_Network_statistics.RData")

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/R/R_Output/240527_Network_Plots")

NetSE25.1=return_dataset("BSE", 24)
NetSE50.1=return_dataset("BSE", 49)
NetSE75.1=return_dataset("BSE", 74)
NetSE95.1=return_dataset("BSE", 94)

NetSR25.1=return_dataset("BSR", 24)
NetSR50.1=return_dataset("BSR", 49)
NetSR75.1=return_dataset("BSR", 74)
NetSR95.1=return_dataset("BSR", 94)

NetHE25.1=return_dataset("BHE", 24)
NetHE50.1=return_dataset("BHE", 49)
NetHE75.1=return_dataset("BHE", 74)
NetHE95.1=return_dataset("BHE", 94)

NetHR25.1=return_dataset("BHR", 24)
NetHR50.1=return_dataset("BHR", 49)
NetHR75.1=return_dataset("BHR", 74)
NetHR95.1=return_dataset("BHR", 94)

pdf("240617_Networks_at_threshold.pdf", width = 15.92, height = 10)

ggnet2(NetSE25.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetSE50.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetSE75.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetSE95.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetSR25.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetSR50.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetSR75.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetSR95.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetHE25.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetHE50.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetHE75.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetHE95.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetHR25.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetHR50.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetHR75.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
ggnet2(NetHR95.1, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=2, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))

dev.off()

## Upset Plot

tiff("240617_UpSetPlot3.tiff", width = 4000, height = 2100, res=600)

listInput <- list(PDS_Encoding=paste0(NetSE50.1[,1],NetSE50.1[,2]), PDS_Retrieval=paste0(NetSR50.1[,1],NetSR50.1[,2]), HC_Encoding=paste0(NetHE50.1[,1],NetHE50.1[,2]), HC_Retrieval=paste0(NetHR50.1[,1],NetHR50.1[,2]))
upset(fromList(listInput), order.by = "freq",text.scale = 1.25)
dev.off()

## Number of edges in each time period and between time periods

NetSE50.1.1 <- NetSE50.1[grep("t_1", NetSE50.1$from), ]
nrow(NetSE50.1.1[grep("t_1", NetSE50.1.1$to),])
nrow(NetSE50.1.1[grep("t_0", NetSE50.1.1$to),])
NetSE50.1.2 <- NetSE50.1[grep("t_0", NetSE50.1$from), ]
nrow(NetSE50.1.2[grep("t_1", NetSE50.1.2$to),])
nrow(NetSE50.1.2[grep("t_0", NetSE50.1.2$to),])


## List fronto-parietal circuit

n1=c("Frontal_Inf_Oper_L_t_0",   "Frontal_Inf_Oper_L_t_1",   "Frontal_Inf_Oper_R_t_0",   "Frontal_Inf_Oper_R_t_1",
     "Frontal_Inf_Orb_L_t_0",    "Frontal_Inf_Orb_L_t_1",    "Frontal_Inf_Orb_R_t_0",    "Frontal_Inf_Orb_R_t_1",   
     "Frontal_Inf_Tri_L_t_0",    "Frontal_Inf_Tri_L_t_1",    "Frontal_Inf_Tri_R_t_0",    "Frontal_Inf_Tri_R_t_1",
     "Frontal_Mid_L_t_0",        "Frontal_Mid_L_t_1",        "Frontal_Mid_R_t_0",        "Frontal_Mid_R_t_1",       
     "Frontal_Mid_Orb_L_t_0",    "Frontal_Mid_Orb_L_t_1",    "Frontal_Mid_Orb_R_t_0",    "Frontal_Mid_Orb_R_t_1",
     "Temporal_Sup_L_t_0",       "Temporal_Sup_L_t_1",       "Temporal_Sup_R_t_0",       "Temporal_Sup_R_t_1"
     )           

## List Default Mode Network circuit

n2=c("Precentral_L_t_0",         "Precentral_L_t_1",        "Precentral_R_t_0",         "Precentral_R_t_1",
     "Precentral_R_t_0",         "Precentral_R_t_1",        "Frontal_Sup_R_t_0",        "Frontal_Sup_R_t_1",
     "Frontal_Sup_Orb_L_t_0",    "Frontal_Sup_Orb_L_t_1",   "Frontal_Sup_Orb_R_t_0",    "Frontal_Sup_Orb_R_t_1",
     "Frontal_Sup_Medial_L_t_0", "Frontal_Sup_Medial_L_t_1","Frontal_Sup_Medial_R_t_0", "Frontal_Sup_Medial_R_t_1",
     "Frontal_Med_Orb_L_t_0",    "Frontal_Med_Orb_L_t_1",   "Frontal_Med_Orb_R_t_0",    "Frontal_Med_Orb_R_t_1",
     "Cingulum_Ant_L_t_0",       "Cingulum_Ant_L_t_1",      "Cingulum_Ant_R_t_0",       "Cingulum_Ant_R_t_1"
) 

## List Sensorimotor-temporal circuit

n3=c("Rolandic_Oper_L_t_0",      "Rolandic_Oper_L_t_1",     "Rolandic_Oper_R_t_0",      "Rolandic_Oper_R_t_1",
     "Postcentral_L_t_0",        "Postcentral_L_t_1",       "Postcentral_R_t_0",        "Postcentral_R_t_1",
     "SupraMarginal_L_t_0",      "SupraMarginal_L_t_1",     "SupraMarginal_R_t_0",      "SupraMarginal_R_t_1",
     "Heschl_L_t_0",             "Heschl_L_t_1",            "Heschl_R_t_0",             "Heschl_R_t_1",
     "Temporal_Sup_L_t_0",       "Temporal_Sup_L_t_1",      "Temporal_Sup_R_t_0",       "Temporal_Sup_R_t_1"
) 

## Edges within FP circuit

Net_SE_FP_Network=NetSE50.1[NetSE50.1$from%in%n1,]
Net_SE_FP_Network2=Net_SE_FP_Network[Net_SE_FP_Network$to%in%n1,]

Net_HE_FP_Network=NetHE50.1[NetHE50.1$from%in%n1,]
Net_HE_FP_Network2=Net_HE_FP_Network[Net_HE_FP_Network$to%in%n1,]

Net_SR_FP_Network=NetSR50.1[NetSR50.1$from%in%n1,]
Net_SR_FP_Network2=Net_SR_FP_Network[Net_SR_FP_Network$to%in%n1,]

Net_HR_FP_Network=NetHR50.1[NetHR50.1$from%in%n1,]
Net_HR_FP_Network2=Net_HR_FP_Network[Net_HR_FP_Network$to%in%n1,]

## Edges within DMN circuit

Net_SE_DMN_Network=NetSE50.1[NetSE50.1$from%in%n2,]
Net_SE_DMN_Network2=Net_SE_DMN_Network[Net_SE_DMN_Network$to%in%n2,]

Net_HE_DMN_Network=NetHE50.1[NetHE50.1$from%in%n2,]
Net_HE_DMN_Network2=Net_HE_DMN_Network[Net_HE_DMN_Network$to%in%n2,]

Net_SR_DMN_Network=NetSR50.1[NetSR50.1$from%in%n2,]
Net_SR_DMN_Network2=Net_SR_DMN_Network[Net_SR_DMN_Network$to%in%n2,]

Net_HR_DMN_Network=NetHR50.1[NetHR50.1$from%in%n2,]
Net_HR_DMN_Network2=Net_HR_DMN_Network[Net_HR_DMN_Network$to%in%n2,]

## Edges within SMT circuit

Net_SE_SMT_Network=NetSE50.1[NetSE50.1$from%in%n3,]
Net_SE_SMT_Network2=Net_SE_SMT_Network[Net_SE_SMT_Network$to%in%n3,]

Net_HE_SMT_Network=NetHE50.1[NetHE50.1$from%in%n3,]
Net_HE_SMT_Network2=Net_HE_SMT_Network[Net_HE_SMT_Network$to%in%n3,]

Net_SR_SMT_Network=NetSR50.1[NetSR50.1$from%in%n3,]
Net_SR_SMT_Network2=Net_SR_SMT_Network[Net_SR_SMT_Network$to%in%n3,]

Net_HR_SMT_Network=NetHR50.1[NetHR50.1$from%in%n3,]
Net_HR_SMT_Network2=Net_HR_SMT_Network[Net_HR_SMT_Network$to%in%n3,]

## ALL atlas numbers

aal_numbers=read.table("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Manuscript_Document/PLoS ONE/241127_Revision/aal116NodeNames.txt")
aal_numbers$V2=seq(1,116,1)

Obtain_adjacency_matrix_static=function(net){
  
  from <- lapply(net$from, function(x) {
    split_name <- strsplit(x, "_")[[1]]
    time_point <- paste0("t_", split_name[length(split_name)])
    return(time_point)
  })
  
  to <- lapply(net$to, function(x) {
    split_name <- strsplit(x, "_")[[1]]
    time_point <- paste0("t_", split_name[length(split_name)])
    return(time_point)
  })
  
  from_to=cbind.data.frame(from=unlist(from),to=unlist(to))
  
  ## rownumbers for each time period
  
  static=which(from_to$from=="t_1"&from_to$to=="t_1")
  #transition=as.numeric(rownames(from_to))[!as.numeric(rownames(from_to))%in%c(time1, time2)]
  
  ## t1 aal numbers
  
  aal_numbers2=aal_numbers
  aal_numbers2$V1=paste(aal_numbers$V1, "t_1", sep="_")
  
  Net_time1=net[static,]
  
  ## brain through brain region ID
  
  Net_SE_FP_Network3=merge(Net_time1, aal_numbers2, by.x="from", by.y="V1", all.x=T)
  Net_SE_FP_Network4=merge(Net_SE_FP_Network3, aal_numbers2, by.x="to", by.y="V1", all.x=T)
  
  ## Order dataset by brain region number
  
  Net_SE_FP_Network4=Net_SE_FP_Network4[order(Net_SE_FP_Network4$to, decreasing = T),]
  Net_SE_FP_Network4=Net_SE_FP_Network4[order(Net_SE_FP_Network4$from, decreasing = T),]
  
  ## Remove brain regions in character format
  
  Net_SE_FP=Net_SE_FP_Network4[,c(4,5,3)]
  
  ## create wide dataframe
  
  Net_SE_FP2=as.data.frame(t(as.data.frame(pivot_wider(Net_SE_FP, names_from ="V2.x", values_from = "weight", id_cols = "V2.y"))))
  
  ## Specify rownames
  
  names(Net_SE_FP2)=Net_SE_FP2[1,]
  
  ## delete rowname column
  
  Net_SE_FP2=Net_SE_FP2[-c(1),]
  
  # Identify names not included
  
  s1=seq(1,116,1)[!seq(1,116,1)%in%colnames(Net_SE_FP2)]
  s2=seq(1,116,1)[!seq(1,116,1)%in%rownames(Net_SE_FP2)]
  
  ## Create empty dataframe
  
  df=as.data.frame(matrix(rep(NA, nrow(Net_SE_FP2)*length(s1)), nrow = nrow(Net_SE_FP2), ncol=length(s1)))
  
  ## provide colnames
  
  names(df)=s1
  
  ## Combine dataframes
  
  Net_SE_FP3=cbind.data.frame(Net_SE_FP2, df)
  Net_SE_FP4=Net_SE_FP3
  
  ## Add rows
  
  Net_SE_FP4[(nrow(Net_SE_FP3)+1):116,]=NA
  
  ## rename rows
  
  rownames(Net_SE_FP4)[(nrow(Net_SE_FP3)+1):116]=s2
  
  ## Order rows and columns
  
  Net_SE_FP4=Net_SE_FP4[,order(as.numeric(colnames(Net_SE_FP4)))]
  Net_SE_FP4=Net_SE_FP4[order(as.numeric(rownames(Net_SE_FP4))),]
  
  ## Change NA to zero and make it unweighted
  
  Net_SE_FP4[is.na(Net_SE_FP4)]=0
  Net_SE_FP4[Net_SE_FP4>0]=1
  return(Net_SE_FP4)
}
Obtain_adjacency_matrix_transition=function(net){
  
  from <- lapply(net$from, function(x) {
    split_name <- strsplit(x, "_")[[1]]
    time_point <- paste0("t_", split_name[length(split_name)])
    return(time_point)
  })
  
  to <- lapply(net$to, function(x) {
    split_name <- strsplit(x, "_")[[1]]
    time_point <- paste0("t_", split_name[length(split_name)])
    return(time_point)
  })
  
  from_to=cbind.data.frame(from=unlist(from),to=unlist(to))
  
  ## rownumbers for each time period
  
  #static=which(from_to$from=="t_1"&from_to$to=="t_1")
  transition=which(from_to$from=="t_1"& from_to$to=="t_0")
  
  ## t1 aal numbers
  
  aal_numbers2=aal_numbers
  aal_numbers2=rbind.data.frame(aal_numbers2,aal_numbers2)
  aal_numbers2$V1[1:116]=paste(aal_numbers2$V1[1:116], "t_1", sep="_")
  aal_numbers2$V1[117:232]=paste(aal_numbers2$V1[117:232], "t_0", sep="_")
  
  Net_time1=net[transition,]
  
  ## brain through brain region ID
  
  Net_SE_FP_Network3=merge(Net_time1, aal_numbers2, by.x="from", by.y="V1", all.x=T)
  Net_SE_FP_Network4=merge(Net_SE_FP_Network3, aal_numbers2, by.x="to", by.y="V1", all.x=T)
  
  ## Order dataset by brain region number
  
  Net_SE_FP_Network4=Net_SE_FP_Network4[order(Net_SE_FP_Network4$to, decreasing = T),]
  Net_SE_FP_Network4=Net_SE_FP_Network4[order(Net_SE_FP_Network4$from, decreasing = T),]
  
  ## Remove brain regions in character format
  
  Net_SE_FP=Net_SE_FP_Network4[,c(4,5,3)]
  
  ## create wide dataframe
  
  Net_SE_FP2=as.data.frame(t(as.data.frame(pivot_wider(Net_SE_FP, names_from ="V2.x", values_from = "weight", id_cols = "V2.y"))))
  
  ## Specify rownames
  
  names(Net_SE_FP2)=Net_SE_FP2[1,]
  
  ## delete rowname column
  
  Net_SE_FP2=Net_SE_FP2[-c(1),]
  
  # Identify names not included
  
  s1=seq(1,116,1)[!seq(1,116,1)%in%colnames(Net_SE_FP2)]
  s2=seq(1,116,1)[!seq(1,116,1)%in%rownames(Net_SE_FP2)]
  
  ## Create empty dataframe
  
  df=as.data.frame(matrix(rep(NA, nrow(Net_SE_FP2)*length(s1)), nrow = nrow(Net_SE_FP2), ncol=length(s1)))
  
  ## provide colnames
  
  names(df)=s1
  
  ## Combine dataframes
  
  Net_SE_FP3=cbind.data.frame(Net_SE_FP2, df)
  Net_SE_FP4=Net_SE_FP3
  
  ## Add rows
  
  Net_SE_FP4[(nrow(Net_SE_FP3)+1):116,]=NA
  
  ## rename rows
  
  rownames(Net_SE_FP4)[(nrow(Net_SE_FP3)+1):116]=s2
  
  ## Order rows and columns
  
  Net_SE_FP4=Net_SE_FP4[,order(as.numeric(colnames(Net_SE_FP4)))]
  Net_SE_FP4=Net_SE_FP4[order(as.numeric(rownames(Net_SE_FP4))),]
  
  ## Change NA to zero and make it unweighted
  
  Net_SE_FP4[is.na(Net_SE_FP4)]=0
  Net_SE_FP4[Net_SE_FP4>0]=1
  return(Net_SE_FP4)
}

## Static

SE_FP_S=Obtain_adjacency_matrix_static(Net_SE_FP_Network2)
SR_FP_S=Obtain_adjacency_matrix_static(Net_SR_FP_Network2)
HE_FP_S=Obtain_adjacency_matrix_static(Net_HE_FP_Network2)
HR_FP_S=Obtain_adjacency_matrix_static(Net_HR_FP_Network2)

SE_DMN_S=Obtain_adjacency_matrix_static(Net_SE_DMN_Network2)
SR_DMN_S=Obtain_adjacency_matrix_static(Net_SR_DMN_Network2)
HE_DMN_S=Obtain_adjacency_matrix_static(Net_HE_DMN_Network2)
HR_DMN_S=Obtain_adjacency_matrix_static(Net_HR_DMN_Network2)

SE_SMT_S=Obtain_adjacency_matrix_static(Net_SE_SMT_Network2)
SR_SMT_S=Obtain_adjacency_matrix_static(Net_SR_SMT_Network2)
HE_SMT_S=Obtain_adjacency_matrix_static(Net_HE_SMT_Network2)
HR_SMT_S=Obtain_adjacency_matrix_static(Net_HR_SMT_Network2)

## Transition

SE_FP_T=Obtain_adjacency_matrix_transition(Net_SE_FP_Network2)
SR_FP_T=Obtain_adjacency_matrix_transition(Net_SR_FP_Network2)
HE_FP_T=Obtain_adjacency_matrix_transition(Net_HE_FP_Network2)
HR_FP_T=Obtain_adjacency_matrix_transition(Net_HR_FP_Network2)

SE_DMN_T=Obtain_adjacency_matrix_transition(Net_SE_DMN_Network2)
SR_DMN_T=Obtain_adjacency_matrix_transition(Net_SR_DMN_Network2)
HE_DMN_T=Obtain_adjacency_matrix_transition(Net_HE_DMN_Network2)
HR_DMN_T=Obtain_adjacency_matrix_transition(Net_HR_DMN_Network2)

SE_SMT_T=Obtain_adjacency_matrix_transition(Net_SE_SMT_Network2)
SR_SMT_T=Obtain_adjacency_matrix_transition(Net_SR_SMT_Network2)
HE_SMT_T=Obtain_adjacency_matrix_transition(Net_HE_SMT_Network2)
HR_SMT_T=Obtain_adjacency_matrix_transition(Net_HR_SMT_Network2)

## Number of edges in transitio networks

#table(SE_FP_T==1)+table(SR_FP_T==1)+table(HE_FP_T==1)+table(HR_FP_T==1)+table(SE_DMN_T==1)+table(SR_DMN_T==1)+table(HE_DMN_T==1)+
#table(HR_DMN_T==1)+table(SE_SMT_T==1)+table(SR_SMT_T==1)+table(HE_SMT_T==1)+table(HR_SMT_T==1)

## One edge of 133 in transition network that is between two different nodes

## Visualise DMN

p1=brainconn(atlas = "aal116", conmat = SE_FP_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p2=brainconn(atlas = "aal116", conmat = SR_FP_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p3=brainconn(atlas = "aal116", conmat = HE_FP_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p4=brainconn(atlas = "aal116", conmat = HR_FP_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p5=brainconn(atlas = "aal116", conmat = SE_DMN_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p6=brainconn(atlas = "aal116", conmat = SR_DMN_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p7=brainconn(atlas = "aal116", conmat = HE_DMN_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p8=brainconn(atlas = "aal116", conmat = HR_DMN_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p9=brainconn(atlas = "aal116", conmat = SE_SMT_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p10=brainconn(atlas = "aal116", conmat = SR_SMT_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p11=brainconn(atlas = "aal116", conmat = HE_SMT_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
p12=brainconn(atlas = "aal116", conmat = HR_SMT_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)

p1.1=brainconn(atlas = "aal116", conmat = SE_FP_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p2.1=brainconn(atlas = "aal116", conmat = SR_FP_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p3.1=brainconn(atlas = "aal116", conmat = HE_FP_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p4.1=brainconn(atlas = "aal116", conmat = HR_FP_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p5.1=brainconn(atlas = "aal116", conmat = SE_DMN_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p6.1=brainconn(atlas = "aal116", conmat = SR_DMN_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p7.1=brainconn(atlas = "aal116", conmat = HE_DMN_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p8.1=brainconn(atlas = "aal116", conmat = HR_DMN_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p9.1=brainconn(atlas = "aal116", conmat = SE_SMT_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p10.1=brainconn(atlas = "aal116", conmat = SR_SMT_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p11.1=brainconn(atlas = "aal116", conmat = HE_SMT_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)
p12.1=brainconn(atlas = "aal116", conmat = HR_SMT_S, view="left", show.legend=FALSE, labels=FALSE, edge.width = .5)


first_column <- plot_grid(
  p1, p1,
  ncol = 2,               # Stack top and bottom
  rel_heights = c(.5, .5)   # Equal height for both plots
)

second_column <- plot_grid(
  p1.1, p1.1,
  ncol = 1,               # Stack top and bottom
  rel_heights = c(.5, .5)   # Equal height for both plots
)

svglite("check_top.svg", width = 10, height=10)
plot_grid(
  p1, p1,
  ncol = 2,               # Stack top and bottom
  rel_heights = c(.5, .5) 
)
dev.off()

svglite("check_left.svg", width = 10, height=10)
plot_grid(
  p1.1, p1.1,
  ncol = 1,               # Stack top and bottom
  rel_heights = c(.5, .5) 
)
dev.off()

svglite("check_left.svg", width = 10, height=10)
plot_grid(
  p1, p1.1,
  ncol = 2,               # Stack top and bottom
  rel_heights = c(.5, .5) 
)
dev.off()

svglite("Network_Visualisation_2.svg", width = 10, height=10)
plot_grid(
  p1, p1.1,
  ncol = 2,               # Stack top and bottom
  rel_heights = c(.5, .5) 
)
dev.off()

svglite("Network_Visualisation_3.svg", width = 10, height=10)
brainconn(atlas = "aal116", conmat = SE_FP_S, view="ortho", show.legend=FALSE, labels=FALSE, edge.width = .5)
dev.off()

plot_1=ggnet2(Net_SE_FP_Network2, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=1.5, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
plot_2=ggnet2(Net_SR_FP_Network2, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=1.5, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
plot_3=ggnet2(Net_HE_FP_Network2, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=1.5, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))
plot_4=ggnet2(Net_HR_FP_Network2, arrow.size = 1, arrow.gap=.0025, label = TRUE, label.size=1.5, node.size = 4, node.color = "black", label.color = "red")+theme(plot.title=element_text(hjust=0.5))

ggsave("240624_FP_Circuit-1.tiff", plot_1, width = 7.5, height = 7.5, units = "in")
ggsave("240624_FP_Circuit-2.tiff", plot_2, width = 7.5, height = 7.5, units = "in")
ggsave("240624_FP_Circuit-3.tiff", plot_3, width = 7.5, height = 7.5, units = "in")
ggsave("240624_FP_Circuit-4.tiff", plot_4, width = 7.5, height = 7.5, units = "in")

nrow(Net_HE_FP_Network2[paste(Net_HE_FP_Network2$from, Net_HE_FP_Network2$to)%in%paste(Net_SE_FP_Network2$from, Net_SE_FP_Network2$to),])
nrow(Net_HR_FP_Network2[paste(Net_HR_FP_Network2$from, Net_HR_FP_Network2$to)%in%paste(Net_SR_FP_Network2$from, Net_SE_FP_Network2$to),])
Net_HR_FP_Network2[!paste(Net_HR_FP_Network2$from, Net_HR_FP_Network2$to)%in%paste(Net_SR_FP_Network2$from, Net_SE_FP_Network2$to),]


## List DMN circuit