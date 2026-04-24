library(dbnR)
library(lmtest)
library(car)
library(igraph)

## Linearity of predictors

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data")
load("230617_Imported_Edges.RData")

## Network Statistics

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/R/R_Output/240527_Network_Metrics")

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


SE=return_dataset("BSE", 0)
SR=return_dataset("BSR", 0)
HE=return_dataset("BHE", 0)
HR=return_dataset("BHR", 0)

fmridat=read.csv("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/240621_Final Data for Sparsity Manuscript.csv")

fmridat=fmridat[order(fmridat$X),]
fmridat=fmridat[order(fmridat$Participant_id),]

fmri.SE=fmridat[fmridat$conditiongroup=="S.E"&fmridat$trialtype2=="TASK",]
fmri.SR=fmridat[fmridat$conditiongroup=="S.R"&fmridat$trialtype2!="CONTROL",]
fmri.HE=fmridat[fmridat$conditiongroup=="H.E"&fmridat$trialtype2=="TASK",]
fmri.HR=fmridat[fmridat$conditiongroup=="H.R"&fmridat$trialtype2!="CONTROL",]

fmri.SE=fmri.SE[,2:120]
fmri.SR=fmri.SR[,2:120]
fmri.HE=fmri.HE[,2:120]
fmri.HR=fmri.HR[,2:120]

fmri.SE2=as.data.frame(filtered_fold_dt(fmri.SE, 2, id_var = "Participant_id", clear_id_var = F))
fmri.SR2=as.data.frame(filtered_fold_dt(fmri.SR, 2, id_var = "Participant_id", clear_id_var = TRUE))
fmri.HE2=as.data.frame(filtered_fold_dt(fmri.HE, 2, id_var = "Participant_id", clear_id_var = TRUE))
fmri.HR2=as.data.frame(filtered_fold_dt(fmri.HR, 2, id_var = "Participant_id", clear_id_var = TRUE))

t1=table(SE$to)
t2=t1
test2=NA
time1=Sys.time()

Linearity=function(data,transform1){
  for(i in 1:length (t2)){
    response_var=data[names(t2)[i]]
    predictors <- paste(SE$from[SE$to %in% names(t2)[i]], collapse = "+")
    formula <- as.formula(paste(names(t2)[i], "~", predictors))
    
    dat2=data[,c(names(response_var), SE$from[SE$to %in% names(t2)[i]])]
    
    for(j in 1:ncol(dat2)){
      sign=dat2[,j]>0
      dat2[,j]=round(abs(dat2[,j])^(transform1),4)
      dat2[,j][sign==FALSE]=-dat2[,j][sign==FALSE]
    }
    
    #response_var=names(t2)[1]
    
    mod=lm(formula, data = dat2)
    test1=raintest(mod)$statistic
    test2=c(test1, test2)
  }
  return(test2)
}

v1=Linearity(fmri.SE2, 1)
v2=Linearity(fmri.SR2, 1)
v3=Linearity(fmri.HE2, 1)
v4=Linearity(fmri.HR2, 1)

mean(test2, na.rm=T)

t1=table(SE$to)
t2=t1
test2=0
time1=Sys.time()


correct_transform=function(transform1){
  for(i in 1:length (t2)){
    response_var=fmri.SE2[names(t2)[i]]
    predictors <- paste(SE$from[SE$to %in% names(t2)[i]], collapse = "+")
    formula <- as.formula(paste(names(t2)[i], "~", predictors))
    
    dat2=fmri.SE2[,c(names(response_var), SE$from[SE$to %in% names(t2)[i]])]
    
    for(j in 1:ncol(dat2)){
      sign=dat2[,j]>0
      dat2[,j]=round(abs(dat2[,j])^(transform1),4)
      dat2[,j][sign==FALSE]=-dat2[,j][sign==FALSE]
    }
    
    #response_var=names(t2)[1]
    
    mod=lm(formula, data = dat2)
    test1=raintest(mod)$statistic
    test2=sum(test1, test2)
  }
  return(test2)
}
optim(par=.5, correct_transform, method = "Nelder-Mead",control = list(trace=1))
Sys.time()-time1

acf(fmri.SE2[names(t1)[1]], plot = T)
