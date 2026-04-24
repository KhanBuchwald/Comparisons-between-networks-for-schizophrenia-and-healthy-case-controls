library(dbnR)
library(bnlearn)
library(snow)
library(MASS)

files0=list.files("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_edges")

files1a=list.files("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/SE")
files1a=files1a[!files1a%in%files0]
files2a=paste("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/SE/", files1a, sep="")

files1b=list.files("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/SR")
files1b=files1b[!files1b%in%files0]
files2b=paste("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/SR/", files1b, sep="")

files1c=list.files("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/HE")
files1c=files1c[!files1c%in%files0]
files2c=paste("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/HE/", files1c, sep="")

files1d=list.files("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/HR")
files1d=files1d[!files1d%in%files0]
files2d=paste("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/HR/", files1d, sep="")

## List of Functions

merge_nets <- function(net0, netCP1, size, acc = NULL, slice = 1){
  if(size > slice){
    within_t = bnlearn::arcs(net0)
    within_t <- apply(within_t, 2, sub, pattern = "_t_0", replacement = paste0("_t_",slice))
    ret <- merge_nets(net0, netCP1, size, rbind(acc,within_t), slice = slice + 1)
  }
  else
    ret <- rbind(bnlearn::arcs(net0), acc, bnlearn::arcs(netCP1))
  return(ret)
}

## Blacklists

create_blacklist <- function(name, size, acc = NULL, slice = 1){
  # Create the blacklist so that there are no arcs from t to t-1 and within t
  if(size >= slice){
    n <- grep(paste0("t_", (slice-1), "$"), name)
    len <- length(n)
    from <- name[n]
    to = name[-n]
    if((size - slice) > 0)
      fromArc <- as.vector(sapply(from, rep, times = (size - slice) * len, simplify=T))
    else
      fromArc = NULL
    toArc <- rep(to, times = len)
    withinTo <- rep(from, len)
    withinFrom <- as.vector(sapply(from, rep, times = len, simplify = T))
    local_blacklist <- cbind(c(withinFrom, fromArc), c(withinTo, toArc))
    acc <- create_blacklist(to, size, rbind(acc, local_blacklist), slice + 1)
  }
  return(acc)
}

##Hybrid networks

BNStatic <- function(dt, size = 2, f_dt = NULL, blacklist = NULL, intra = TRUE,
                     blacklist_tr = NULL, ...){
  dt_copy <- data.table::copy(dt)
  net0 <- bnlearn::hc(x = dt_copy, ...)
  return(net0)} # Static network

BNtransition <- function(dt, size, f_dt = NULL, blacklist = NULL, intra = TRUE,
                         blacklist_tr = NULL, ...){ 
  blacklist <- create_blacklist(names(f_dt), size)
  net <- bnlearn::hc(x = f_dt, blacklist = blacklist, ...) # Transition network
}

## DBN function


net.fun=function(filenames){

dat0=read.csv(filenames)
filenames2=sub(".*/", "", filenames)

dat=dat0[,c(2:120)]

for(i in c(2:119)){
  dat[,i]=as.numeric((dat[,i]))
}

dat2=as.data.frame(dat)
a=Sys.time()
dat3=dbnR::filtered_fold_dt(dat2, 2, id_var = "Participant_id", clear_id_var = TRUE)
Static1=BNStatic(dat2[,2:ncol(dat2)],2,  dat3, blacklist = NULL, intra = T)
Static2=Static1
Transition=BNtransition(dat2[,2:ncol(dat2)],2,  dat3, blacklist = NULL)
Sys.time()-a
ArcT0=bnlearn::arcs(Static2)
ArcT0=apply(ArcT0, 2, sub, pattern = "$", replacement = paste0("_t_","0"))
ArcT1=bnlearn::arcs(Static1)
ArcT1=apply(ArcT1, 2, sub, pattern = "$", replacement = paste0("_t_","1"))
BNnet=rbind.data.frame(Transition$arcs, ArcT0, ArcT1)
write.csv(BNnet, file=paste("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_edges", filenames2[1], sep="/"))
}

## SE

numcores=makeCluster(12, type="SOCK")

clusterExport(numcores, list = c("BNStatic", "BNtransition","create_blacklist","merge_nets", "net.fun"))

a=Sys.time()
clusterApply(numcores,files2a, net.fun)
Sys.time()-a 

stopCluster(numcores)

## SR

numcores=makeCluster(12, type="SOCK")

clusterExport(numcores, list = c("BNStatic", "BNtransition","create_blacklist","merge_nets", "net.fun"))

a=Sys.time()
clusterApply(numcores,files2b, net.fun)
Sys.time()-a 

stopCluster(numcores)

## HE

numcores=makeCluster(12, type="SOCK")

clusterExport(numcores, list = c("BNStatic", "BNtransition","create_blacklist","merge_nets", "net.fun"))

a=Sys.time()
clusterApply(numcores,files2c, net.fun)
Sys.time()-a 

stopCluster(numcores)

## HR

numcores=makeCluster(12, type="SOCK")

clusterExport(numcores, list = c("BNStatic", "BNtransition","create_blacklist","merge_nets", "net.fun"))

a=Sys.time()
clusterApply(numcores,files2d, net.fun)
Sys.time()-a  

stopCluster(numcores)