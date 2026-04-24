library(data.table)

setwd("Z:/Documents/University/Masters_Research/fmriprep")

dat=read.csv("Preprocessed Encoding and Retrieval Data.csv", header=T)
dat$group=rep(NA, nrow(dat))
dat$group[grepl("sub-5",dat$Participant_id)]="S"
dat$group[grepl("sub-1",dat$Participant_id)]="H"
dat$conditiongroup=rep(NA, nrow(dat))
dat$conditiongroup[dat$group=="S"&dat$Condition=="Encoding"]="S.E"
dat$conditiongroup[dat$group=="S"&dat$Condition=="Retreival"]="S.R"
dat$conditiongroup[dat$group=="H"&dat$Condition=="Encoding"]="H.E"
dat$conditiongroup[dat$group=="H"&dat$Condition=="Retreival"]="H.R"
dat2=dat[c(4,7,8,9:124,167,180)]

demo=read.csv("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Confounding analysis and Case Control/240710_Case_Control_Participants.csv", header=T)

dat3=dat2[dat2$Participant_id%in%demo$participant_id,]
dat3=dat3[order(dat3$Scan_no),]
dat3=dat3[order(dat3$Participant_id),]
dat4=dat3[!is.na(dat3$DVARS),]
write.csv(dat4,"C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/240621_Final Data for Sparsity Manuscript.csv")

##ADD age, gender, DDD, education

SE=dat4[dat4$conditiongroup=="S.E"&dat4$trialtype2=="TASK",]
SR=dat4[dat4$conditiongroup=="S.R"&dat4$trialtype2!="CONTROL",]
HE=dat4[dat4$conditiongroup=="H.E"&dat4$trialtype2=="TASK",]
HR=dat4[dat4$conditiongroup=="H.R"&dat4$trialtype2!="CONTROL",]
part.S=rownames(table(SE$Participant_id))
part.H=rownames(table(HE$Participant_id))

set.seed(1000)

for(i in 1:100){
  SE.s=sample(part.S,45, replace=T)
  SE2=do.call(rbind, lapply(SE.s, function(i) SE[SE$Participant_id == i,]))[,1:119]
  write.csv(SE2, paste("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/SE/SE",i, ".csv", sep=""))
}

set.seed(5000)

for(i in 1:100){
  SR.s=sample(part.S,45, replace=T)
  SR2=do.call(rbind, lapply(SR.s, function(i) SR[SR$Participant_id == i,]))[,1:119]
  write.csv(SR2, paste("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/SR/SR",i, ".csv", sep=""))
}

set.seed(10000)

for(i in 1:100){
  HE.s=sample(part.H,45, replace=T)
  HE2=do.call(rbind, lapply(HE.s, function(i) HE[HE$Participant_id == i,]))[,1:119]
  write.csv(HE2, paste("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/HE/HE",i, ".csv", sep=""))
}

set.seed(15000)

for(i in 1:100){
  HR.s=sample(part.H,45, replace=T)
  HR2=do.call(rbind, lapply(HR.s, function(i) HR[HR$Participant_id == i,]))[,1:119]
  write.csv(HR2, paste("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Bootstrapped_Data/HR/HR",i, ".csv", sep=""))
}
