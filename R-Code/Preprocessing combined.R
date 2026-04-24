
library(fmri)
#library(FIAR)
library(data.table)
library(oro.nifti)
library(label4MRI)
library(RNifti)
library(zoo)
library(R.utils)

## GUNZIP

setwd("Z:/Documents/University/Masters_Research/fmriprep/Filtered_Encoding")

files1=list.files()

for(i in 1:length(files1)){
  gunzip(files1[i], ext="gz", FUN=gzfile)
}

setwd("Z:/Documents/University/Masters_Research/fmriprep/Filtered_Retrieval")

files1=list.files()

for(i in 1:length(files1)){
  gunzip(files1[i], ext="gz", FUN=gzfile)
}

## Parcellation

dat=readNIfTI("Z:/Documents/University/Masters_Research/fmriprep/Filtered_Encoding/sub-10506_task-pamenc_bold_space-MNI152NLin2009cAsym_preproc_regressed_filtered.nii.gz")
dat4=expand.grid(seq(1,65,1), seq(1,77,1), seq(1,49,1))
names(dat4)=c("x","y","z")
dat5=matrix(voxelToWorld(as.matrix(dat4), dat), ncol=3)
a=Sys.time()
dat7=apply(dat5, 1, function(x)mni_to_region_name(x[1], x[2], x[3], distance=F)$aal.label)
Sys.time()-a
setwd("Z:/Documents/University/Masters_Research/fmriprep")
write.csv(dat7, "Ordered Brain Regions Post Parcellation .csv")
write.csv(dat5, "MNI Coroodinates.csv")
write.csv(dat4, "Voxel number.csv")

## Reorganising the data

files.enc=list.files(path="Z:/Documents/University/Masters_Research/fmriprep/Filtered_Encoding")
files.enc2=paste("Z:/Documents/University/Masters_Research/fmriprep/Filtered_Encoding", files.enc, sep="/")
files.enc3.1=paste("Z:/Documents/University/Masters_Research/fmriprep/RData_Encoding", files.enc, sep="/")
files.enc3=gsub(".nii",".rData", x=files.enc3.1)
files.ret=list.files(path="Z:/Documents/University/Masters_Research/fmriprep/Filtered_Retrieval")
files.ret2=paste("Z:/Documents/University/Masters_Research/fmriprep/Filtered_Retrieval", files.ret, sep="/")
files.ret3.1=paste("Z:/Documents/University/Masters_Research/fmriprep/RData_Retrieval", files.ret, sep="/")
files.ret3=gsub(".nii",".rData", x=files.ret3.1)
for (i in 1:length(files.enc2)){
  dat=read.NIFTI(files.enc2[i])
  dat2=as.vector(extractData(dat))
  save(dat2,file=files.enc3[i])
}
for (i in 1:length(files.ret2)){
  dat=read.NIFTI(files.ret2[i])
  dat2=as.vector(extractData(dat))
  save(dat2,file=files.ret3[i])
}

## Smoothing

files.enc=dir(path="Z:/Documents/University/Masters_Research/fmriprep/RData_Encoding", pattern="rData")
files.enc2=paste("Z:/Documents/University/Masters_Research/fmriprep/RData_Encoding", files.enc, sep="/")
regions=as.data.frame(read.csv("Z:/Documents/University/Masters_Research/fmriprep/Ordered Brain Regions Post Parcellation.csv"))
seq.1=seq(1,242,1)
rep.1=sort(rep(seq.1, 245245))
y=data.frame()
regions2=rep(regions$x, 242)
a=Sys.time()
for(i in 1:127){
  load(files.enc2[i])
  dat3=cbind.data.frame(dat2, regions2, rep.1)
  dat4=cbind.data.frame(i, seq(1,242,1),tapply(dat3$dat2, list( dat3$rep.1,dat3$regions2), mean))
  y=rbind(y, dat4)
}
Sys.time()-a
files.ret=dir(path="Z:/Documents/University/Masters_Research/fmriprep/RData_Retrieval", pattern="rData")
files.ret2=paste("Z:/Documents/University/Masters_Research/fmriprep/RData_Retrieval", files.ret, sep="/")
y.ret=data.frame()
seq.1=seq(1,268,1)
rep.1=sort(rep(seq.1, 245245))
regions2=rep(regions$x, 268)
a=Sys.time()
for(i in 1:127){
  load(files.ret2[i])
  dat3=cbind.data.frame(dat2, regions2, rep.1)
  dat4=cbind.data.frame(i, seq(1,268,1),tapply(dat3$dat2, list( dat3$rep.1,dat3$regions2), mean))
  y.ret=rbind(y.ret, dat4)
}
Sys.time()-a
setwd("Z:/Documents/University/Masters_Research/fmriprep/")
save(y,file= "Encoding data reformatted for DBN.rData")
save(y.ret,file= "Retrieval data reformatted for DBN.rData")

##DVARS Encoding

diff=c()
diff2=c()
DVARS=c()
y2=y[,-c(65)]


  for(j in 1:max(y$i)){
    dat2=y2[y2$i==j,]
     for(i in 2:max(dat2$`seq(1, 242, 1)`)){
       for(l in 3:118){
           diff[l-2]=(dat2[i,l]-dat2[i-1,l])^2}
    diff2[i]=sqrt(mean(diff))
     }
    DVARS=c( DVARS,diff2)
  }
y3=cbind.data.frame(DVARS, y2)

##DVARS Retrieval

diff=c()
diff2=c()
DVARS=c()

y.ret2=y.ret[,-c(65)]

for(j in 1:max(y.ret2$i)){
  dat2=y.ret2[y.ret2$i==j,]
  for(i in 2:max(dat2$`seq(1, 268, 1)`)){
    for(l in 3:118){
      diff[l-2]=(dat2[i,l]-dat2[i-1,l])^2}
    diff2[i]=sqrt(mean(diff))
  }
  DVARS=c(DVARS,diff2)
}
y.ret3=cbind.data.frame(DVARS, y.ret2)

setwd("Z:/Documents/University/Masters_Research/fmriprep/")
save(y3,file= "Encoding data reformatted for DBN.rData")
save(y.ret3,file= "Retrieval data reformatted for DBN.rData")


##Merging

scan.enc=list.files("Z:/Documents/University/Massey University/161.893/Orginal Data/Events data/Encoding")
scan.ret=list.files("Z:/Documents/University/Massey University/161.893/Orginal Data/Events data/Retrieval")
scan.enc2=paste("Z:/Documents/University/Massey University/161.893/Orginal Data/Events data/Encoding", scan.enc, sep="/")
scan.ret2=paste("Z:/Documents/University/Massey University/161.893/Orginal Data/Events data/Retrieval", scan.ret, sep="/")
encode.scan=data.frame()
for(i in 1:length(scan.enc2)){
  dat=read.table(scan.enc2[i], sep="\t", header=T)
  encode.scan=rbind(encode.scan, dat)
}
encode.scan$Participant=sort(rep(seq(1,127,1),64))
Retrieve.scan=data.frame()
for(i in 1:length(scan.ret2)){
  dat=read.table(scan.ret2[i], sep="\t", header=T)
  Retrieve.scan=rbind(Retrieve.scan, dat)
}
Retrieve.scan$Participant=sort(rep(seq(1,127,1),104))
encode.scan$twosec=ceiling(encode.scan$onset/2)*2
Retrieve.scan$twosec=ceiling(Retrieve.scan$onset/2)*2

Retrieve.scan$response_str3=Retrieve.scan$response_str
Retrieve.scan$response_str3[Retrieve.scan$response_str=="n/a"]="CONTROL"
Retrieve.scan$response_str3[Retrieve.scan$response_str=="SURE_CORRECT"]="CORRECTLY"
Retrieve.scan$response_str3[Retrieve.scan$response_str=="MAYBE_CORRECT"]="CORRECTLY"
Retrieve.scan$response_str3[Retrieve.scan$response_str=="MAYBE_INCORRECT"]="INCORRECTLY"
Retrieve.scan$response_str3[Retrieve.scan$response_str=="SURE_INCORRECT"]="INCORRECTLY"
Retrieve.scan$response_str4=Retrieve.scan$response_str3==Retrieve.scan$trial_type
Retrieve.scan$response_str4=as.numeric(Retrieve.scan$response_str4)
pt1=cbind.data.frame(Participant=seq(1,127,1),Propincorrect=prop.table(table(Retrieve.scan$response_str4, Retrieve.scan$Participant),2)[1,])
Retrieve.scan=merge(Retrieve.scan, pt1, by="Participant", all.x=T)
encode.scan=merge(encode.scan, pt1, by="Participant", all.x = T)

load("Z:/Documents/University/Masters_Research/fmriprep/Encoding data reformatted for DBN.rDAta")
load("Z:/Documents/University/Masters_Research/fmriprep/Retrieval data reformatted for DBN.rDAta")
demo=read.csv("Z:/Documents/University/Massey University/161.893/Orginal Data/Demographics analysis/Demographics Combined.csv")
enc=merge(y3, demo, by.x="i", by.y = "IDNUmb", all.x=T)
ret=merge(y.ret3, demo, by.x="i", by.y = "IDNUmb", all.x=T)
enc$acquisition.time=rep(seq(0,482,2), 127)
ret$acquisition.time=rep(seq(0,534,2), 127)

encodingfinal=merge(enc, encode.scan, by.x=c("i", "acquisition.time"), by.y=c("Participant", "twosec"), all.x=T)
Retrievalfinal=merge(ret, Retrieve.scan, by.x=c("i", "acquisition.time"), by.y=c("Participant", "twosec"), all.x=T)
encodingfinal.1=encodingfinal[c(121, 1:120, 122:163)]
Retrievalfinal.1=Retrievalfinal[c(121, 1:120, 122:159)]
names(encodingfinal.1)=c("Participant_id", "Participant_number", "Acquisition_time", "DVARS","Scan_no", names(encodingfinal.1[c(6:163)]))
names(Retrievalfinal.1)=c("Participant_id", "Participant_number", "Acquisition_time","DVARS","Scan_no", names(Retrievalfinal.1[c(6:159)]))
setwd("Z:/Documents/University/Masters_Research/fmriprep/")
save(encodingfinal.1, file="Final Encoding dataset.rData")
save(Retrievalfinal.1, file="Final Retrieval dataset.rData")
write.csv(encodingfinal.1, "Encoding.csv")
write.csv(Retrievalfinal.1, "Retrieval.csv")

##Event data recoded

setwd("Z:/Documents/University/Masters_Research/fmriprep/")

enc=read.csv("Encoding.csv", header=T)
ret=read.csv("Retrieval.csv", header=T)
#names(ret)=c(names(ret[c(1:149)]),"reaction_time", "response_coded",	"response_str",	"pairno_rec",	"enc_codes_during_rec",	"onset_noTriggerAdjust")
#names(enc)=c(names(enc[c(1:159)]),"onset_noTriggerAdjust",	"onset_probe_noTriggerAdjust")
enc$roworder=seq(1,30734, 1)
ret$roworder=seq(1,34036, 1)
enc$trial_type[enc$Acquisition_time==0]="Firstob"
ret$trial_type[ret$Acquisition_time==0]="Firstob"
enc$trialtype2=na.locf(enc$trial_type)
ret$trialtype2=na.locf(ret$trial_type)
enc$Propincorrect[enc$Acquisition_time==0]="Firstob"
ret$Propincorrect[ret$Acquisition_time==0]="Firstob"
enc$Propincorrect=na.locf(enc$Propincorrect)
ret$Propincorrect=na.locf(ret$Propincorrect)
ret$response_str[ret$Acquisition_time==0]="Firstob"
ret$response_str2=na.locf(ret$response_str)
enc$onsetlast=NA
ret$onsetlast=NA
enc$onsetlast[c(2:30734)]=na.locf(enc$onset[c(2:30734)])
ret$onsetlast[c(2:34036)]=na.locf(ret$onset[c(2:34036)])
enc$timesincelasttrial=enc$Acquisition_time-enc$onsetlast
ret$timesincelasttrial=ret$Acquisition_time-ret$onsetlast
enc$consectuvienas=enc$onset
ret$consectuvienas=ret$onset
enc$consectuvienas[is.na(enc$consectuvienas)]=0
ret$consectuvienas[is.na(ret$consectuvienas)]=0
enc$scansinceencode=sequence(rle(as.character(enc$consectuvienas))$lengths)
ret$scansinceencode=sequence(rle(as.character(ret$consectuvienas))$lengths)
enc$nearest5=round(enc$timesincelasttrial*2)/2
ret$nearest5=round(ret$timesincelasttrial*2)/2
enctrialnumber=enc$roworder[!is.na(enc$onset_probe)]
rettrialnumber=ret$roworder[!is.na(ret$onset)]
enctrialnumber2=cbind.data.frame(enctrialnumber, trialnumber=c(seq(1,5248,1), seq(1,2880,1)))
rettrialnumber2=cbind.data.frame(rettrialnumber, trialnumber=c(seq(1,8528,1), seq(1,4680,1)))
enc2=merge(enc, enctrialnumber2, by.x="onset_probe", by.y="enctrialnumber",all.x=T)
ret2=merge(ret, rettrialnumber2, by.x="onset", by.y="rettrialnumber",all.x=T)
enc2=enc2[order(enc2$roworder, decreasing = F),]
ret2=ret2[order(ret2$roworder, decreasing = F),]
enc2$trialnumber2[c(2:30734)]=na.locf(enc2$trialnumber[c(2:30734)])
ret2$trialnumber2[c(2:34036)]=na.locf(ret2$trialnumber[c(2:34036)])

parcel=read.csv("Z:/Documents/University/Massey University/161.893/Orginal Data/Parcellation/Ordered Brain Regions Post Parcellation with  found regions only and voxel number.csv", header=T)
parcel$zround=round(parcel$average.z/2)*2
parcel$ztimesecond=parcel$zround*2/49
##Combining datasets

ret2$onset_probe=rep(NA, 34036)
ret2$response=rep(NA, 34036)
ret2$trial_type_coded=rep(NA, 34036)
ret2$expected_response_coded=rep(NA, 34036)
ret2$accuracy=rep(NA, 34036)
ret2$accuracy_scrambled=rep(NA, 34036)
ret2$accuracy_task=rep(NA, 34036)
ret2$rec_codes_during_enc=rep(NA, 34036)
ret2$accuracy_scrambled=rep(NA, 34036)
ret2$onset_probe_noTriggerAdjust=rep(NA, 34036)
enc2$response_str2=rep(NA, 30734)
enc2$response_str=rep(NA, 30734)
enc2$pairno_rec=rep(NA, 30734)
enc2$enc_codes_during_rec=rep(NA, 30734)
ret2<-ret2[names(enc2)]
enc2$Condition=rep("Encoding", 30734)
ret2$Condition=rep("Retreival", 34036)
Complete=rbind.data.frame(enc2, ret2)
write.csv(Complete, "Preprocessed Encoding and Retrieval Data.csv")

## Preparation for Bayesian

##setwd("Z:/Documents/University/Masters_Research/fmriprep")

##dat=read.csv("Preprocessed Encoding and Retrieval Data.csv", header=T)
##demo=read.csv("Z:/Documents/University/Massey University/161.893/Orginal Data/Demographics analysis/Demographics.csv")
##for (i in 8:124){
##  dat[,i+171]=cut(dat[,i], breaks=c(-Inf, quantile(dat[,i], probs = .333333), quantile(dat[,i], probs = .666666), Inf), labels=c("Low", "Med", "High"))
##}
##names(dat)[c(179:295)]=paste(names(dat[,8:124]), "Factor", sep="_")

##for (i in 179:295){
##  dat[,i]=as.factor(dat[,i])
##}
##dat2=dat
##dat2$conditiongroup=dat2$Condition
##dat2$conditiongroup[dat2$condition=="Encoding"&dat2$Group=="H"]="H.Enc"
##dat2$conditiongroup[dat2$condition=="Retreival"&dat2$Group=="H"]="H.Ret"
##dat2$conditiongroup[dat2$condition=="Encoding"&dat2$Group=="S"]="S.Enc"
##dat2$conditiongroup[dat2$condition=="Retreival"&dat2$Group=="S"]="S.Ret"
##dat2.1=dat2[dat2$Ghost=="No_ghost",]
##dat2.1$Participant_number=as.numeric(dat2.1$IDNUmb2)
##dat2.2=dat2.1[,-c(149)]
##dat3=dat2.2[!is.na(dat2.2$visualacuity),]
##write.csv(dat3,"BN Preprocessed Encoding and Retrieval Data.csv")

