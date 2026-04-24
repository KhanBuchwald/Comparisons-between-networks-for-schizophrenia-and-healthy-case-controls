library(MatchIt)
library(missForest)
library(optmatch)
library(naniar)
library(car)
library(randomForest)

## Read Participants included in encoding and retrieval

EncRet=read.table("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Orginal Data/Raw Data/phenotype/participants.tsv", header=T, sep = "\t")

## Remove participants without encoding or retrival data

EncRet=EncRet[EncRet$pamenc!="n/a",]
EncRet=EncRet[EncRet$pamret!="n/a",]

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Orginal Data/Raw Data/phenotype")
Demo0=read.table("demographics.tsv", header=T, sep = "\t")

## Include only those with encoding and retrieval data

Demo=Demo0[Demo0$participant_id%in%EncRet$participant_id,]

## Remove groups other than HC and PDS

Demo$Group=rep(NA, nrow(Demo))
Demo$Group[grepl("sub-1", Demo$participant_id)]="H"
Demo$Group[grepl("sub-5", Demo$participant_id)]="S"

## Select participant for case control

dat0=Demo[!is.na(Demo$Group),]

## Removed Variables: Race_1, Race_2; cigs, Second language, sexuality_opt, cig years, cig_mons, cigs_pack, ethnicity

dat=dat0[,c(1,3:4,6,8:10,15:18,20,22,24:25,27:28)]

## remove n/a

dat[dat=="n/a"]=NA

## Change cigs past to factor

dat$cigs_past2=rep(NA,nrow(dat))
dat$cigs_past2[is.na(dat$cigs_past)]=0
dat$cigs_past2[!is.na(dat$cigs_past)]=1

## Remove orginal cigs_past

dat=dat[,-c(5)]

## Replace "other" values

dat$sexuality[dat$sexuality=="-9998"]=NA

## Dichotmize Race

dat$race_main[dat$race_main==2|dat$race_main==4|dat$race_main==6]=1

## Combine residence "In Residential Treatment Facility" "In home of siblings/non-lineal relatives" "With Partner not married" "In own home w/ spouse and/or children" "Other"

dat$residence[dat$residence=="2"|dat$residence=="3"|dat$residence=="5"|dat$residence=="7"]="8"

## Combined Married and divorced

dat$civil_stat[dat$civil_stat==1|dat$civil_stat==3]="1"

## Change sexuality, hetero/other

dat$sexuality[dat$sexuality=="3"|dat$sexuality=="4"]=2

## Change relion not affliated, affliated

dat$religion[dat$religion=="2"|dat$religion=="3"|dat$religion=="4"|dat$religion=="6"]="1"

## Change NA in cigs-cigs to zero

dat$cigs_cigs[is.na(dat$cigs_cigs)]=0

## Change degree high school or higher

dat$school_degree[dat$school_degree=="4"|dat$school_degree=="5"|dat$school_degree=="6"|dat$school_degree=="7"]="8"
dat$school_degree[dat$school_degree=="2"|dat$school_degree=="3"]="1"

## Marriage numb missing to zero

dat$marriage_num[is.na(dat$marriage_num)]=0
dat$marriage_num[dat$marriage_num=="1"|dat$marriage_num=="2"|dat$marriage_num=="3"]="1"

## Language, english/other

dat$language1[dat$language1=="3"]="2"

## Change variable type of variables

dat$Group=as.factor(dat$Group)
dat$race_main=as.factor(dat$race_main)
dat$gender=as.factor(dat$gender)
dat$residence=as.factor(dat$residence)
dat$cigs_past2=as.factor(dat$cigs_past2)
dat$school_back=as.factor(dat$school_back)
dat$civil_stat=as.factor(dat$civil_stat)
dat$sexuality=as.factor(dat$sexuality)
dat$religion=as.factor(dat$religion)
dat$cigs_cigs=as.numeric(dat$cigs_cigs)
dat$school_yrs=as.numeric(dat$school_yrs)
dat$school_degree=as.factor(dat$school_degree)
dat$marriage_num=as.factor(dat$marriage_num)
dat$age=as.numeric(dat$age)
dat$children_num=as.numeric(dat$children_num)
dat$language1=as.factor(dat$language1)
dat$Group=as.factor(dat$Group)



## Apply random forests Imputation

dat2=cbind(participant_id=dat$participant_id,missForest(dat[c(2:ncol(dat))])$ximp)

## Round continuous variables

for (i in c(9:10,13:14)){
  dat2[i]=round(dat2[,i],0)
}

## Create binary outcome variable

dat2$CaseControl=rep(NA, nrow(dat2))
dat2$CaseControl[dat2$Group=="S"]=1
dat2$CaseControl[dat2$Group=="H"]=0

## Order dataset

dat2=dat2[order(dat2$participant_id),]

## Change rownames

rownames(dat2)=seq(1, 127, 1)

##Remove group

dat3=dat2[,-c(16)]

# Step 1: Estimate propensity scores for each covariate

propensity_age <- glm(CaseControl ~ age, data = dat3, family = binomial)$fitted.values
propensity_gender <- glm(CaseControl ~ gender, data = dat3, family = binomial)$fitted.values
propensity_school_yrs <- glm(CaseControl ~ school_yrs, data = dat3, family = binomial)$fitted.values

# Step 2: Combine propensity scores equally

combined_propensity_score <- (propensity_age + propensity_gender + propensity_school_yrs) / 3
dat3$combined_propensity_score <- combined_propensity_score

## Match algorithm

matchit_model <- matchit(CaseControl ~ combined_propensity_score, data = dat3, method = "nearest", ratio = 1)
summary(matchit_model)

## Create new matched data

matched_data <- match.data(matchit_model)

## Extract information needed

matched_data2=matched_data[,c(1:17)]

## Change case control values and rename it

matched_data2$CaseControl[matched_data2$CaseControl==0]="H"
matched_data2$CaseControl[matched_data2$CaseControl==1]="S"

names(matched_data2)[2]="group"

## Save data

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Confounding analysis and Case Control")

write.csv(matched_data2,"240710_Case_Control_Participants.csv")


