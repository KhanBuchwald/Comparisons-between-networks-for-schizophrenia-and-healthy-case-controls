setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Chlorpromazine Equivilent/Data/Chlorpromazine_Equivilent_Dose")

dat=read.csv("medication.csv", header=T)
n=sort(names(dat))
n2=n[c(121,61:80,21:40,1:20,41:60, 81:100,101:120)]
n3=n2[c(1,2,13,15:21, 3:12, 14, 22,33,35:41,23:32,34,42,53,55:61, 43:52, 54, 62, 73, 75:81, 63:72, 74, 82, 93, 95:101, 83:92, 94,102,113,115:121,103:112, 114)]
dat2=dat[,c(n3)]
dat3=dat2[,c(1:16,22:36, 42:56, 62:76, 82:96, 102:116)]
n4=vector()
for (i in 0:14){
  s=seq(2+i, 77+i, 15)
  n4=c(n4, s)
}
n5=c(1,n4)
dat4=dat3[,c(n5)]
dat4[dat4=="n/a"]=NA
dat4.1=dat4
dat4.1=dat4[,-c(2:7)]
dat4.2=dat4.1[,-c(2:7)]
dat4.3=dat4.2[,-c(2:7)]
dat4.4=dat4.3[,-c(2:7)]
dat4.5=dat4.4[,-c(2:7)]
dat4.6=dat4.5[,-c(2:7)]
dat4.7=dat4.6[,-c(2:7)]
dat4.8=dat4.7[,-c(2:7)]
dat4.9=dat4.8[,-c(2:7)]
dat4.10=dat4.9[,-c(2:7)]
dat4.11=dat4.10[,-c(2:7)]
dat4.12=dat4.11[,-c(2:7)]
dat4.13=dat4.12[,-c(2:7)]
dat4.14=dat4.13[,-c(2:7)]
dat4.0=dat4[c(1:7)]
dat4.1=dat4.1[-c(8:ncol(dat4.1))]
dat4.2=dat4.2[-c(8:ncol(dat4.2))]
dat4.3=dat4.3[-c(8:ncol(dat4.3))]
dat4.4=dat4.4[-c(8:ncol(dat4.4))]
dat4.5=dat4.5[-c(8:ncol(dat4.5))]
dat4.6=dat4.6[-c(8:ncol(dat4.6))]
dat4.7=dat4.7[-c(8:ncol(dat4.7))]
dat4.8=dat4.8[-c(8:ncol(dat4.8))]
dat4.9=dat4.9[-c(8:ncol(dat4.9))]
dat4.10=dat4.10[-c(8:ncol(dat4.10))]
dat4.11=dat4.11[-c(8:ncol(dat4.11))]
dat4.12=dat4.12[-c(8:ncol(dat4.12))]
dat4.13=dat4.13[-c(8:ncol(dat4.13))]
names(dat4.0)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.1)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.2)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.3)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.4)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.5)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.6)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.7)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.8)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.9)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.10)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.11)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.12)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.13)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
names(dat4.14)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
dat5=rbind.data.frame(dat4.0,dat4.1,dat4.2,dat4.3,dat4.4,dat4.5,dat4.6,dat4.7,dat4.8,dat4.9,dat4.10,dat4.11,dat4.12,dat4.13,dat4.14)
dat6=dat5[!is.na(dat5$name),]
table(dat6$name)
dat6[dat6$name=="Ambien/ Zolpidem"|dat6$name=="Artane/Trihexyphenidyl"|dat6$name=="Atenolol/ Tenormin"|
       dat6$name=="Ativan/ Lorazepam"|dat6$name=="Diphenhydramine"|dat6$name=="Celexa/ Citalopram"|
       dat6$name=="Clonazepam/ Klonopin"|dat6$name=="Cogentin/ Benztropine"|dat6$name=="Depakote ER/ Divalproex"|
       dat6$name=="Doxepin/ Sinequan/ Adapin"|dat6$name=="ECT"|dat6$name=="Fluoxetine/Prozac"|
       dat6$name=="Hydroxyzine/ Atarax/ Vistaril"|dat6$name=="Klonopin/ Clonazepam"|dat6$name=="Lamictal/ Lamotrigine"|
       dat6$name=="Lamotrigine/ Lamictal"| dat6$name=="Levothyroxine sodium/ Levotabs/ Levothroid/ etc"|
       dat6$name=="Lexapro/ Escitalopram oxalate"|dat6$name=="Lithium Carbonate/Eskalith/Lithonate/ etc"|
       dat6$name=="Lorazepam/ Ativan"|dat6$name=="Navane/ Thiothixene"|dat6$name=="Paroxetine/ Paxil"|
       dat6$name=="Paxil/ Paroxetine"|dat6$name=="Phentermine Hydrochloride"|dat6$name=="Propranolol/ Inderal"|
       dat6$name=="Prozac/ Fluoxetine"|dat6$name=="Remeron/ Mirtazapine"|dat6$name=="Restoril/ Temazepam"|
       dat6$name=="Rozerem"|dat6$name=="Saphris"|dat6$name=="Sleep Eze/ Diphenhydramine"|
       dat6$name=="Synthroid/ Levothyroxine sodium"|dat6$name=="Tegretol/ Carbamazepine"|dat6$name=="Temazepam/ Restoril"|
       dat6$name=="Topamax/ Topiramate"|dat6$name=="Trazodone/ Desyrel"|dat6$name=="Wellbutrin SR /Bupropion"|
       dat6$name=="Wellbutrin/Bupropion"|dat6$name=="Benadryl/ Diphenhydramine"|dat6$name=="Depakote/ Divalproex"|
       dat6$name=="Divalproex/ Depakote ER",]=NA
dat7=dat6[!is.na(dat6$name),]
dat7=dat7[!is.na(dat7$use),]
dat7.1=dat4[!dat4$participant_id%in%dat7$participant_id,][,c(1:7)]
names(dat7.1)=c("participant_id", "name", "dose", "days", "Duration", "prn", "use")
dat7.1[,c(2:7)]=NA
dat8=rbind.data.frame(dat7, dat7.1)
dat8$ChrolpromazineEquivlient=rep(NA, nrow(dat8))
dat8$dose[dat8$dose<0]=NA
dat8$dose=as.numeric(dat8$dose)
dat8$ChrolpromazineEquivlient[dat8$name=="Abilify/ Aripiprazole"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Abilify/ Aripiprazole"&!is.na(dat8$name)]/5*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Clozapine/ Clozaril"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Clozapine/ Clozaril"&!is.na(dat8$name)]/100*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Clozaril/ Clozapine"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Clozaril/ Clozapine"&!is.na(dat8$name)]/100*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Fanapt/Iloperidone"&!is.na(dat8$name)]=NA
dat8$ChrolpromazineEquivlient[dat8$name=="Geodon/ Ziprasidone"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Geodon/ Ziprasidone"&!is.na(dat8$name)]/26.67*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Haloperidol/ Haldol"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Haloperidol/ Haldol"&!is.na(dat8$name)]/2.67*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Invega / Paliperidone"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Invega / Paliperidone"&!is.na(dat8$name)]/2*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Loxitane/ Loxapine"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Loxitane/ Loxapine"&!is.na(dat8$name)]/33.33*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Prolixin/ Fluphenazine"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Prolixin/ Fluphenazine"&!is.na(dat8$name)]/3.33*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Risperdal/ Risperidone"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Risperdal/ Risperidone"&!is.na(dat8$name)]/1.67*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Risperidone/ Risperdal Consta"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Risperidone/ Risperdal Consta"&!is.na(dat8$name)]/1.67*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Risperidone/Risperdal"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Risperidone/Risperdal"&!is.na(dat8$name)]/1.67*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Seroquel/ Quetiapine"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Seroquel/ Quetiapine"&!is.na(dat8$name)]/133.33*100)
dat8$ChrolpromazineEquivlient[dat8$name=="Zyprexa/ Olanzapine"&!is.na(dat8$name)]=(dat8$dose[dat8$name=="Zyprexa/ Olanzapine"&!is.na(dat8$name)]/3.33*100)
dat8$ChrolpromazineEquivlient=round(dat8$ChrolpromazineEquivlient)
dat8$ChrolpromazineEquivlient[is.na(dat8$name)]=0
dat8.1=dat8[duplicated(dat8$participant_id),]
dat8.2=dat8[!duplicated(dat8$participant_id),]
dat8.3=dat8.1[duplicated(dat8.1$participant_id),]
dat8.4=dat8.1[!duplicated(dat8.1$participant_id),]
dat8.5=dat8.4[!is.na(dat8.4$ChrolpromazineEquivlient),]
dat8.6=rbind.data.frame(dat8.3, dat8.5)
dat10=merge(dat8.2,dat8.6,  all.x = T, by="participant_id")
dat10$ComDoseEquiv2=dat10$ChrolpromazineEquivlient.x
dat10$ComDoseEquiv2[!is.na(dat10$name.y)]=dat10$ChrolpromazineEquivlient.x[!is.na(dat10$name.y)]+dat10$ChrolpromazineEquivlient.y[!is.na(dat10$name.y)]
MeanVal=mean(dat10$ComDoseEquiv2, na.rm=T)
SDVal=sd(dat10$ComDoseEquiv2, na.rm=T)
TwoSD=MeanVal+2*SDVal
dat10$ComDoseEquiv3=dat10$ComDoseEquiv2
dat10$ComDoseEquiv3[dat10$ComDoseEquiv3>TwoSD]=NA
setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Chlorpromazine Equivilent/Data/Chlorpromazine_Equivilent_Dose")
write.csv(dat10, "221101_Medication_For_Analysis.csv")
