
setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Orginal Data/Demographics analysis")
library(lawstat)
part=read.csv("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Confounding analysis and Case Control/240710_Case_Control_Participants.csv", header=T)
demo=read.csv("Demographics.csv", header=T)
comb=read.csv("Demographics Combined.csv", header=T)
comb=comb[-c(3,4,5,6,7,25,8,26,27)]

demo2=merge(demo, comb, by.x="participant_id", by.y = "participant_id", all.x=T)
demo2.1=demo2[demo2$participant_id%in%part$participant_id,]
demo=demo2.1

table(demo$Group)
table(demo$Group, demo$gender)
prop.table(table(demo$Group, demo$gender),1)*100

table(demo$civil_stat, demo$Group)
prop.table(table(demo$civil_stat, demo$Group),2)

#chisq.test(demo$Group, demo$gender)
tapply(demo$age, demo$Group, mean)
tapply(demo$age, demo$Group, median)
tapply(demo$age, demo$Group, sd)
tapply(demo$age, demo$Group, min)
tapply(demo$age, demo$Group, max)
levene.test(demo$age,demo$Group, location = "mean")
#t.test(demo$age~demo$Group, var.equal=TRUE)
table(demo$Group, demo$race_main)
round(prop.table(table(demo$Group, demo$race_main), 1)*100,1)
#demo$racenew=demo$race_main
#demo$racenew[demo$race_main!="5"&demo$race_main!="n/a"]="nc"
#demo$racenew[demo$race_main=="n/a"]=NA
#chisq.test(demo$Group, demo$racenew)
tapply(demo$school_yrs, demo$Group, mean)
tapply(demo$school_yrs, demo$Group, median)
tapply(demo$school_yrs, demo$Group, sd)
tapply(demo$school_yrs, demo$Group, min)
tapply(demo$school_yrs, demo$Group, max)

table(demo$Group, demo$cigs)
prop.table(table(demo$Group, demo$cigs), 1)*100
#chi1=chisq.test(demo$Group, demo$cigs)
#chi1$expected
#levene.test(demo$school_yrs, demo$Group, location = "mean")
#t.test(demo$school_yrs~demo$Group, var.equal=T)
handedness=read.csv("Handedness.csv", header=T)
handedness=handedness[handedness$participant_id%in%demo$participant_id,]
tapply(handedness$leftscore, handedness$X, mean)
tapply(handedness$rightscore, handedness$X, mean)
tapply(handedness$rightscore, handedness$X, median)
tapply(handedness$rightscore, handedness$X, sd)
tapply(handedness$rightscore, handedness$X, min)
tapply(handedness$rightscore, handedness$X, max)
#levene.test(handedness$leftscore, handedness$X, location = "mean")
#levene.test(handedness$rightscore, handedness$X, location = "mean")
#t.test(handedness$leftscore~handedness$X, var.equal=TRUE)
#t.test(handedness$rightscore~handedness$X, var.equal=TRUE)
table(demo$language1, demo$Group)
round(prop.table(table(demo$language1, demo$Group),2)*100,1)
#demo$english=demo$language1
#demo$english[demo$language1=="3"]="2"
#chisq.test(demo$Group, demo$english)


#visual=read.csv("phenotype_visualacuity.csv", header=T)
#visual=visual[visual$participant_id%in%demo$participant_id,]
#visual$eyesight=as.numeric(visual$visualacuity)
#tapply(visual$eyesight, visual$Group, mean, na.rm=T)
#tapply(visual$eyesight, visual$Group, median, na.rm=T)
#tapply(visual$eyesight, visual$Group, sd, na.rm=T)
#tapply(visual$eyesight, visual$Group, min, na.rm=T)
#tapply(visual$eyesight, visual$Group, max, na.rm=T)
#visual2=visual[!is.na(visual$eyesight),]
#levene.test(visual2$eyesight, as.factor(visual2$Group), location = "mean")
#t.test(visual2$eyesight~visual2$Group, var.equal=TRUE)

saps=read.csv("phenotype_saps.csv", header=T)
saps=saps[saps$participant_id%in%demo$participant_id,]
mean(saps$factor_hallucinations)
median(saps$factor_hallucinations)
sd(saps$factor_hallucinations)
min(saps$factor_hallucinations)
max(saps$factor_hallucinations)

mean(saps$factor_delusions)
median(saps$factor_delusions)
sd(saps$factor_delusions)
min(saps$factor_delusions)
max(saps$factor_delusions)

mean(saps$factor_posformalthought)
median(saps$factor_posformalthought)
sd(saps$factor_posformalthought)
min(saps$factor_posformalthought)
max(saps$factor_posformalthought)

sans=read.csv("phenotype_sans.csv", header=T)
sans=sans[sans$participant_id%in%demo$participant_id,]
mean(sans$factor_alogia)
median(sans$factor_alogia)
sd(sans$factor_alogia)
min(sans$factor_alogia)
max(sans$factor_alogia)

mean(sans$factor_anhedonia)
median(sans$factor_anhedonia)
sd(sans$factor_anhedonia)
min(sans$factor_anhedonia)
max(sans$factor_anhedonia)

mean(sans$factor_avolition)
median(sans$factor_avolition)
sd(sans$factor_avolition)
min(sans$factor_avolition)
max(sans$factor_avolition)

mean(sans$factor_attention)
median(sans$factor_attention)
sd(sans$factor_attention)
min(sans$factor_attention)
max(sans$factor_attention)

mean(sans$factor_bluntaffect)
median(sans$factor_bluntaffect)
sd(sans$factor_bluntaffect)
min(sans$factor_bluntaffect)
max(sans$factor_bluntaffect)

bprs=read.csv("phenotype_bprs.csv", header=T)
bprs=bprs[bprs$participant_id%in%demo$participant_id,]
mean(bprs$bprs_positive)
median(bprs$bprs_positive)
sd(bprs$bprs_positive)
min(bprs$bprs_positive)
max(bprs$bprs_positive)


mem=read.csv("phenotype_wms.csv", header=T)
mem=mem[mem$participant_id%in%demo$participant_id,]
tapply(mem$ds_totalraw, mem$Group, mean, na.rm=T)
tapply(mem$ds_totalraw, mem$Group, median, na.rm=T)
tapply(mem$ds_totalraw, mem$Group, sd, na.rm=T)
tapply(mem$ds_totalraw, mem$Group, min, na.rm=T)
tapply(mem$ds_totalraw, mem$Group, max, na.rm=T)
#levene.test(mem$ds_totalraw, mem$Group, location="mean")
#t.test(mem$ds_totalraw~mem$Group, var.equal=FALSE)

tapply(mem$ssp_totalraw, mem$Group, mean, na.rm=T)
tapply(mem$ssp_totalraw, mem$Group, median, na.rm=T)
tapply(mem$ssp_totalraw, mem$Group, sd, na.rm=T)
tapply(mem$ssp_totalraw, mem$Group, min, na.rm=T)
tapply(mem$ssp_totalraw, mem$Group, max, na.rm=T)
levene.test(mem$ssp_totalraw, mem$Group, location="mean")
t.test(mem$ssp_totalraw~mem$Group, var.equal=TRUE)

tapply(mem$vr2r_totalraw, mem$Group, mean, na.rm=T)
tapply(mem$vr2r_totalraw, mem$Group, median, na.rm=T)
tapply(mem$vr2r_totalraw, mem$Group, sd, na.rm=T)
tapply(mem$vr2r_totalraw, mem$Group, min, na.rm=T)
tapply(mem$vr2r_totalraw, mem$Group, max, na.rm=T)
levene.test(mem$vr2r_totalraw, mem$Group, location="mean")
t.test(mem$vr2r_totalraw~mem$Group, var.equal=FALSE)

wais=read.csv("phenotype_wais.csv", header=T)
wais=wais[wais$participant_id%in%demo$participant_id,]
tapply(wais$voc_totalraw, wais$Group, mean, na.rm=T)
tapply(wais$voc_totalraw, wais$Group, median, na.rm=T)
tapply(wais$voc_totalraw, wais$Group, sd, na.rm=T)
tapply(wais$voc_totalraw, wais$Group, min, na.rm=T)
tapply(wais$voc_totalraw, wais$Group, max, na.rm=T)
levene.test(wais$voc_totalraw, wais$Group, location="mean")
t.test(wais$voc_totalraw~wais$Group, var.equal=TRUE)

setwd("E:/161.799/Orginal Data/Encoding and Retrieval preprocessed data")
scores=read.csv("Bn Preprocessed Encoding and Retrieval Data.csv", header=T)
t1=transform(table(scores$Participant_id, scores$Propincorrect))
t2=t1[t1$Freq!=0&t1$Var2!="Firstob",]
scores2=cbind.data.frame(Participant_id=t2$Var1, Prop_incorrect=t2$Var2)
demo3=merge(demo, scores2, by.x="participant_id",by.y="Participant_id",all.x-T)
demo3$Group=as.factor(demo3$Group)
demo3$Prop_incorrect=as.numeric(as.character(demo3$Prop_incorrect))
tapply(1-demo3$Prop_incorrect, demo3$Group, mean)
tapply(1-demo3$Prop_incorrect, demo3$Group, median)
tapply(1-demo3$Prop_incorrect, demo3$Group, sd)
tapply(1-demo3$Prop_incorrect, demo3$Group, min)
tapply(1-demo3$Prop_incorrect, demo3$Group, max)
levene.test(1-demo3$Prop_incorrect,demo3$Group, location = "mean")
t.test(1-demo3$Prop_incorrect~demo3$Group)
