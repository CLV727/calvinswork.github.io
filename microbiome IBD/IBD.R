rm(list = ls())

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(arsenal)
library(xlsx)

setwd("/Users/calvinzhang/Desktop/Microbiome/IBD")
ibd <- read_excel("/Users/calvinzhang/Desktop/Microbiome/IBD/clinical-data/risk/RISK_Summary_2022.xlsx")
mydata <- read.csv("/Users/calvinzhang/Desktop/Thesis2/meta_16S.csv")
unique(ibd[c("DIAGNOSIS")])

id <- unique(ibd$DEIDENTIFIED_PATIENT_ID) #1813 -> sample size

ibd1 <- ibd[grep("Enrollment Visit", ibd$TYPE_OF_ENCOUNTER),]
ibd1 <- ibd1[, -15]
ibd1 <- ibd1[,-which(names(ibd1) == "")]
ibd1$
CD <- grep("Crohn's Disease",ibd1$DIAGNOSIS) #1134
UC <- grep("Ulcerative Colitis",ibd1$DIAGNOSIS) #142
Unclassified <- grep("IBD Unclassified",ibd1$DIAGNOSIS) #179
not_ibd <- grep("Not IBD",ibd1$DIAGNOSIS) # 357

case <- c(CD,UC,Unclassified) #1455
control <- not_ibd #357

case <- mydata[grep(c("Crohn's Disease","Ulcerative Colitis","IBD Unclassified"), mydata$DIAGNOSIS),]
#628
control <- mydata[grep("Not IBD", ibd$DIAGNOSIS),]#297

mydata$group <- ifelse(mydata$DIAGNOSIS == "Not IBD","control","case")
case <- mydata[grep(paste(c("Crohn's Disease", "Ulcerative Colitis", "IBD Unclassified"), collapse = "|"), mydata$DIAGNOSIS),]
control <- mydata[grep("Not IBD", mydata$DIAGNOSIS),]


total_female <- grep("Female", mydata$GENDER) #398
total_male <- grep("Male", mydata$GENDER) #527
case.female <-  grep("Female",case$GENDER) #257
case.male <- grep("Male",case$GENDER) #371
control.female <- grep("Female",control$GENDER) #141
control.male <- grep("Male",control$GENDER) #156


case.G.No. <- intersect(control, grep("No",ibd1$`DISEASE BEHAVIOR - STRICTURING/FIBROSTENOTIC`)) #310
case.G.Yes <- intersect(control, grep("Yes",ibd1$`DISEASE BEHAVIOR - STRICTURING/FIBROSTENOTIC`)) #1
control.G.No <- intersect(control, grep("No",ibd1$`DISEASE BEHAVIOR - STRICTURING/FIBROSTENOTIC`)) #310
control.G.Yes <- intersect(control, grep("Yes",ibd1$`DISEASE BEHAVIOR - STRICTURING/FIBROSTENOTIC`)) #1

case.H.No. <- intersect(control, grep("No",ibd1$`DISEASE BEHAVIOR - INTERNALLY PENTRATING`)) #312
case.H.Yes <- intersect(control, grep("Yes",ibd1$`DISEASE BEHAVIOR - INTERNALLY PENTRATING`)) #0
control.H.No <- intersect(control, grep("No",ibd1$`DISEASE BEHAVIOR - INTERNALLY PENTRATING`)) #312
control.H.Yes <- intersect(control, grep("Yes",ibd1$`DISEASE BEHAVIOR - INTERNALLY PENTRATING`)) #0

case.I.No. <- intersect(control, grep("No",ibd1$`IBD - FAMILY HISTORY`)) #263
case.I.Yes <- intersect(control, grep("Yes",ibd1$`IBD - FAMILY HISTORY`)) #94
control.I.No <- intersect(control, grep("No",ibd1$`IBD - FAMILY HISTORY`)) #263
control.I.Yes <- intersect(control, grep("Yes",ibd1$`IBD - FAMILY HISTORY`)) #94

id.age <- mydata[,1:7] 
head(id.age)
age_table <- table(id.age$AGE_AT_ENCOUNTER)
age_table
plot(age_table)


id.age$AGE_AT_ENCOUNTER <- as.numeric(id.age$AGE_AT_ENCOUNTER)
mean(id.age$AGE_AT_ENCOUNTER)
median(id.age$AGE_AT_ENCOUNTER)
sd(id.age$AGE_AT_ENCOUNTER)

case_age <- table(case$AGE_AT_ENCOUNTER) 
mean(as.numeric(case$AGE_AT_ENCOUNTER)) #12.21656
median(as.numeric(case$AGE_AT_ENCOUNTER)) #12
sd(as.numeric(case$AGE_AT_ENCOUNTER)) #2.973715
case_ci <- t.test(as.numeric(case$AGE_AT_ENCOUNTER))$conf.int #(11.98353 12.44959)

mean(as.numeric(case$`IGA ASCA`), na.rm = T) #16.42308
mean(as.numeric(case$IGG.ASCA), na.rm = T) #29.94822
mean(as.numeric(case$OMPC), na.rm = T) #11.91724
mean(as.numeric(case$ANCA), na.rm = T) #19.92831


mean(as.numeric(case$LYMPHOCYTES), na.rm = T) #36.24424
mean(as.numeric(case$CREATININE), na.rm = T) #0.5751095
mean(as.numeric(case$ALANINE.AMINOTRANSFERASE..ALT.), na.rm = T) #16.46168
mean(as.numeric(case$ALBUMIN), na.rm = T) #3.452083
mean(as.numeric(case$ALKALINE.PHOSPHATASE..ALP.), na.rm = T) #128.7464
mean(as.numeric(case$ASPARTATE.AMINOTRANSFERASE..AST.), na.rm = T) #24.4275
mean(as.numeric(case$C.REACTIVE.PROTEIN), na.rm = T) #16.86559
mean(as.numeric(case$EOSINOPHIL), na.rm = T) #6.456048
mean(as.numeric(case$GAMMA.GLUTAMYL.TRANSFERASE..GGT.), na.rm = T) #20.52406
mean(as.numeric(case$HEMATOCRIT), na.rm = T) #34.43951
mean(as.numeric(case$NEUTROPHIL), na.rm = T) #88.88563
mean(as.numeric(case$PLATELET.COUNT), na.rm = T) #477.9553
mean(as.numeric(case$UREA), na.rm = T) #8.253773
mean(as.numeric(case$WHITE.BLOOD.CELL.COUNT), na.rm = T) #9.455543
mean(as.numeric(case$GM.CSF), na.rm = T) #4.804376
mean(as.numeric(case$IGA.ASCA), na.rm = T) #16.42
mean(as.numeric(case$IGG.ASCA), na.rm = T) #29.94822
mean(as.numeric(case$I2), na.rm = T) #6.287591
mean(as.numeric(case$OMPC), na.rm = T) #11.91724
mean(as.numeric(case$CBIR.FLA), na.rm = T) #45.6126
mean(as.numeric(case$ANCA), na.rm = T) #19.92831
mean(as.numeric(case$PCDAI...MOST.SEVERE.ABDOMINAL.PAIN.PAST.7.DAYS), na.rm = T)#12.10356
mean(as.numeric(case$PCDAI...STOOLS.PER.DAY.PAST.7.DAYS), na.rm = T) #6.140777
mean(as.numeric(case$PCDAI.PT.FUNCTING.GENERAL.WELLBEING.PAST.7.DAY), na.rm = T) #8.899676
mean(as.numeric(case$ERYTHROCYTE.SEDIMENTATION.RATE..ESR.), na.rm = T)#37.90283
mean(as.numeric(case$PCDAI...WEIGHT), na.rm = T) #4.498382



control_age <- table(control$AGE_AT_ENCOUNTER) 
mean(as.numeric(control$AGE_AT_ENCOUNTER)) #12.15488
median(as.numeric(control$AGE_AT_ENCOUNTER))#13
sd(as.numeric(control$AGE_AT_ENCOUNTER)) #3.633959
control_ci <- t.test(as.numeric(control$AGE_AT_ENCOUNTER))$conf.int #(11.73990 12.56986)
mean(as.numeric(control$IGA.ASCA), na.rm = T) #2.661507
mean(as.numeric(control$IGG.ASCA), na.rm = T) #7.285827
mean(as.numeric(control$OMPC), na.rm = T) #6.947639
mean(as.numeric(control$ANCA), na.rm = T) #15.63706

mean(as.numeric(control$LYMPHOCYTES), na.rm = T)#98.81242
mean(as.numeric(control$CREATININE), na.rm = T)#1.650704
mean(as.numeric(control$ALANINE.AMINOTRANSFERASE..ALT.), na.rm = T)#22.2037
mean(as.numeric(control$ALBUMIN), na.rm = T)#4.269461
mean(as.numeric(control$ALKALINE.PHOSPHATASE..ALP.), na.rm = T)#171.6415
mean(as.numeric(control$ASPARTATE.AMINOTRANSFERASE..AST.), na.rm = T)#31.86747
mean(as.numeric(control$C.REACTIVE.PROTEIN), na.rm = T)#3.259214
mean(as.numeric(control$EOSINOPHIL), na.rm = T). #0.8826294
mean(as.numeric(control$ERYTHROCYTE.SEDIMENTATION.RATE..ESR.), na.rm = T)#15.37288
mean(as.numeric(control$GAMMA.GLUTAMYL.TRANSFERASE..GGT.), na.rm = T) #19.61972
mean(as.numeric(control$HEMATOCRIT), na.rm = T) #37.63564
mean(as.numeric(control$NEUTROPHIL), na.rm = T) #162.1875
mean(as.numeric(control$PLATELET.COUNT), na.rm = T) #300.1809
mean(as.numeric(control$UREA), na.rm = T) #11.11429
mean(as.numeric(control$WHITE.BLOOD.CELL.COUNT), na.rm = T) #7.49105
mean(as.numeric(control$GM.CSF), na.rm = T) #1.297249
mean(as.numeric(control$IGA.ASCA), na.rm = T) #2.661507
mean(as.numeric(control$IGG.ASCA), na.rm = T) #7.285827
mean(as.numeric(control$I2), na.rm = T) #4.136071
mean(as.numeric(control$OMPC), na.rm = T) #6.947639
mean(as.numeric(control$CBIR.FLA), na.rm = T) #17.46787
mean(as.numeric(control$ANCA), na.rm = T) #15.63706
mean(as.numeric(control$PROBIOTIC.SUPPLEMENT), na.rm = T)
mean(as.numeric(control$PCDAI...MOST.SEVERE.ABDOMINAL.PAIN.PAST.7.DAYS), na.rm = T)#12.75676
mean(as.numeric(control$PCDAI...STOOLS.PER.DAY.PAST.7.DAYS), na.rm = T) #5.432432
mean(as.numeric(control$PCDAI.PT.FUNCTING.GENERAL.WELLBEING.PAST.7.DAY), na.rm = T) #7.135135
mean(as.numeric(control$PCDAI...WEIGHT), na.rm = T) #2.087912



table.xl <- tableby(DIAGNOSIS~AGE_AT_ENCOUNTER+GENDER+
                      `DISEASE BEHAVIOR - STRICTURING/FIBROSTENOTIC`+
                      `DISEASE BEHAVIOR - INTERNALLY PENTRATING`+
                      `IBD - FAMILY HISTORY`+HEMOGLOBIN+LYMPHOCYTES+CREATININE+
                      `ALANINE AMINOTRANSFERASE (ALT)`+ALBUMIN+`ALKALINE PHOSPHATASE (ALP)`+
                      `ASPARTATE AMINOTRANSFERASE (AST)`+`C REACTIVE PROTEIN`+
                      EOSINOPHIL+`ERYTHROCYTE SEDIMENTATION RATE (ESR)`+`GAMMA-GLUTAMYL TRANSFERASE (GGT)`+
                      HEMATOCRIT+NEUTROPHIL+`PLATELET COUNT`+UREA+`WHITE BLOOD CELL COUNT`, data = ibd1)
new_table <- summary(table.xl, title = "Descriptive Data")
new_table <- as.data.frame(new_table)
write.csv(x=new_table, file = "new_table.csv")
#####
table.all <- tableby(DIAGNOSIS ~., data = ibd)
all_table <- summary(table.all, title = "Descriptive Data")
all_table <- as.data.frame(all_table)
write.csv(x=all_table, file = "all_table")

temp <- ibd[-which(is.na(ibd$DIAGNOSIS_DATE)),]
temp$diagdate<-as.integer(substring(temp$DIAGNOSIS_DATE, 8,11) )- as.integer(temp$BIRTH_YEAR) 
age.diff <- as.integer(temp$AGE_AT_ENCOUNTER)-temp$diagdate
table(age.diff)

barplot(as.integer(temp$AGE_AT_ENCOUNTER)-temp$diagdate, 
        main='encounter age - diagnosis age ')


table1 <- tableby(DIAGNOSIS~ AGE_AT_ENCOUNTER+GENDER+
                    `DISEASE BEHAVIOR - STRICTURING/FIBROSTENOTIC`+
                    `DISEASE BEHAVIOR - INTERNALLY PENTRATING`+
                    `IBD - FAMILY HISTORY`, data = ibd1)
my.table1 <- summary(table1, title = "Descriptive Data")
my.table1 <- as.data.frame(my.table1)

cd.index <- grep("Crohn's Disease",ibd$DIAGNOSIS)
cd.HEMOGLOBI <- intersect(cd.index,ibd$HEMOGLOBIN)

ibd2 <- read_excel("/Users/calvinzhang/Desktop/Microbiome/IBD/clinical-data/risk/RISK.xlsx")

type.encounter <- unique(ibd2$TYPE_OF_ENCOUNTER)
name <- names(ibd2) 
table.plot <- data.frame(matrix(ncol=10,nrow=1)) 
colnames(table.plot) <- name[4:13] 

library(stringr)
for (i in 1:length(type.encounter)) {
  temp <- ibd2 %>% filter( TYPE_OF_ENCOUNTER == type.encounter[i])
  
  numb1 <- na.omit(str_count(temp[,4],".idat"))  
  numb2 <- na.omit(str_count(temp[,5],".gz"))
  numb3 <- na.omit(str_count(temp[,6],".gz"))
  numb4 <- na.omit(str_count(temp[,7],".gz"))
  numb5 <- na.omit(str_count(temp[,8],".gz"))
  numb6 <- na.omit(str_count(temp[,9],".gz"))
  numb7 <- na.omit(str_count(temp[,10],".BAM"))
  numb8 <- na.omit(str_count(temp[,11],".idat"))
  numb9 <- na.omit(str_count(temp[,12],".idat"))
  numb10 <- nrow(na.omit(temp[,13]))
  temp1 <- data.frame(numb1, numb2, numb3, numb4, numb5, numb6, numb7, numb8, numb9, numb10) 
  colnames(temp1) <- name[4:13]
  table.plot <- rbind(table.plot,temp1)
}

table.plot <- table.plot[-1,]
rownames(table.plot) <- type.encounter
table.plot <- as.data.frame(table.plot)

WGS.index  <- na.omit(grep(".gz",ibd2$`WHOLE SHOTGUN SEQUENCING (WGS)`))
sixs.index <- na.omit(grep(".BAM",ibd2$`16S`))
mts <- intersect(WGS.index,sixs.index)
table(mts)

sixs <- na.omit(str_count(ibd2$`16S`,".BAM"))
t1 <- table(sixs)

library(RColorBrewer)

idb.temp <- ibd2[,c(1,2,10)]
idb.temp <- na.omit(idb.temp)
idb.temp$number <-str_count( idb.temp$`16S`,';')+1
barplot(table(idb.temp$number),col=brewer.pal(5,'Set1'))

