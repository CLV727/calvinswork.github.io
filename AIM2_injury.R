rm(list = ls())

library(foreign)
library(dplyr)
library(plyr)
library(ggplot2)
library("readxl")


data <- read_excel("/Users/calvinzhang/Desktop/Smoke-free policy /NYCHA_Property_file_2019.xlsx")

#DU houseing unit, GEO_ID1: cenusus block group 
table <- aggregate(DU ~ GEO_ID1, data, sum)
summary(table)
sum(table$DU)

ACS_HU <- read_excel("/Users/calvinzhang/Desktop/Smoke-free policy /ACS_16_5YR_B25001_clean.xlsx")
summary(ACS_HU)

#Merging together 2016 ACS and NYCHA data #
data_nycha <- merge(ACS_HU,table,by.x="GEOID_1", by.y="GEO_ID1", all.x=TRUE)

# Calculate NYCHA Housing Unit % of BG with Margin of Error #
data_nycha$pct_NYCHA <- data_nycha$DU/data_nycha$Total_HU
data_nycha$pct_min <- data_nycha$DU/(data_nycha$Total_HU + data_nycha$ME_HU)
data_nycha$pct_max <- abs(data_nycha$DU/(data_nycha$Total_HU - data_nycha$ME_HU))


# Create Variable for Treatment Group #
data_nycha$INT <- ifelse(data_nycha$pct_max>=1,1,0)
length(data_nycha$INT[which(data_nycha$INT==1)])
length(data_nycha$INT[which(data_nycha$INT==0)])

#creating a new table for CBG, treatment group indicator, housing units
new<- data_nycha[,c("DU", "GEOID_1","INT")]
# getting counts of aggregate number of housing units per treatment group
sum(new$DU[which(new$INT==1)])
sum(new$DU[which(new$INT==0)])

# write.csv(x=new, file = "CBG list.csv")

# Import ACS 2016 Demographic Data #
# demo <-ACS2016_Cleaned
demo <- read_excel("/Users/calvinzhang/Desktop/Smoke-free policy /ACS2016_Cleaned.xlsx")
demo$GEOID <- as.numeric(substr(demo$Id, 10, 22))

# create age distribution variables - matching the age categorization from SPARCS data #
demo$pctle5<- (demo$Age_le5)/demo$Total*100
demo$pct5_11<-(demo$Age_5t9)/demo$Total*100
demo$pct12_17<- (demo$Age_10t14 + demo$Age_15t17)/demo$Total*100
demo$pct50_64<- (demo$Age_50t54 + demo$Age_55t59 + demo$Age_60t61 + demo$Age_62t64)/demo$Total*100
demo$pct65over <- (demo$Age_65t66 + demo$Age_67t69 + demo$Age_70t74 + demo$Age_75t79 + demo$Age_80t84 + demo$Age_85up) / demo$Total *100

#alternative age categorizations#
demo$pct0_17 <- (demo$Age_le5 + demo$Age_5t9 + demo$Age_10t14 + demo$Age_15t17)/ demo$Total *100
demo$pct18_49 <- (demo$Age_18t19 +  demo$Age_20 + demo$Age_21 + demo$Age_22t24 + demo$Age_25t29 + demo$Age_30t34 +  demo$Age_35t39 + demo$Age_40t44 + demo$Age_45t49) / demo$Total *100
demo$pct50over <- (demo$Age_50t54 +  demo$Age_55t59 + demo$Age_60t61 + demo$Age_62t64 + demo$Age_65t66 + demo$Age_67t69 + demo$Age_70t74 + demo$Age_75t79 + demo$Age_80t84 + demo$Age_85up) / demo$Total *100

# create Race/Ethnicity distribution variables #
demo$pctHis <- demo$His / demo$Total *100
demo$pctAfAm <- demo$AfAm / demo$Total *100
demo$pctWh <- demo$Wh / demo$Total *100
demo$pctOther <- demo$Other_Race / demo$Total *100
demo$pctWhOther <- (demo$Wh + demo$Other_Race) / demo$Total *100

# create male/female ratio #
demo$pctSex <- demo$M_Total / demo$Total *100

# create FPL distribution variables #
demo$pctFPL100 <- demo$FPL_le100 *100
demo$pctFPL125 <- demo$FPL_100t125 *100
demo$pctFPL150 <- demo$FPL_125t150 *100

# Merge Demographic Data #     
data <- merge(data_nycha, demo, by.x = "GEOID_1", by.y = "GEOID", all.x = TRUE)
summary(data)

# Drop Missing (Housing Unit = 0, Population = 0, & FPL cannot determined) #
data <- dplyr::filter(data, Total_HU > 0)
data <- dplyr::filter(data, Total > 0)
length(data$INT[which(data$INT==1)])
table1 <- aggregate(Total ~ INT, data, sum)


#import ACS data files with more housing information
# ACS_year<-ACS2016_5YR_YearBuilt
ACS_year<- read_excel("/Users/calvinzhang/Desktop/Smoke-free policy /ACS2016_5YR_YearBuilt.xlsx")
# ACS_unit<-ACS2016_5YR_HousingUnits
ACS_unit<- read_excel("/Users/calvinzhang/Desktop/Smoke-free policy /ACS2016_5YR_HousingUnits.xlsx")

# merging together ACS data with housing info
ACS_housing<-merge(ACS_year, ACS_unit, by.x = "GEO_ID1", by.y = "GEO_ID1", all.x=TRUE)

# drop missing values
ACS_housing<-dplyr::filter(ACS_housing, total.x>0)
ACS_housing<-dplyr::filter(ACS_housing, GEO_ID1>0)

#merging into ACS 2016 Cleaned Data
data <- merge(data, ACS_housing, by.x = "GEOID_1" ,by.y = "GEO_ID1", all.x = TRUE)

# summary
summary(ACS_housing)

#
data <- merge(data, ACS_housing, by.x = "GEOID_1" ,by.y = "GEO_ID1", all.x = TRUE)

# importing pre-intervention quarterly health outcome rates
# sparcs<-Sparcs_Quarterly_Rates
sparcs <- read_excel("/Users/calvinzhang/Desktop/Smoke-free policy /injury quarterly counts.xlsx")

#merging with ACS dataset
data<- merge (data, sparcs, by.x="GEOID_1", by.y="cbg", all.x =TRUE)
summary (data)

#creating denominator counts specific to age population
data$age_total_1 <- (data$Age_le5 + data$Age_5t9 + data$Age_10t14 + data$Age_15t17)
data$age_total_2 <- (data$Age_50t54 + data$Age_55t59 + data$Age_60t61 +data$Age_62t64 + data$Age_65t66 + data$Age_67t69
                     + data$Age_70t74 + data$Age_75t79 + data$Age_80t84 + data$Age_85up)

#creating percentage variables for pre-intervention quarterly rates
#asthma #####recreate injures %
data$pct_ped_injury_q1 <- (data$q1_ped_injury )/ data$age_total_1 *1000
data$pct_ped_injury_q2 <- (data$q2_ped_injury)/ data$age_total_1 *1000
data$pct_ped_injury_q3 <- (data$q3_ped_injury)/ data$age_total_1 *1000
data$pct_ped_injury_q4 <- (data$q4_ped_injury)/ data$age_total_1 *1000
data$pct_ped_injury_q5 <- (data$q5_ped_injury)/ data$age_total_1 *1000
data$pct_ped_injury_q6 <- (data$q6_ped_injury)/ data$age_total_1 *1000
data$pct_ped_injury_q7 <- (data$q7_ped_injury)/ data$age_total_1 *1000
data$pct_ped_injury_q8 <- (data$q8_ped_injury)/ data$age_total_1 *1000
data$pct_ped_injury_q9 <- (data$q9_ped_injury)/ data$age_total_1 *1000
data$pct_ped_injury_q10 <- (data$q10_ped_injury)/ data$age_total_1 *1000
data$pct_ped_injury_q11 <- (data$q11_ped_injury)/ data$age_total_1 *1000



summary(data)

data$pct_u1950<- data$ pct_u1950.x
data$pct_o1950<-data$pct_o1950.x
data$pct_u10<-data$pct_u10.x
data$pct_10t19<- data$pct_10t19.x
data$pct_20t49<-data$pct_20t49.x
data$pct_o50<-data$pct_o50.x


# READY for Propensity Score Matching - don't forget drop some columns #
data1 <- data[, c("GEOID_1","INT.x","DU", "Total_HU","Total","pctle5","pct5_11","pct12_17","pct50_64", "pct65over",
                  "pctAfAm","pctHis","pctOther","pctFPL100","pctFPL125","pctFPL150",
                  "pct_u1950","pct_o1950", 
                  "pct_u10", "pct_10t19", "pct_20t49", "pct_o50",
                  "pct_ped_injury_q1", "pct_ped_injury_q2", "pct_ped_injury_q3","pct_ped_injury_q4",
                  "pct_ped_injury_q5","pct_ped_injury_q6","pct_ped_injury_q7","pct_ped_injury_q8",
                  "pct_ped_injury_q9","pct_ped_injury_q10","pct_ped_injury_q11") ]
summary(data1)
data1$INT <- ifelse(is.na(data1$INT),999,ifelse(data1$INT==1,1,0))
data1 <- dplyr::filter(data1, data1$INT==1 | data1$INT==999)
data1$INT <- ifelse(data1$INT==999,0,data1$INT)
dplyr::count(data1,INT)

# getting counts of aggregate number of population totals per treatment group
table2 <- aggregate(age_total_1 ~ INT.x, data, sum)

#replace Inf in a vector by NA
data1<- do.call(data.frame,lapply(data1,function(x) replace(x, is.infinite(x),NA)))
#replace NA with 0 in a vector#
data1[is.na(data1)]<- 0



# Propenstiy Score Matching #
#install.packages("MatchIt")
#install.packages("optmatch")
library(MatchIt)
library(optmatch)

# check number of NYCHA blocks (Treatment) #
length(data1$INT[which(data1$INT==1)])


# match Total Pop, FPL, Age, Race and Built Environment on a 1:4 ratio - BASE #
mod.match4_BE <- matchit(INT.x ~ pctle5 + pct5_11 + pct12_17 + pct50_64 +pct65over+ pctAfAm + pctHis + pctFPL100 
                         + pctFPL125 + pct_u1950 +pct_o1950 + pct_u10 + pct_10t19
                         + pct_20t49 + pct_o50 + pct_ped_injury_q1+ pct_ped_injury_q2+ pct_ped_injury_q3+pct_ped_injury_q4+pct_ped_injury_q5+pct_ped_injury_q6+pct_ped_injury_q7+pct_ped_injury_q8+pct_ped_injury_q9+pct_ped_injury_q10+pct_ped_injury_q11,
                         
                         method = "nearest", data = data1, ratio=4, caliper=.2)
summary(mod.match4_BE, standardize = TRUE)
matched4_BE <- match.data(mod.match4_BE)
plot(mod.match4_BE)
hist(matched4_BE$distance) 
# write.csv(x=matched4_BE, file="matched4_method1_BE_caliper.csv")

# Find best matching

num_iteration = 100
sum_cnt_important = rep(0, length = num_iteration)

start_time = Sys.time()
i = 1
while(i <= num_iteration)
{
  set.seed(i)
  mod.match4_BE <- matchit(INT.x ~ pctle5 + pct5_11 + pct12_17 + pct50_64 +pct65over + pctAfAm + pctHis + pctFPL100 
                           + pctFPL125 + pctFPL150 +pct_u1950 +pct_o1950 + pct_u10 + pct_10t19
                           + pct_20t49 + pct_o50 + pct_ped_injury_q1+ pct_ped_injury_q2+ pct_ped_injury_q3+pct_ped_injury_q4+pct_ped_injury_q5+pct_ped_injury_q6+
                             pct_ped_injury_q7+pct_ped_injury_q8+pct_ped_injury_q9+pct_ped_injury_q10+pct_ped_injury_q11,
                           
                           method = "nearest", data = data1, ratio=4, caliper=.2)
  
  
  tmp = summary(mod.match4_BE, standardize = TRUE)
  
  team_seed <- data.frame(tmp$sum.matched)
  
  abs_std_matched<-abs(team_seed$Std..Mean.Diff.)[2:25]
  
  sum_cnt_important[i] = sum(abs_std_matched <= 0.1)
  tmp_str = paste0("seed: ", i, "   Num of cnt <=0.1 : ", sum_cnt_important[i])
  print(tmp_str)
  
  if(sum_cnt_important[i] == 25)
  {
    i = num_iteration
  }
  i = i+1
}
end_time = Sys.time()

end_time - start_time

seed_index = c(1:num_iteration)
seed_index[sum_cnt_important == max(sum_cnt_important)]
hist(sum_cnt_important)


# Best for ITT/Per Protocol : seed 1,48,100
set.seed(100)
mod.match4_BE <-matchit(INT.x ~ pctle5 + pct5_11 + pct12_17 + pct50_64 +pct65over + pctAfAm + pctHis + pctFPL100 
                        + pctFPL125 + pctFPL150 +pct_u1950 +pct_o1950 + pct_u10 + pct_10t19
                        + pct_20t49 + pct_o50 + pct_ped_injury_q1+ pct_ped_injury_q2+ pct_ped_injury_q3+pct_ped_injury_q4+pct_ped_injury_q5+pct_ped_injury_q6+
                          pct_ped_injury_q7+pct_ped_injury_q8+pct_ped_injury_q9+pct_ped_injury_q10+pct_ped_injury_q11,
                        
                        method = "nearest", data = data1, ratio=4, caliper=.2)
summary(mod.match4_BE, standardize = TRUE)
matched4_BE <- match.data(mod.match4_BE)
plot(mod.match4_BE)
hist(matched4_BE$distance) 

setwd("/Users/calvinzhang/Desktop/Smoke-free policy ")

write.csv(x=matched4_BE, file="FINAL_4_CBG_PSM.csv")

write.csv(x=data, file= "CBG_line_listing.csv")
write.csv(x=data1, file = "data.csv")


injury_1 <- read_excel("/Users/calvinzhang/Desktop/Smoke-free policy /injury_p_all.xlsx")
injury_1 <- as.data.frame(injury_1)

gfg_plot <- ggplot(injury_1, aes(x, group = 2)) +  
  geom_line(aes(y = INT), color = "green") +
  geom_line(aes(y = prematch), color = "red")+
  ggtitle("Aults injury_All Data (118/238)-Pre intervation period")
gfg_plot







