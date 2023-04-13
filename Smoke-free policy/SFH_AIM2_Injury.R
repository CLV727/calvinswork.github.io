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
demographic_data <- merge(data_nycha, demo, by.x = "GEOID_1", by.y = "GEOID", all.x = TRUE)
summary(demographic_data)

# Drop Missing (Housing Unit = 0, Population = 0, & FPL cannot determined) #
demographic_data <- dplyr::filter(demographic_data, Total_HU > 0)
demographic_data <- dplyr::filter(demographic_data, Total > 0)
length(demographic_data$INT[which(demographic_data$INT==1)])
table1 <- aggregate(Total ~ INT, demographic_data, sum)


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
my_data <- merge(demographic_data, ACS_housing, by.x = "GEOID_1" ,by.y = "GEO_ID1", all.x = TRUE)

# summary
summary(ACS_housing)

#
my_data <- merge(demographic_data, ACS_housing, by.x = "GEOID_1" ,by.y = "GEO_ID1", all.x = TRUE)

# importing pre-intervention quarterly health outcome rates
# sparcs<-Sparcs_Quarterly_Rates
sparcs <- read_excel("/Users/calvinzhang/Desktop/Smoke-free policy /sparcs_quarterly_outcomes.x;sx.xlsx")

#merging with ACS dataset
sparcs_data<- merge (my_data, sparcs, by.x="GEOID_1", by.y="cbg", all.x =TRUE)
summary (sparcs_data)

#creating denominator counts specific to age population
sparcs_data$age_total_1 <- (sparcs_data$Age_le5 + sparcs_data$Age_5t9 + sparcs_data$Age_10t14 + sparcs_data$Age_15t17)
sparcs_data$age_total_2 <- (sparcs_data$Age_50t54 + sparcs_data$Age_55t59 + sparcs_data$Age_60t61 +sparcs_data$Age_62t64 + sparcs_data$Age_65t66 + sparcs_data$Age_67t69
                     + sparcs_data$Age_70t74 + sparcs_data$Age_75t79 + sparcs_data$Age_80t84 + sparcs_data$Age_85up)

#creating percentage variables for pre-intervention quarterly rates
#asthma #####recreate injures %
sparcs_data$pct_injury_q1 <- (sparcs_data$q1_injury )/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q2 <- (sparcs_data$q2_injury)/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q3 <- (sparcs_data$q3_injury)/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q4 <- (sparcs_data$q4_injury)/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q5 <- (sparcs_data$q5_injury)/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q6 <- (sparcs_data$q6_injury)/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q7 <- (sparcs_data$q7_injury)/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q8 <- (sparcs_data$q8_injury)/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q9 <- (sparcs_data$q9_injury)/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q10 <- (sparcs_data$q10_injury)/ sparcs_data$age_total_1 *1000
sparcs_data$pct_injury_q11 <- (sparcs_data$q11_injury)/ sparcs_data$age_total_1 *1000



summary(sparcs_data)

sparcs_data$pct_u1950<- sparcs_data$ pct_u1950.x
sparcs_data$pct_o1950<-sparcs_data$pct_o1950.x
sparcs_data$pct_u10<-sparcs_data$pct_u10.x
sparcs_data$pct_10t19<- sparcs_data$pct_10t19.x
sparcs_data$pct_20t49<-sparcs_data$pct_20t49.x
sparcs_data$pct_o50<-sparcs_data$pct_o50.x


# READY for Propensity Score Matching - don't forget drop some columns #
data1 <- sparcs_data[,c("GEOID_1","INT","DU", "Total_HU","Total","pctle5","pct5_11","pct12_17","pct50_64", "pct65over",
                  "pct_afam","pct_his","pct_other","pctFPL100","pctFPL125","pctFPL150",
                  "pct_u1950","pct_o1950", 
                  "pct_u10", "pct_10t19", "pct_20t49", "pct_o50",
                  "pct_injury_q1", "pct_injury_q2", "pct_injury_q3","pct_injury_q4","pct_injury_q5","pct_injury_q6","pct_injury_q7","pct_injury_q8","pct_injury_q11","pct_injury_q9","pct_injury_q10")]
summary(data1)
data1$INT <- ifelse(is.na(data1$INT),999,ifelse(data1$INT==1,1,0))
data1 <- dplyr::filter(data1, data1$INT==1 | data1$INT==999)
data1$INT <- ifelse(data1$INT==999,0,data1$INT)
dplyr::count(data1,INT)

# getting counts of aggregate number of population totals per treatment group
table2 <- aggregate(age_total_1 ~ INT.x, sparcs_data, sum)

#replace Inf in a vector by NA
data1<- do.call(sparcs_data,lapply(data1,function(x) replace(x, is.infinite(x),NA)))
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
mod.match4_BE <- matchit(INT ~ pctle5 + pct5_11 + pct12_17 + pct50_64 +pct65over+ pctAfAm + pctHis + pctFPL100 
                         + pctFPL125 + pct_u1950 +pct_o1950 + pct_u10 + pct_10t19
                         + pct_20t49 + pct_o50 + pct_asthma_q1+ pct_asthma_q2+ pct_asthma_q3+pct_asthma_q4+pct_asthma_q5+pct_asthma_q6+pct_asthma_q7+pct_asthma_q8+pct_asthma_q11+pct_asthma_q9+pct_asthma_q10 ,
                         
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
  mod.match4_BE <- matchit(INT ~ pctle5 + pct5_11 + pct12_17 + pct50_64 +pct65over + pctAfAm + pctHis + pctFPL100 
                           + pctFPL125 + pctFPL150 +pct_u1950 +pct_o1950 + pct_u10 + pct_10t19
                           + pct_20t49 + pct_o50 + pct_asthma_q1+ pct_asthma_q2+ pct_asthma_q3+pct_asthma_q4+pct_asthma_q5+pct_asthma_q6+
                             pct_asthma_q7+pct_asthma_q8+pct_asthma_q11+pct_asthma_q9+pct_asthma_q10,
                           
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
mod.match4_BE <-matchit(INT ~ pctle5 + pct5_11 + pct12_17 + pct50_64 +pct65over + pctAfAm + pctHis + pctFPL100 
                        + pctFPL125 + pctFPL150 +pct_u1950 +pct_o1950 + pct_u10 + pct_10t19
                        + pct_20t49 + pct_o50 + pct_asthma_q1+ pct_asthma_q2+ pct_asthma_q3+pct_asthma_q4+pct_asthma_q5+pct_asthma_q6+pct_asthma_q7+pct_asthma_q8+pct_asthma_q11+pct_asthma_q9+pct_asthma_q10,
                        
                        method = "nearest", data = data1, ratio=4, caliper=.2)
summary(mod.match4_BE, standardize = TRUE)
matched4_BE <- match.data(mod.match4_BE)
plot(mod.match4_BE)
hist(matched4_BE$distance) 
# write.csv(x=matched4_BE, file="FINAL_4_CBG_PSM.csv")
# write.csv(x=data, file= "CBG_line_listing.csv")
# write.csv(x=data1, file = "data.csv")
