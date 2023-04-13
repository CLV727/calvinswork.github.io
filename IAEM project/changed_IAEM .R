rm(list = ls())

library(dplyr)
library(stringr)
library(readr)
library(tidyverse)
library(psych)
library(psychTools)
library(ggplot2)
library(survey)

setwd("/Users/calvinzhang/Desktop/IAEM project")
data2 <- read.csv("numeric.csv")

new_data <- data2[-c(1:6,8:17)]
new_datafram <- data.frame(new_data)

# filter people answered 1 or 2 and 4 or 5 
group1.index <- which(str_detect(new_datafram$Q2, "1|2")) 
group2.index <- which(str_detect(new_datafram$Q2, "4|5")) 

# filter people answered 1 or 2, also contain 4 or 5. 
inter <- intersect(group1.index, group2.index) 

# remove people answered 4 or 5 in people answered 1 or 2
group1.ind <- group1.index[-which( group1.index %in%  inter)] 
# remove people answered 1 or 2 in people answered 4 or 5
group2.ind <- group2.index[-which( group2.index %in%  inter)] 
# filter people only answered 3
which(new_datafram$Q2 =='3') 

# create new columns sort by group #s
new_datafram$group <- NA
new_datafram$group[c(group1.ind, which(new_datafram$Q2 =='3'))] <- "group1"  
new_datafram$group[group2.ind] <- "group2" 
new_datafram$group[inter] <- "group1"

# reorder the Q4-13 answers 
x <-c()
for(i in 5:14){
  x <- new_datafram[,i]
  ind1<- which(x=='1')
  ind2<- which(x=='5')
  ind3<- which(x=='2')
  ind4<- which(x=='4')
  x[ind1] <- '5'
  x[ind2] <- '1'
  x[ind3] <- '4'
  x[ind4] <- '2'
  new_datafram[,i] <- x
}
new_num <- na.omit(new_datafram)

# create new column with sum 
numeric_sum <- apply(new_num[,5:14], 2, as.numeric)
sum <- c()
for (j in 1:nrow(numeric_sum)) {
  sum[j] = sum(numeric_sum[j,],na.rm = T)
}
new_num$sum <- sum
final_num <- new_num[-which(new_num$sum==0),]
df <- cbind(ID = 1:nrow(final_num), final_num)

write.csv(df,"changed_final_numeric.csv")

# histogram: 
h <- hist(df$sum,
          main = "Sum of question 4-13 scores",
          xlab = "Sum of scores",
          ylab = "Number of people",
          xlim = c(0,60),
          ylim = c(0,100),
          col = "darkmagenta", breaks = 12)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

data_matrix <- data.matrix(df)
df2 <- df %>%
  select(ID,group,sum)
df3 <- df %>%
  select(ID,6:15)

# alpha value for each questions 
df5 <- lapply(df3, as.numeric)
df5 <- as.data.frame(df5)
cro_alpha <- psych::alpha(drop_na(df5[,2:11]),check.keys = T)
cro_alpha

#reorder the column for group and sum 
num_group <- df2 %>%
  pivot_wider(names_from = group, values_from = sum)

describe(num_group)
describe(df)

# distribution of scores by age
age_group <- df %>%
  dplyr::select(ID,Q14,group,sum)
colnames(age_group)[2] <-"Age"
age_group <- as.data.frame(age_group)
age_group[age_group == ""] <- NA
age_group <- na.omit(age_group)
table(age_group$group)

ggplot(data = age_group,
       aes(x = group, y = Age))+
  geom_boxplot()

age_group %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(sum),
    median = median(sum)
  )

age_group$Age <- as.numeric(age_group$Age)

fit2 <- lm(age_group$group ~ age_group$Age,data = age_group)








# distribution of scores by sex
sex_group <- df %>%
  select(ID,Q15,sum)
sex_group <- as.data.frame(sex_group)
sex_group[sex_group == ""] <- NA
sex_group <- na.omit(sex_group)

table(sex_group$Q15)
ggplot(data = sex_group,
       aes(x = Q15, y = sum))+
  geom_boxplot()
mean_sex_list <- tapply(sex_group$sum, sex_group$Q15, mean,
                         simplify = FALSE)
mean_sex_list













# distribution of scores by race (white vs. non-white)
race_group <- df %>%
  select(ID,Q16,sum)
race_group <- as.data.frame(race_group)
race_group
table(race_group$Q16)
nonwhite_group.index <- which(str_detect(race_group$Q16, "1|2|3|6|7"))
white_group.index <- which(str_detect(race_group$Q16, "5"))


race_group$race <- NA
race_group$race[white_group.index] <- "white"
race_group$race[nonwhite_group.index] <-"non-white"
race_group <- na.omit(race_group)
table(race_group$race)
ggplot(data = race_group,
       aes(x = race, y = sum))+
  geom_boxplot()
mean_race_list <- tapply(race_group$sum, race_group$race, mean,
                         simplify = FALSE)
mean_race_list

# distribution of scores by race (Hispanic vs. non-Hispanic)

hispanic_group <- df %>%
  select(ID,Q17,sum)
hispanic_group <- as.data.frame(hispanic_group)
hispanic_group[hispanic_group == ""] <- NA
hispanic_group <- na.omit(hispanic_group)

table(hispanic_group$Q17)
ggplot(data = hispanic_group,
       aes(x = Q17, y = sum))+
  geom_boxplot()

mean_hispanic_list <- tapply(hispanic_group$sum, hispanic_group$Q17, mean,
                         simplify = FALSE)
mean_hispanic_list




















