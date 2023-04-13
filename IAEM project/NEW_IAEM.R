rm(list = ls())

library(dplyr)
library(stringr)
library(readr)
library(tidyverse)
library(psych)
library(psychTools)
library(janitor)



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
fianl_num <- new_num[-which(new_num$sum==0),]
df <- cbind(ID = 1:nrow(fianl_num), fianl_num)

df2 <- df %>%
  dplyr::select(ID,group,sum)
num_group <- df2 %>%
  pivot_wider(names_from = group, values_from = sum)

table(df2$group)





describe(num_group)

qd <- df %>%
  dplyr::select(Q4_1,group)

qd <- qd[!(is.na(qd$Q4_1) | qd$Q4_1==""), ]

qd <- as.data.frame(qd)
describe(qd)

qd2 <- qd %>%
  pivot_wider(names_from = group, values_from = Q4_1)
describe(qd2)

qd5 <- df %>%
  dplyr::select(ID,Q5_1,group)

qd5 <- qd5[!(is.na(qd5$Q5_1) | qd5$Q5_1==""), ]

qd5 <- as.data.frame(qd5)
qd5 <- qd5 %>%
  pivot_wider(names_from = group, values_from = Q5_1)
describe(qd5)

qd6 <- df %>%
  dplyr::select(ID,Q6_1,group)

qd6 <- qd6[!(is.na(qd6$Q6_1) | qd6$Q6_1==""), ]

qd6 <- as.data.frame(qd6)
qd6 <- qd6 %>%
  pivot_wider(names_from = group, values_from = Q6_1)
describe(qd6)

qd7 <- df %>%
  dplyr::select(ID,Q7_1,group)

qd7 <- qd7[!(is.na(qd7$Q7_1) | qd7$Q7_1==""), ]

qd7 <- as.data.frame(qd7)
qd7 <- qd7 %>%
  pivot_wider(names_from = group, values_from = Q7_1)
describe(qd7)

qd8 <- df %>%
  dplyr::select(ID,Q8_1,group)

qd8 <- qd8[!(is.na(qd8$Q8_1) | qd8$Q8_1==""), ]

qd8 <- as.data.frame(qd8)
qd8 <- qd8 %>%
  pivot_wider(names_from = group, values_from = Q8_1)
describe(qd8)

qd9 <- df %>%
  dplyr::select(ID,Q9_1,group)

qd9 <- qd9[!(is.na(qd9$Q9_1) | qd9$Q9_1==""), ]

qd9 <- as.data.frame(qd9)
qd9 <- qd9 %>%
  pivot_wider(names_from = group, values_from = Q9_1)
describe(qd9)

qd10 <- df %>%
  dplyr::select(ID,Q10_1,group)

qd10 <- qd10[!(is.na(qd10$Q10_1) | qd10$Q10_1==""), ]

qd10 <- as.data.frame(qd10)
qd10 <- qd10 %>%
  pivot_wider(names_from = group, values_from = Q10_1)
describe(qd10)

qd11 <- df %>%
  dplyr::select(ID,Q11_1,group)

qd11 <- qd11[!(is.na(qd11$Q11_1) | qd11$Q11_1==""), ]

qd11 <- as.data.frame(qd11)
qd11 <- qd11 %>%
  pivot_wider(names_from = group, values_from = Q11_1)
describe(qd11)

qd12 <- df %>%
  dplyr::select(ID,Q12_1,group)

qd12 <- qd12[!(is.na(qd12$Q12_1) | qd12$Q12_1==""), ]

qd12 <- as.data.frame(qd12)
qd12 <- qd12 %>%
  pivot_wider(names_from = group, values_from = Q12_1)
describe(qd12)

qd13 <- df %>%
  dplyr::select(ID,Q13_1,group)

qd13 <- qd13[!(is.na(qd13$Q13_1) | qd13$Q13_1==""), ]

qd13 <- as.data.frame(qd13)
qd13 <- qd13 %>%
  pivot_wider(names_from = group, values_from = Q13_1)
describe(qd13)





# histogram: 
h <- hist(df$sum,
     main = "Sum of question 4-13 scores",
     xlab = "Sum of scores",
     ylab = "Number of people",
     xlim = c(0,60),
     ylim = c(0,100),
     col = "darkmagenta", breaks = 12)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

df_sorce <- df %>%
  dplyr::select(ID,6:15)

# alpha value for each questions 
df_sorce <- lapply(df_sorce, as.numeric)
df_sorce <- as.data.frame(df_sorce)
cro_alpha <- psych::alpha(drop_na(df_sorce[,2:11]),check.keys = T)
cro_alpha

#reorder the column for group and sum 
num_group <- df %>%
  pivot_wider(names_from = group, values_from = sum)
describe(num_group)

# compare group1 and group3
num_group.in <- as.data.frame(num_group)
num_group.in[is.na(num_group.in)] <- 0
num_group.in

fit1 <- lm(group3~group1, data = num_group.in)
fit1

### H0: there is no relationship between group1 and 3.
cor.test(num_group.in$group1,num_group.in$group3,
         method = "pearson")

##Pearson's product-moment correlation
### p=0.023, cor = -0.19. we reject null. group1 and 3 are significantly correlated 

### Therefore we combined the group3 into group1. 
df$group <- gsub('group3','group1',df$group)
df
new_group <- df %>%
  pivot_wider(names_from = group, values_from = sum)




# distribution of scores by age
age_group <- df %>%
  dplyr::select(ID,Q14,group,sum)
colnames(age_group)[2] <-"Age"
age_group <- as.data.frame(age_group)
age_group[age_group == ""] <- NA
age_group <- na.omit(age_group)
table(age_group$group)

fit_age <- lm(sum~group+Age, data = age_group)
summary(fit_age)

ggplot(data = age_group,
       aes(x = group, y = sum))+
  geom_boxplot()

age_group %>%
  group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(sum),
    median = median(sum)
  )

# distribution of scores by sex
sex_group <- df %>%
  dplyr::select(ID,Q15,group,sum)

Male.index <- which(str_detect(sex_group$Q15, "1"))
Female.index <- which(str_detect(sex_group$Q15, "2"))
All_other.index <- which(str_detect(sex_group$Q15,"3|4"))
sex_group$sex <- NA
sex_group$sex[Male.index] <- "male"
sex_group$sex[Female.index] <-"female"
sex_group$sex[All_other.index] <- "all_other"
sex_group[sex_group == ""] <- NA
sex_group <- na.omit(sex_group)
sex_group <- as.data.frame(sex_group)


sex_group <- sex_group %>%
  pivot_wider(names_from = group, values_from = sum)




table(sex_group$sex)

fit_sex <- lm(sum ~ group+sex, data = sex_group)
summary(fit_sex)

ggplot(data = sex_group,
       aes(x = sex, y = sum))+
  geom_boxplot()
mean_sex_list <- tapply(sex_group$sum, sex_group$sex, mean,
                        simplify = FALSE)
mean_sex_list

sex_group %>%
  group_by(sex) %>%
  summarise(
    count = n(),
    mean = mean(sum),
    median = median(sum)
  )

# distribution of scores by race (white vs. non-white)
race_group <- df %>%
  dplyr::select(ID,Q16,group,sum)
race_group <- as.data.frame(race_group)
race_group
table(race_group$Q16)
nonwhite_group.index <- which(str_detect(race_group$Q16, "1|2|3|6"))
white_group.index <- which(str_detect(race_group$Q16, "5"))

race_group <- na.omit(race_group)
race_group$race <- NA
race_group$race[white_group.index] <- "white"
race_group$race[nonwhite_group.index] <-"non-white"
race_group <- na.omit(race_group)
table(race_group$race)



fit_race <- lm(sum~ group+race, data = race_group)
summary(fit_race)


ggplot(data = race_group,
       aes(x = race, y = sum))+
  geom_boxplot()
mean_race_list <- tapply(race_group$sum, race_group$race, mean,
                         simplify = FALSE)
mean_race_list

race_group %>%
  group_by(race) %>%
  summarise(
    count = n(),
    mean = mean(sum),
    median = median(sum)
  )





# distribution of scores by race (Hispanic vs. non-Hispanic)
hispanic_group <- df %>%
  dplyr::select(ID,Q17,group, sum)

Hispanic.index <- which(str_detect(hispanic_group$Q17, "1"))
Non_Hispanic.index <- which(str_detect(hispanic_group$Q17, "2"))


hispanic_group$hispanic <- NA
hispanic_group$hispanic[Male.index] <- "hispanic"
hispanic_group$hispanic[Female.index] <-"non_hispanic"
hispanic_group <- as.data.frame(hispanic_group)
hispanic_group[hispanic_group == ""] <- NA
hispanic_group <- na.omit(hispanic_group)
table(hispanic_group$hispanic)

fit_hispanic <- lm(sum~group+hispanic, data = hispanic_group)
summary(fit_hispanic)

ggplot(data = hispanic_group,
       aes(x = hispanic, y = sum))+
  geom_boxplot()

mean_hispanic_list <- tapply(hispanic_group$sum, hispanic_group$hispanic, mean,
                             simplify = FALSE)
mean_hispanic_list

hispanic_group %>%
  group_by(hispanic) %>%
  summarise(
    count = n(),
    mean = mean(sum),
    median = median(sum)
  )



shapiro.test(new_group$group1)
shapiro.test(new_group$(group2)



wilcoxtest <- wilcox.test(new_group$group1, new_group$group2, alternative = "two.sided", mu=0, paired= FALSE)
wilcoxtest

ttest <- t.test(new_group$group1, new_group$group2, alternative = "two.sided", mu = 0, paired= FALSE)
ttest




