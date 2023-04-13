rm(list = ls())

library(dplyr)
library(stringr)
library(readr)
setwd("/Users/calvinzhang/Desktop/IAEM project")
data2 <- read.csv("numeric.csv")

new_data <- data2[-c(1:6,8:17)]
new_datafram <- data.frame(new_data)

group1.index <- which(str_detect(new_datafram$Q2, "1|2")) ## 选出选了1或2的人
group2.index <- which(str_detect(new_datafram$Q2, "4|5")) ## 选出选了4或5的人
inter <- intersect(group1.index, group2.index) ## 选出既含有1或2，也含有4或5的人

group1.ind <- group1.index[-which( group1.index %in%  inter)] ## 去掉选了1或2的人中也选了4或5的人
group2.ind <- group2.index[-which( group2.index %in%  inter)] ## 去掉选了4或5的人中也选了1或2的人

which(new_datafram$Q2 =='3') ## 选出只选了3的人

new_datafram$group <- NA
new_datafram$group[c(group1.ind, which(new_datafram$Q2 =='3'))] <- "group1"  
new_datafram$group[group2.ind] <- "group2" 
new_datafram$group[inter] <- "group3"

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

numeric_sum <- apply(new_num[,5:14], 2, as.numeric)
sum <- c()
for (j in 1:nrow(numeric_sum)) {
  sum[j] = sum(numeric_sum[j,],na.rm = T)
}
sum <- new_num$sum

h <- hist(new_num$sum,
          main = "Sum of question 4-13 scores",
          xlab = "Sum of scores",
          ylab = "Number of people",
          xlim = c(0,60),
          ylim = c(0,100),
          col = "darkmagenta", breaks = 12)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))




write.csv(new_num,"new_numeric.csv")
