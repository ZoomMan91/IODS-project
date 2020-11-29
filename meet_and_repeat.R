### by @ndre! Andrei Kudinov (AK)
## 29.11.2020
# Data wrangling part of Exercise 6.

### 1. Read and write data to IDOS folder.
BPRS<-read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", header=T)
head(BPRS)
RATS<-read.table('https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt',header=T)
head(RATS)
write.table(BPRS, file="data/BPRS.txt", col.names = T, row.names = F,quote = F)
write.table(RATS, file="data/RATS.txt", col.names = T, row.names = F,quote = F)
##Let's check the data sets.
#BPRS
str(BPRS)
dim(BPRS)
table(BPRS$subject)
table(BPRS$treatment)
library(dplyr)
library(tidyr)
#RATS
str(RATS)
dim(RATS)
table(RATS$ID)
table(RATS$Group)
library(dplyr)
library(tidyr)
### 2. convert categorical variables to factors
#BPRS
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
#RATS
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)
### 3. Convert to long form.
#BPRS
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))
head(BPRSL)
glimpse(BPRSL)
#RATS
RATSL <-  RATS %>% gather(key = WDs, value = BW, -ID, -Group)
RATSL <-  RATSL %>% mutate(time = as.integer(ifelse(nchar(WDs)==4,substr(WDs,3,4),substr(WDs,3,3))))
head(RATSL)
glimpse(RATSL)
### 4.Serious look =)
#BPRS
#wide data
head(BPRS)
str(BPRS)
library(ggplot2)
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))
#long data
head(BPRSL)
summary(BPRSL)
#The wide format data has 8 observations (variables) from 20 individuals (subjects). The key idea is that weeks 0 to 8 presented as separate 
# variables. At some points values inside week could be compared within treatments or individuals. Long format data is 5 variables, where weekly
# measurements are repeated records.Thus, variable week is somehow could be used as weighting or fixed effect in the model. And bprs could be 
# related to individuals using for example Incidence matrix.
head(RATSL)
str(RATSL)
summary(RATSL)
#Pretty similar idea was used in Long format of Rats Body Weight data set. This somehow similar to what is used in Animal Evaluations. 
#For example in beef cattle. Consecutive observations (body weight measurements)









