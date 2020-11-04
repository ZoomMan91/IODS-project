### by @ndre! Andrei Kudinov (AK)
## 03.11.2020
# This is code to read data learning2014 from http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt

# set working dir
setwd("~/R/IODS-project/data")

# read the data
learning2014<-read.csv("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t")

# exploring dimension
dim(learning2014)
## data has 183 lines and 60 columns (183 observations from 60 variables)

#exploring structure of the data
str(learning2014)
## all variables except gender are integer (numbers), gender is character. 
## type of the data is data.frame. 
## ! Note that only "gender" variable name starts with lowercase letter

# read dplyr package for data manipulation
library(dplyr)

# create vector of deep learning questions
deep_quest <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30", "D06", "D15", "D23", "D31")
# create subset of data with deep learning questions only
deep_cols <- select(learning2014, one_of(deep_quest))
# create new variable (deep) in the data by averaging deep learning questions
learning2014$deep <- rowMeans(deep_cols)

# create vector of surface questions
surf_quest <- c("SU02", "SU10", "SU18", "SU26", "SU05", "SU13", "SU21", "SU29", "SU08", "SU16", "SU24", "SU32")
# create subset of data with surf learning questions only
surf_cols <- select(learning2014, one_of(surf_quest))
# create new variable (surf) in the data by averaging surf learning questions
learning2014$surf <- rowMeans(surf_cols)

# create vector of strategic learning questions
stra_quest <- c("ST01", "ST09", "ST17", "ST25", "ST04", "ST12", "ST20", "ST28")
# create subset of data with stra learning questions only
stra_cols <- select(learning2014, one_of(stra_quest))
# create new variable (stra) in the data by averaging stra learning questions
learning2014$stra <- rowMeans(stra_cols)

# subset the data using columns requested from task
sel_col<-c('gender','Age','Attitude','deep','stra','surf','Points')
learn2014<-select(learning2014,(one_of(sel_col)))
# exclude observations where exam points are zero.
learn2014 <-filter(learn2014, !Points=="0")
# example from DataCamp has "" around 0, but I don't see any reason for integer variables.
# filter equal to 0 and > then 0 here have same meaning, as no negative values were in the data
# instead of filter subset function could be used, without disturbing dplyr

# my working directory already ../data see line 6

# write table to dir
write.table(learn2014,file="learning2014.txt", col.names = T,row.names = F, quote = F, sep="\t")
# read just written file
read_again<-read.table(file="learning2014.txt", header=T, sep="\t")
# check
str(learn2014)
str(read_again)
head(learn2014)
head(read_again)
cor(learn2014$Points,read_again$Points)
# data looks nice! I am master of data!