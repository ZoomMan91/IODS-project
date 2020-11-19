### by @ndre! Andrei Kudinov (AK)
## 10.11.2020
# This is code to read and merge data from two qustioneares student-mat and student-por.
#
#
### Reading data
#
#
s_mat<-read.csv(file="student-mat.csv",header=T,sep=";")
#check structure and that reading was correct.
dim(s_mat)#395 observations 33 variables
head(s_mat)
str(s_mat)
# look's OK

s_por<-read.csv(file="student-por.csv",header=T,sep=";")
#check structure and that reading was correct.
dim(s_por)#649 observations 33 variables
head(s_por)
str(s_por)
# look's OK
#
#
### Joining data sets using common columns
#
#
#I will use Reijo Sund R code as example. 
library(dplyr)#attach dplyr
s_mat<-s_mat %>% mutate(id=1000+row_number()) #I am adding id number. Don't understand why to create duplication of data.
s_por<-s_por %>% mutate(id=2000+row_number()) #I am adding id number. Don't understand why to create duplication of data.
#check
head(s_mat)
tail(s_mat)
head(s_por)
tail(s_por)
#looks OK

join_cols<-c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")

new_dataset <- s_mat %>% right_join(s_por, by=join_cols)


free_cols<-setdiff(colnames(s_por),join_cols)

por_math_free<-s_por %>% bind_rows(s_mat)%>%select(one_of(free_cols))

pormath<- s_por %>% bind_rows(s_mat) %>%group_by(.dots=join_cols) %>%
  summarise(                                                           
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)),     #  Rounded mean for numerical
    paid=first(paid),                   #    and first for chars
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))    
     ) %>% 
  filter(n==2, id.m-id.p>650) %>%  
  # Join original free fields, because rounded means or first values may not be relevant
  inner_join(por_math_free,by=c("id.p"="id"),suffix=c("",".p")) %>%
  inner_join(por_math_free,by=c("id.m"="id"),suffix=c("",".m")) %>%
  # Calculate other required variables  
  ungroup %>% mutate(
    alc_use = (Dalc + Walc) / 2,
    high_use = alc_use > 2,
    cid=3000+row_number()
  )


por_id <- s_por %>% mutate(id=1000+row_number()) 
math_id <-s_mat %>% mutate(id=2000+row_number())

# Which columns vary in datasets
free_cols <- c("id","failures","paid","absences","G1","G2","G3")

# The rest of the columns are common identifiers used for joining the datasets
join_cols <- setdiff(colnames(por_id),free_cols)

pormath_free <- por_id %>% bind_rows(math_id) %>% select(one_of(free_cols))

# Combine datasets to one long data
#   NOTE! There are NO 382 but 370 students that belong to both datasets
#         Original joining/merging example is erroneous!
pormath <- por_id %>% 
  bind_rows(math_id) %>%
  # Aggregate data (more joining variables than in the example)  
  group_by(.dots=join_cols) %>%  
  # Calculating required variables from two obs  
  summarise(                                                           
    n=n(),
    id.p=min(id),
    id.m=max(id),
    failures=round(mean(failures)),     #  Rounded mean for numerical
    paid=first(paid),                   #    and first for chars
    absences=round(mean(absences)),
    G1=round(mean(G1)),
    G2=round(mean(G2)),
    G3=round(mean(G3))    
  ) %>%
  # Remove lines that do not have exactly one obs from both datasets
  #   There must be exactly 2 observations found in order to joining be succesful
  #   In addition, 2 obs to be joined must be 1 from por and 1 from math
  #     (id:s differ more than max within one dataset (649 here))
  filter(n==2, id.m-id.p>650) %>%  
  # Join original free fields, because rounded means or first values may not be relevant
  inner_join(pormath_free,by=c("id.p"="id"),suffix=c("",".p")) %>%
  inner_join(pormath_free,by=c("id.m"="id"),suffix=c("",".m")) %>%
  # Calculate other required variables  
  ungroup %>% mutate(
    alc_use = (Dalc + Walc) / 2,
    high_use = alc_use > 2,
    cid=3000+row_number()
  )
str(pormath)
write.table()








