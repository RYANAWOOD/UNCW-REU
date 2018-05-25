rm(list=ls());
fn = "/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 1-May 22, 2018/MorphII_BIF_s7-37_g0.1_max_partial.csv";
data = read.csv(fn,header = F);

library(dplyr)
library(tidyr)

###################
### format data ###
###################

info = data[c('V1')]
step1 = info %>% separate('V1', into = c('id', 'rest'), sep = "_", remove = TRUE)
step2 = step1 %>% separate('rest', into = c('goodstuff', ',jpgshit'), remove = TRUE)

step3 = step2[c('id','goodstuff')]
step3$prevarrests = NA
step3$gender = NA
step3$age = NA

#step3$prevarrests = ifelse(!is.na(as.numeric(step3$goodstuff[3])),substr(step3$goodstuff,1,1,),substr(step3$goodstuff,1,1))

for (ii in 1:length(step3[,1])) {
  if ( nchar(step3$goodstuff[ii]) == 4 ) {
    step3$prevarrests[ii] = substr(step3$goodstuff[ii],1,1)
    step3$gender[ii] = substr(step3$goodstuff[ii],2,2)
    step3$age[ii] = substr(step3$goodstuff[ii],3,4)
  }
  else if ( nchar(step3$goodstuff[ii]) == 5 ) {
    step3$prevarrests[ii] = as.numeric(substr(step3$goodstuff[ii],1,2))
    step3$gender[ii] = substr(step3$goodstuff[ii],3,3)
    step3$age[ii] = as.numeric(substr(step3$goodstuff[ii],4,5))
  }
}
step3$goodstuff = NULL


#################
### plot data ###
#################

### find summaries for previous arrests
summary(as.numeric(step3$prevarrests))
sd(as.numeric(step3$prevarrests))
hist(as.numeric(step3$prevarrests),breaks=6,main='Previous Arrests')


### find summaries for gender
Ms = 0
Fs = 0
for (ii in 1:length(step3$gender)) {
  if (step3$gender[ii] == 'M') {
    Ms = Ms + 1
  }
  else {
    Fs = Fs + 1
  }
}
pie(c(Ms,Fs),c('Males','Females'),main = 'Genders')

### find summaries for age
summary(as.numeric(step3$age))
sd(as.numeric(step3$age))
hist(as.numeric(step3$age),breaks=6,main='Ages')

### find summaries for BIFs
summary(as.numeric(unlist(data[,2:length(data[1,])])))
sd(as.numeric(unlist(data[,2:length(data[1,])])))

# BIF data for male and female
data$V1 = step3$gender
maleBIF = filter(data,V1=="M")
femaleBIF = filter(data,V1=="F")

boxplot(as.numeric(unlist(maleBIF[,2:length(maleBIF[1,])])),as.numeric(unlist(femaleBIF[,2:length(femaleBIF[1,])])),main="BIFs - Male, Female")