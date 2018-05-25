rm(list=ls())
DData = read.csv("/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 2-May 23, 2018/morphII_dirty.csv",header=TRUE);
CData = read.csv("/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 2-May 23, 2018/morphII_clean.csv",header=TRUE);

###################################################################
### Step 1 - Find number of males, number of females, and total ###
###################################################################

usedData = CData

### list all distinct id numbers
IDs = union(usedData$id_num,usedData$id_num)

### for each ID, find the corresponding number of male and female hits, and youngest age of incarceration
MaleHits = vector(mode = "logical", length = length(IDs))
MaleHitsAge = rep(1000, length(IDs))
FemaleHits = vector(mode = "logical", length = length(IDs))
FemaleHitsAge = rep(1000, length(IDs))
for (ii in 1:length(IDs)) {
  visits = subset(usedData, id_num == IDs[ii], select=c(gender,age))
  Ms = 0
  Fs = 0
  for (jj in 1:length(visits[,1])) {
    if (visits$gender[jj] == "M") {
      Ms = Ms + 1
      MaleHitsAge[ii] = min(MaleHitsAge[ii],visits$age[jj])
    }
    if (visits$gender[jj] == "F") {
      Fs = Fs + 1
      FemaleHitsAge[ii] = min(FemaleHitsAge[ii],visits$age[jj])
    }
  }
  MaleHits[ii]=Ms;
  FemaleHits[ii]=Fs;
}

### count the number of nonzero male and female hits for each ID
Ms = sum(MaleHits != 0)
Fs = sum(FemaleHits != 0)

### for each ID, find corresponding number of each demographic
MaleHitsB = vector(mode = "logical", length = length(IDs))
FemaleHitsB = vector(mode = "logical", length = length(IDs))
MaleHitsW = vector(mode = "logical", length = length(IDs))
FemaleHitsW = vector(mode = "logical", length = length(IDs))
MaleHitsA = vector(mode = "logical", length = length(IDs))
FemaleHitsA = vector(mode = "logical", length = length(IDs))
MaleHitsH = vector(mode = "logical", length = length(IDs))
FemaleHitsH = vector(mode = "logical", length = length(IDs))
MaleHitsO = vector(mode = "logical", length = length(IDs))
FemaleHitsO = vector(mode = "logical", length = length(IDs))
for (ii in 1:length(IDs)) {
  visits = subset(usedData, id_num == IDs[ii], select=c(gender,race))
  MBs = 0
  FBs = 0
  MWs = 0
  FWs = 0
  MAs = 0
  FAs = 0
  MHs = 0
  FHs = 0
  MOs = 0
  FOs = 0
  for (jj in 1:length(visits[,1])) {
    if (visits$gender[jj] == "M" ) {
      if (visits$race[jj] == "B") {
        MBs = MBs + 1
      }
      if (visits$race[jj] == "W") {
        MWs = MWs + 1
      }
      if (visits$race[jj] == "A") {
        MAs = MAs + 1
      }
      if (visits$race[jj] == "H") {
        MHs = MHs + 1
      }
      if (visits$race[jj] == "O") {
        MOs = MOs + 1
      }
    }
    if (visits$gender[jj] == "F") {
      if (visits$race[jj] == "B") {
        FBs = FBs + 1
      }
      if (visits$race[jj] == "W") {
        FWs = FWs + 1
      }
      if (visits$race[jj] == "A") {
        FAs = FAs + 1
      }
      if (visits$race[jj] == "H") {
        FHs = FHs + 1
      }
      if (visits$race[jj] == "O") {
        FOs = FOs + 1
      }
    }
  }
  MaleHitsB[ii]=MBs;
  MaleHitsW[ii]=MWs;
  MaleHitsA[ii]=MAs;
  MaleHitsH[ii]=MHs;
  MaleHitsO[ii]=MOs;
  FemaleHitsB[ii]=FBs;
  FemaleHitsW[ii]=FWs;
  FemaleHitsA[ii]=FAs;
  FemaleHitsH[ii]=FHs;
  FemaleHitsO[ii]=FOs;
}

### count the number of nonzero hits of each demographic for each ID
MBs = sum(MaleHitsB != 0)
FBs = sum(FemaleHitsB != 0)
MWs = sum(MaleHitsW != 0)
FWs = sum(FemaleHitsW != 0)
MAs = sum(MaleHitsA != 0)
FAs = sum(FemaleHitsA != 0)
MHs = sum(MaleHitsH != 0)
FHs = sum(FemaleHitsH != 0)
MOs = sum(MaleHitsO != 0)
FOs = sum(FemaleHitsO != 0)

### find the number of males and females with 1,2,3,4,5+ extra visits
M1 = sum(MaleHits == 2)
M2 = sum(MaleHits == 3)
M3 = sum(MaleHits == 4)
M4 = sum(MaleHits == 5)
M5 = sum(MaleHits > 5)

F1 = sum(FemaleHits == 2)
F2 = sum(FemaleHits == 3)
F3 = sum(FemaleHits == 4)
F4 = sum(FemaleHits == 5)
F5 = sum(FemaleHits > 5)

### find number of males and females with first-visit ages in the ranges
AgeM1 = sum(MaleHitsAge < 20)
AgeM2 = sum(MaleHitsAge >= 20 & MaleHitsAge < 30)
AgeM3 = sum(MaleHitsAge >= 30 & MaleHitsAge < 40)
AgeM4 = sum(MaleHitsAge >= 40 & MaleHitsAge < 50)
AgeM5 = sum(MaleHitsAge >= 50 & MaleHitsAge < 999)

AgeF1 = sum(FemaleHitsAge < 20)
AgeF2 = sum(FemaleHitsAge >= 20 & FemaleHitsAge < 30)
AgeF3 = sum(FemaleHitsAge >= 30 & FemaleHitsAge < 40)
AgeF4 = sum(FemaleHitsAge >= 40 & FemaleHitsAge < 50)
AgeF5 = sum(FemaleHitsAge >= 50 & FemaleHitsAge < 999)

#################################################################################
### Step 4 - Merge with BIF data for 1000 visitors, summarize the BIF by race ###
#################################################################################
rm(list=ls())
BIFData = read.csv("/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 1-May 22, 2018/MorphII_BIF_s7-37_g0.1_max_partial.csv",header = F);
CData = read.csv("/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 2-May 23, 2018/morphII_clean.csv",header=TRUE);
indexes = rep(0,length(BIFData[,1]))

### make a vector with the indexes of each of the rows in BIFData
for (ii in 1:length(indexes)) {
  tmpInfo = as.character(BIFData[ii,1])
  tmpInfo = substr(tmpInfo,1,6)
  indexes[ii] = tmpInfo
}

### replace the first column of BIFData with the race of the corresponding
BIFData$V1 = as.character(BIFData$V1)
for (ii in 1:length(indexes)) {
  curPerson = subset(CData,id_num == as.numeric(indexes[ii]))
  race = as.character(curPerson$race[1])
  BIFData$V1[ii] = race
}

### run summary statistics on the male and female subsets of BIFData
BBIF = filter(BIFData,V1=="B")
WBIF = filter(BIFData,V1=="W")
ABIF = filter(BIFData,V1=="A")
HBIF = filter(BIFData,V1=="H")
OBIF = filter(BIFData,V1=="O")

len = length(BBIF[1,])
boxplot(as.numeric(unlist(BBIF[,2:len])),as.numeric(unlist(WBIF[,2:len])),as.numeric(unlist(ABIF[,2:len])),as.numeric(unlist(HBIF[,2:len])),as.numeric(unlist(OBIF[,2:len])),main="BIFs - Black, White, Asian, Hispanic, Other")
