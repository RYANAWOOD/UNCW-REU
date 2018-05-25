rm(list=ls())
dev.off()
DData = read.csv("/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 2-May 23, 2018/morphII_dirty.csv",header=TRUE);
CData = read.csv("/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 2-May 23, 2018/morphII_clean.csv",header=TRUE);
BIFData = read.csv("/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 1-May 22, 2018/MorphII_BIF_s7-37_g0.1_max_partial.csv",header = F);

#######################################################################
### Part 1: Regression of Age from First 20 BIFs ######################
#######################################################################

# import ID vector and min age vector
IDs = read.csv('/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 2-May 23, 2018/IDs.csv')
IDs = unlist(IDs$x)
minAges = read.csv('/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 2-May 23, 2018/minAges.csv')
minAges = unlist(minAges$x)

# take only first 20 BIFs
BIFData1 = BIFData[,1:21]
# extract the IDs
BIFData1$V1 = as.character(BIFData1$V1)
for (ii in 1:length(BIFData[,1])) {
  BIFData1$V1[ii] = substr(as.character(BIFData1[ii,1]),1,6)
}
# make vector with min ages corresponding to BIF data
BIFages = unlist(as.numeric(BIFData1$V1))
for (ii in 1:length(BIFages)) {
  tmpIndex = match(BIFages[ii],IDs)
  BIFages[ii] = minAges[tmpIndex]
}

### linear regression
attach(BIFData1[,2:length(BIFData1[1,])])
lm.fit = lm(BIFages~V2)
summary(lm.fit)
plot(V2,BIFages,main='Linear Regression of First 20 BIFs')
abline(lm.fit$coefficients[1],lm.fit$coefficients[2])

### quadratic regression
lm.fit2 = lm(BIFages~poly(V3,2))
summary(lm.fit2)
plot(V3,BIFages,main='Quadratic Regression of First 20 BIFs')
x = seq(0,255,length=100)
lines(x,predict(lm.fit2,data.frame(V3=x)),col="blue")

## polynomial regression
lm.fit3 = lm(BIFages~poly(V3,3))
summary(lm.fit3)
plot(V3,BIFages,main='Cubic Regression of First 20 BIFs')
x = seq(0,255,length=100)
lines(x,predict(lm.fit3,data.frame(V3=x)),col="red")


#######################################################################
### Part 2: Classification of Gender and Race from Full BIF Dataset ###
#######################################################################
rm(list=ls())
BIFData = read.csv("/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 1-May 22, 2018/MorphII_BIF_s7-37_g0.1_max_partial.csv",header = F);

# import ID vector
IDs = read.csv('/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 2-May 23, 2018/IDs.csv')
IDs = unlist(IDs$x)

# import gender vector
genders = read.csv('/Users/ryanwood/Documents/Summer 2018 - UNCW /Project 2-May 23, 2018/genders.csv')
genders = genders$x

# extract the IDs
BIFData$V1 = as.character(BIFData$V1)
for (ii in 1:length(BIFData[,1])) {
  BIFData$V1[ii] = substr(as.character(BIFData[ii,1]),1,6)
}

# make vector with genders corresponding to BIF data
BIFgenders = unlist(as.numeric(BIFData$V1))
tmpgenders = rep("M",length(BIFgenders))
for (ii in 1:length(BIFgenders)) {
  tmpIndex = match(BIFgenders[ii],IDs)
  if (genders[tmpIndex] == "F") {
    tmpgenders[ii] = "F"
  }
}
BIFgenders = tmpgenders
rm("tmpgenders","ii","genders","tmpIndex")

# make labels vector for gender, 1 for female and 0 for male
genderLabels = rep(0,length(BIFgenders))
for (ii in 1:length(genderLabels)) {
  if (BIFgenders[ii] == "F") {
    genderLabels[ii] = 1
  }
}

### Logistic Regression

# train the model
glm.fit=glm(genderLabels~.-V1,data=BIFData[,1:150],family=binomial)

# predict using training data
glm.predictions = predict(glm.fit,type="response")
glm.predictions[glm.predictions<0.5] = 0
glm.predictions[glm.predictions>=0.5] = 1

# confusion table
confusionTable = table(glm.predictions,genderLabels)
confusionTable

# overall accuracy
accuracy=mean(genderLabels==glm.predictions)

# sensitivity (portion of 1's correctly predicted)
sensitivity=confusionTable[4]/(confusionTable[4]+confusionTable[3])

# specificity (portion of 0's correctly predicted)
specificity=confusionTable[1]/(confusionTable[1]+confusionTable[2])

### LDA

# train model
lda.fit=lda(genderLabels~.-V2,data=BIFData[,2:150])

# plot the model
plot(lda.fit)

# predict using training data
lda.predictions = predict(lda.fit,BIFData[,2:150],type="response")
lda.predictions = lda.predictions$class

# confusion table
confusionTable = table(lda.predictions,genderLabels)
confusionTable

# overall accuracy
accuracy=mean(genderLabels==lda.predictions)

# sensitivity (portion of 1's correctly predicted)
sensitivity=confusionTable[4]/(confusionTable[4]+confusionTable[3])

# specificity (portion of 0's correctly predicted)
specificity=confusionTable[1]/(confusionTable[1]+confusionTable[2])


### QDA

# train model
qda.fit=qda(genderLabels~.-V2,data=BIFData[,2:50])

# plot the model
plot(qda.fit)

# predict using training data
qda.predictions = predict(qda.fit,BIFData[,2:50],type="response")
qda.predictions = qda.predictions$class

# confusion table
confusionTable = table(qda.predictions,genderLabels)
confusionTable

# overall accuracy
accuracy=mean(genderLabels==qda.predictions)

# sensitivity (portion of 1's correctly predicted)
sensitivity=confusionTable[4]/(confusionTable[4]+confusionTable[3])

# specificity (portion of 0's correctly predicted)
specificity=confusionTable[1]/(confusionTable[1]+confusionTable[2])



### KNN

# train/predict model
knn.predictions = knn(BIFData[,2:150],BIFData[,2:150],genderLabels,k=3)

# confusion table
confusionTable = table(knn.predictions,genderLabels)
confusionTable

# overall accuracy
accuracy=mean(genderLabels==knn.predictions)

# sensitivity (portion of 1's correctly predicted)
sensitivity=confusionTable[4]/(confusionTable[4]+confusionTable[3])

# specificity (portion of 0's correctly predicted)
specificity=confusionTable[1]/(confusionTable[1]+confusionTable[2])

# test






