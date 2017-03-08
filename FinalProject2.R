require(class)
require(MASS)
rm(list=ls())
mydata = read.csv("YelpBusinessDataSlimmed.csv")  # read csv file 
dim(mydata)
set.seed(5072)

#####################
######Model 1########
#####################

#Below is a multiple regression testing how accepting credit cards, having coat checks, having TVs, Outdoor seating
#and price range impact the number of stars a restaurant recieves (rating utilized as an operational definition of a
#resturant's value)
new5<-mydata[,c(1,10,19,23,24,34,36,31)]
sum(is.na(new5$attributes_Accepts.Credit.Cards))

#Ommiting rows that are missing information or have Nulls.
new5[new5 == ""] <- NA
newer=na.omit(new5)
dim(newer)
AcceptsCreditCards <-newer$attributes_Accepts.Credit.Cards
CoatCheck <-newer$attributes_Coat.Check
HasTV <-newer$attributes_Has.TV
OutDoorSeating <-newer$attributes_Outdoor.Seating
PriceRange <- newer$attributes_Price.Range
Stars <- newer$stars


lm.fit<-lm(Stars~AcceptsCreditCards+CoatCheck+HasTV+OutDoorSeating+PriceRange,data=newer)
summary(lm.fit)

#Initializing a list of Rsq. values for comparison
RSq <- numeric(0)
RSq[1] <- summary(lm.fit)$r.squared
AdjRsq <- numeric(0)
AdjRsq[1] <- summary(lm.fit)$adj.r.squared
MSEs <- numeric(0)
MSEs[1] <- mean(lm.fit$residuals^2)



####################
######Model 2#######
####################

#In the modern tech erra, having WiFi in a restraunt is generally considered important, so we tested if there was
# a significant difference in star ratings when WiFi does not exist (no - 1), is must be paid for (paid - 2), or
#if the restaurant offers it for free 


#Question on Wifi
wifi<-mydata[,c(30,36)]
wifi[wifi == ""] <- NA
newwifi=na.omit(wifi)

#Reordering the levels of the wifi variable in order to more resemble a ranked categorical variable
newwifi$attributes_Wi.Fi <- factor(newwifi$attributes_Wi.Fi, levels=c("no","paid","free"))

factorwifi <- sapply(newwifi, is.factor)
newwifi[,factorwifi] <- as.numeric(as.factor(newwifi[,factorwifi]))

lm.fitWiFi <- lm(stars~., data = newwifi)
summary(lm.fitWiFi)

  #WiFi in restaurants does appear to have a significant impact on the stars in reviews

#Adding the RSq. and MSE values to the list for ease of comparison
RSq[2] <- summary(lm.fitWiFi)$r.squared
AdjRsq[2] <- summary(lm.fitWiFi)$adj.r.squared
MSEs[2] <- mean(lm.fitWiFi$residuals^2)



####################
#####Model 3########
####################

#Question regarding how predictors with and without different levels service impact ratings.
#select predictors: accept credit cards (T/F), Alcohol (3 levels, 1-none, 2-beer & wine, 3-full bar), Has Tv (T/F), 
# Noise Level (4 levels, 1- quiet, 2-average, 3-loud, 4-very loud), Outdoor Seating (T/F), Price Range (int - 1-5), 
#Waiter Service (T/F), WiFi (3 levels - 1-no, 2-paid, 3-free)
#For true false, they are binaries with True = 1
#Y variable: stars (rating 0-5)

secondFrame<-mydata[,c(1,4,19,20,23,24,28,30,36)]
secondFrame[secondFrame == ""] <- NA
MultipleReg=na.omit(secondFrame)
dim(MultipleReg)

#Fix the levels of the variables that are factors so there are reasonable rankings
MultipleReg$attributes_Alcohol <- factor(MultipleReg$attributes_Alcohol, levels=c("none","beer_and_wine","full_bar"))
MultipleReg$attributes_Noise.Level <- factor(MultipleReg$attributes_Noise.Level, levels=c("quiet","average","loud","very_loud"))
MultipleReg$attributes_Wi.Fi <- factor(MultipleReg$attributes_Wi.Fi, levels=c("no","paid","free"))

#converting the columns in the data frame into numeric or binary ints so the linear regression sees levels
factorCols <- sapply(MultipleReg, is.factor)
MultipleReg[factorCols] <- sapply(MultipleReg[factorCols], as.numeric)

logicCols <- sapply(MultipleReg, is.logical)
MultipleReg[logicCols] <- sapply(MultipleReg[logicCols], as.integer)

str(MultipleReg)

#Testing relationships with Multiple linear regression

lm.fitMult <- lm(stars~., data = MultipleReg)
summary(lm.fitMult)

#Adding the RSq. and MSE values to the list for ease of comparison
RSq[3] <- summary(lm.fitMult)$r.squared
AdjRsq[3] <- summary(lm.fitMult)$adj.r.squared
MSEs[3] <- mean(lm.fitMult$residuals^2)


###################
######Model 4######
###################

#Adding the attribute regarding coat check as it may be correlated to outdoor seating as the presence of both may 
#indicate cold weather rather than anything significant about ambiance

thirdFrame<-mydata[,c(1,4,10,19,20,23,24,28,30,36)]
thirdFrame[thirdFrame == ""] <- NA
MultipleReg2=na.omit(thirdFrame)
dim(MultipleReg2)

MultipleReg2$attributes_Alcohol <- factor(MultipleReg2$attributes_Alcohol, levels=c("none","beer_and_wine","full_bar"))
MultipleReg2$attributes_Noise.Level <- factor(MultipleReg2$attributes_Noise.Level, levels=c("quiet","average","loud","very_loud"))
MultipleReg2$attributes_Wi.Fi <- factor(MultipleReg2$attributes_Wi.Fi, levels=c("no","paid","free"))

factorCols2 <- sapply(MultipleReg2, is.factor)
MultipleReg2[factorCols2] <- sapply(MultipleReg2[factorCols2], as.numeric)

logicCols2 <- sapply(MultipleReg2, is.logical)
MultipleReg2[logicCols2] <- sapply(MultipleReg2[logicCols2], as.integer)

lm.fitMult2 <- lm(stars~., data = MultipleReg2)
summary(lm.fitMult2)

RSq[4] <- summary(lm.fitMult2)$r.squared
AdjRsq[4] <- summary(lm.fitMult2)$adj.r.squared
MSEs[4] <- mean(lm.fitMult2$residuals^2)

#Testing the corrolation between outdoor seating and coat check as the presence of both may 
#indicate cold weather rather than anything significant about ambiance

cor.test(MultipleReg2$attributes_Coat.Check, MultipleReg2$attributes_Outdoor.Seating)

#Testing the correlation between coat check and price range as the interacion of the two may have caused
#price range to loose significance
cor.test(MultipleReg2$attributes_Coat.Check, MultipleReg2$attributes_Price.Range)
  #As they are highly correlated it is likely that the presence of the coat check attribute contributed heavily
  #to the loss in significance of price range. We posit that the presence of a coat check is indicative of a more
  #formal restaurant in colder weather, therefore the restaurant would likely have a higher price range than a 
  #restaurant lacking a coat check and as coat checks are only needed if it is cold, outdoor seating would then
  #be impractical.

#################
#####Model 5#####
#################

#Add state to the model as it seems that the loss of significance in outdoor seating when coat check was added
#may have to do with coat checks being primarily in colder evironments where outdoor seating is not as desired

fourthFrame<-mydata[,c(1,4,10,19,20,23,24,28,30,36,37)]
fourthFrame[fourthFrame == ""] <- NA
MultipleReg3=na.omit(fourthFrame)
dim(MultipleReg3)

MultipleReg3$attributes_Alcohol <- factor(MultipleReg3$attributes_Alcohol, levels=c("none","beer_and_wine","full_bar"))
MultipleReg3$attributes_Noise.Level <- factor(MultipleReg3$attributes_Noise.Level, levels=c("quiet","average","loud","very_loud"))
MultipleReg3$attributes_Wi.Fi <- factor(MultipleReg3$attributes_Wi.Fi, levels=c("no","paid","free"))

factorCols3 <- sapply(MultipleReg3, is.factor)
MultipleReg3[factorCols3] <- sapply(MultipleReg3[factorCols3], as.numeric)

logicCols3 <- sapply(MultipleReg3, is.logical)
MultipleReg3[logicCols3] <- sapply(MultipleReg3[logicCols3], as.integer)

str(MultipleReg3)

lm.fitMult3 <- lm(stars~., data = MultipleReg3)
summary(lm.fitMult3)

RSq[5] <- summary(lm.fitMult3)$r.squared
AdjRsq[5] <- summary(lm.fitMult3)$adj.r.squared
MSEs[5] <- mean(lm.fitMult3$residuals^2)


###################
######Model 6######
###################
#Remove coat check to see if that makes state significant
lm.fitMult4 <- lm(stars~.-attributes_Coat.Check, data = MultipleReg3)
summary(lm.fitMult4)

RSq[6] <- summary(lm.fitMult4)$r.squared
AdjRsq[6] <- summary(lm.fitMult4)$adj.r.squared
MSEs[6] <- mean(lm.fitMult4$residuals^2)


###################
######Model 7######
###################

#Splitting the data into regions (North and South) to see if the temperature of the locations impacts ratings
#given to each restaurant

fifthFrame<-mydata[,c(10,36,37)]
dim(fifthFrame)
fifthFrame[fifthFrame == ""] <- NA
sixthFrame=na.omit(fifthFrame)
dim(sixthFrame)
North=c("AK","ON","WI","PA","MN","IL","BW","EDH","ELN","FIF","HAM","KHL","MLN","NTH","QC","SCB","RP","NW","XGL")
sixthFrame$Northern <- match(sixthFrame$state,North)
sixthFrame$region=0
sixthFrame$region[which(sixthFrame$Northern != "NA")] <- 1
sixthFrame$Northern <- NULL

cor(sixthFrame$region, sixthFrame$attributes_Coat.Check)
  #This correlation is saying that there is a small correlation between being in the North and having a coat check.
  #so there may be a weather element as it would hardly make sense to have a coat check in Arizona

logicCols4 <- sapply(sixthFrame, is.logical)
sixthFrame[logicCols4] <- sapply(sixthFrame[logicCols4], as.integer)

lm.fitRegion<- lm(stars~attributes_Coat.Check+region, data=sixthFrame)
summary(lm.fitRegion)

RSq[7] <- summary(lm.fitRegion)$r.squared
AdjRsq[7] <- summary(lm.fitRegion)$adj.r.squared
MSEs[7] <- mean(lm.fitRegion$residuals^2)

####################
#####Model 8########
####################

# This regression shows that having a a coat check has no significant effect on your reviews, but that being in the North does. 
#by testing the interaction effect between coat check and region
lm.fitInteration<-lm(stars ~ attributes_Coat.Check * region, data=sixthFrame)
summary(lm.fitInteration)

RSq[8] <- summary(lm.fitRegion)$r.squared
AdjRsq[8] <- summary(lm.fitRegion)$adj.r.squared
MSEs[8] <- mean(lm.fitRegion$residuals^2)


###################
######Model 9######
###################
#Adding the Region variable to the wider model (adding the rest of the variables in)
seventhFrame<-mydata[,c(1,4,10,19,20,23,24,28,30,36,37)]
names(seventhFrame)
seventhFrame[seventhFrame == ""] <- NA
eighthFrame=na.omit(seventhFrame)
dim(eighthFrame)
eighthFrame$Northern <- match(eighthFrame$state,North)
eighthFrame$region=0
eighthFrame$region[which(eighthFrame$Northern != "NA")] <- 1
eighthFrame$Northern <- NULL
eighthFrame$state <- NULL

eighthFrame$attributes_Alcohol <- factor(eighthFrame$attributes_Alcohol, levels=c("none","beer_and_wine","full_bar"))
eighthFrame$attributes_Noise.Level <- factor(eighthFrame$attributes_Noise.Level, levels=c("quiet","average","loud","very_loud"))
eighthFrame$attributes_Wi.Fi <- factor(eighthFrame$attributes_Wi.Fi, levels=c("no","paid","free"))

factorCols8 <- sapply(eighthFrame, is.factor)
eighthFrame[factorCols8] <- sapply(eighthFrame[factorCols8], as.numeric)

logicCols8 <- sapply(eighthFrame, is.logical)
eighthFrame[logicCols8] <- sapply(eighthFrame[logicCols8], as.integer)

lm.fitRegionFull<- lm(stars~., data=eighthFrame)
summary(lm.fitRegionFull)

RSq[9] <- summary(lm.fitRegionFull)$r.squared
AdjRsq[9] <- summary(lm.fitRegionFull)$adj.r.squared
MSEs[9] <- mean(lm.fitRegionFull$residuals^2)


##############
###Matrix of the R squared values and MSEs of all models above to compare the fit of the models
FitMatrix <-  matrix(c(RSq, AdjRsq, MSEs), nrow=9, ncol=3, dimnames=list(c("Initial Coat","WiFi", "Initial Out.","Add Coat", "Add St.", "Remv. Coat", "Region", "Interaction","Full Region"), c("Rsq", "Adj. Rq.", "MSE")))
FitMatrix
##############


#Best subset selection of the model above with the lowest error and highest R squared values (Model 9 - Full region)
require(leaps)
regfit.full <- regsubsets(stars~.,eighthFrame,nvmax=10)
reg.summary <- summary(regfit.full)
names(reg.summary)

# Plotting RSS, adjusted R2, Cp, and BIC to test the best subset model
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
(max<-which.max(reg.summary$adjr2))
points(max,reg.summary$adjr2[max], col="red",cex=2,pch=20)

# Plotting the Cp and BIC statistics, and indicate the models with the smallest statistic using which.min().
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
(min<-which.min(reg.summary$cp))
points(min,reg.summary$cp[min],col="red",cex=2,pch=20)
(min<-which.min(reg.summary$bic))
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(min,reg.summary$bic[min],col="red",cex=2,pch=20)

# Plot command to display the selected variables for the best model with a given number of predictors, ranked 
# according to the BIC, Cp, adjusted R2, or AIC.
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

#Use the coef() function to get the coefficient estimates for the model with the lowest BIC score (5 in this case)
coef(regfit.full,5)
par(mfrow=c(1,1))

####################
######Model 10######
####################

#Test the model with the chosen coefficients from the Best Subset Selection Model
lm.fitBest <- lm(stars~attributes_Alcohol+attributes_Has.TV+attributes_Noise.Level+attributes_Wi.Fi+region, data=eighthFrame)
summary(lm.fitBest)

RSq2<- append(RSq, summary(lm.fitBest)$r.squared)
AdjRsq2 <- append(AdjRsq, summary(lm.fitBest)$adj.r.squared)
MSEs2 <- append(MSEs, mean(lm.fitBest$residuals^2))

FitMatrix2 <-  matrix(c(RSq2, AdjRsq2, MSEs2), nrow=10, ncol=3, dimnames=list(c("Initial Coat","WiFi", "Initial Out.","Add Coat", "Add St.", "Remv. Coat", "Region", "Interaction","Full Region", "Best Subset"), c("Rsq", "Adj. Rq.", "MSE")))
FitMatrix2
  #Almost as good as the full eightFrame data set from which the best subset was selected

##Confusion matrix and Error Testing
# Split the star variable into an 80/20 binary where 1 means that the variable is 80th percentile or above and 0
# is below so as to use training and test groups to test how accurate various approaches are in approaching the
# true relationship between factors

eighth <- quantile(eighthFrame$stars, .80)
starsBin <- ifelse(eighthFrame$stars >= eighth, 1, 0)
binReg <- data.frame(eighthFrame, starsBin)
binReg$stars <- NULL

trainindices <- sample(1:nrow(binReg), 0.8*nrow(binReg))
testindices<-setdiff(1:nrow(binReg),trainindices)         
train<-binReg[trainindices,]
test<-binReg[testindices,]

glm.fitBin<-glm(starsBin~.,data=binReg,family=binomial,subset=trainindices)
summary(glm.fitBin)

#For the test set, use the model to display a confusion matrix wih below-median mpg as the null hypothesis, 
#and compute the same five performance statistics
glm.probs<-predict(glm.fitBin, binReg[testindices,],type="response")
glm.pred<-rep("Below",length(testindices))
glm.pred[glm.probs>.5]<-"Above"
predicted<-factor(glm.pred, ordered=T, levels=c('Below','Above'))
glmTable<-table(test$starsBin, predicted)
rownames(glmTable) <- c("Below", "Above")
glmTable

#Compute fraction of correct predictions, overall error rate, type I and type II error rate, Power and Precision 
CorrPred <- numeric(0)
CorrPred[1]<-(glmTable[1,1]+glmTable[2,2])/sum(glmTable)
OverErr <- numeric(0)
OverErr[1] <- (glmTable[2,1]+glmTable[1,2])/sum(glmTable)
Type1 <- numeric(0)
Type1[1] <- (glmTable[1,2]/sum(glmTable[1,]))
Type2 <- numeric(0)
Type2[1] <- (glmTable[2,1]/sum(glmTable[2,]))
Power <- numeric(0)
Power[1] <- 1-(glmTable[2,1]/sum(glmTable[2,]))
Prec <- numeric(0)
Prec[1]<-glmTable[2,2]/sum(glmTable[,2])

#Repeat training model and confusion matrix using LDA
require(MASS)
lda.fitBin<-lda(starsBin~.,data=binReg,subset=trainindices)
lda.fitBin

lda.pred<-predict(lda.fitBin, test)
ldaTable<-table(test$starsBin,lda.pred$class)
rownames(ldaTable) <- c("Below", "Above")
colnames(ldaTable) <- c("Below", "Above")
ldaTable

#Error Rates
CorrPred[2]<-(ldaTable[1,1]+ldaTable[2,2])/sum(ldaTable)
OverErr[2] <- (ldaTable[2,1]+ldaTable[1,2])/sum(ldaTable)
Type1[2] <- (ldaTable[1,2]/sum(ldaTable[1,]))
Type2[2] <- (ldaTable[2,1]/sum(ldaTable[2,]))
Power[2] <- 1-(ldaTable[2,1]/sum(ldaTable[2,]))
Prec[2]<-ldaTable[2,2]/sum(ldaTable[,2])

#Repeat with QDA
qda.fitBin<-qda(starsBin~.,data=binReg,subset=trainindices)
qda.fitBin

qda.pred<-predict(qda.fitBin, test)
qdaTable<-table(test$starsBin, qda.pred$class)
rownames(qdaTable) <- c("Below", "Above")
colnames(qdaTable) <- c("Below", "Above")
qdaTable

CorrPred[3]<-(qdaTable[1,1]+qdaTable[2,2])/sum(qdaTable)
OverErr[3] <- (qdaTable[2,1]+qdaTable[1,2])/sum(qdaTable)
Type1[3] <- (qdaTable[1,2]/sum(qdaTable[1,]))
Type2[3] <- (qdaTable[2,1]/sum(qdaTable[2,]))
Power[3] <- 1-(qdaTable[2,1]/sum(qdaTable[2,]))
Prec[3]<-qdaTable[2,2]/sum(qdaTable[,2])

#Repeat with KNN, k=(1,5)

train.X<- train[,-11]
test.X <- test[,-11]
train.Y<- train$starsBin

ksequence <- seq(1,5)
for(k in ksequence) {
  knn.model <- knn(train.X, test.X, train.Y, k=k)
  knnTable<-table(test$starsBin, knn.model)
  knnTable
  CorrPred[3+k] <- (knnTable[1,1]+knnTable[2,2])/sum(knnTable)
  OverErr[3+k] <- (knnTable[2,1]+knnTable[1,2])/sum(knnTable)
  Type1[3+k] <- (knnTable[1,2]/sum(knnTable[1,]))
  Type2[3+k] <- (knnTable[2,1]/sum(knnTable[2,]))
  Power[3+k] <-  1-(knnTable[2,1]/sum(knnTable[2,]))
  Prec[3+k] <- knnTable[2,2]/sum(knnTable[,2])
}

ErrorMatrix = matrix(c(CorrPred, OverErr, Type1, Type2, Power, Prec), nrow=8, ncol=6, dimnames=list(c("Log.","LDA","QDA","K=1","K=2","K=3","K=4","K=5"), c("Corr Pred", "Over. Err.", "Type 1", "Type 2", "Power", "Prec")))
ErrorMatrix

  #The KNN model where k=3 seem to have the lowest overall error and highest rate of correct predictions
  #Though it does sometimes change with different random samples

