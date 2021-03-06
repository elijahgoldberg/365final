# Akshay Kumar and Elijah Goldberg
# STAT 365

######################################
######## DECLARATIONS ################
######################################

library(ggplot2)
library(gplots)

# Get the index in FRAME that is LAGSEC lagged behind TIME, starting to look from START.  LAGSEC is a difftime class (see as.difftime(x,units="y"))).  Let FRAME be the full data frame we're searching through.
getLagIndex = function(frame,time,lagsec,start) {

	# Create placeholder vectors.
	ftime = frame$time
	last = start
	
	# iterate through the array.
	for (i in (start+1):length(ftime)) {
		
		# If the frame's time is NA, do nothing.  There are many NAs in our data.
		if (!is.na(ftime[i])) {
		  
			
			# If the frame's time is already ahead of the time we want, skip it.  All times are chronological, so once we find a single frame time ahead of our time, we can go straight to NA.
			if (as.double(time - ftime[i],units="secs") > 0) {
			  
				
				# If the frame's time is behind time, keep iterating until we find a time such that the last non-NA time is too far lagged and this time is lagged enough.  Return that time.
				if ((as.double(time - ftime[last],units="secs") > lagsec) & (as.double(time - ftime[i],units="secs") <= as.double(lagsec,units="secs"))) {
				  
					return(i)
				}
				
			# If ever we get past time, return NA because no later frame times will be correct.
			} else {
			  
					return(NA)
			}
				
			# As long as ftime[i] was not NA, we should remember it as the last non-NA time index.
			last = i
		}
	}
	
	# If somehow we reach the end of the array without finding a good time, return NA.
	return(NA)
}

# Let FRAME be the full frame of things we're searching through. Let LAG be a difftime object created by as.difftime(x,units="y")

# ADDED var to getFutureLagged - to select variable from frame
getFutureLagged = function(futures,frame,lag,var) {

	# Create placeholder vectors.
	indepTime = futures$time
	fulag1 = rep(NA,length(indepTime))
	indices = rep(NA,length(indepTime))
	
	# Iterate through the array.
	for (i in 1:length(indepTime)) {
		
		# Let the lagged index for i be given by getLagIndex.  Start the search at one before the last index for which we found something.
		newInd = getLagIndex(frame,indepTime[i],lag,max(2,indices,na.rm=TRUE)-1)
		
		# If no lagged index was found, let index be the value of the last known good index to start from, or let it be 1 if there is no last known good index.
		if (is.na(newInd)) {
			indices[i] = NA
			fulag1[i] = NA
		
		# If we found a lagged index, let fulag[i] be the close price at that index, and let it be known that the next solution will be found near here.
		} else {
			indices[i] = newInd
			fulag1[i] = frame[newInd,var]
		}
	}
	
	return(fulag1)
	
}

# Seems to work.

getStockLagged = function(futures,stocknumber,lag,var) {
	frame = eval(parse(text=paste("stock",stocknumber,sep="")))
	laggedStock = getFutureLagged(futures,frame,lag,var)
	return(laggedStock)
}

a <- getStockLagged(future5.sel,1,as.difftime(5, unit="mins"),"close")
# Gets the best high in FRAME in the past LOOKBACK minutes, where LOOKBACK is a difftime object.
getFutureBestHigh = function(futures,frame,lookback) {
	indepTime = futures$time
	besthigh = rep(NA,length(indepTime))
	indices = rep(NA,length(indepTime))
	
	for (i in 1:length(indepTime)) {
		firstCheck = getLagIndex(frame,indepTime[i],lookback,max(2,indices,na.rm=TRUE)-1)
		potentialList = c()
		
		if (!is.na(firstCheck)) {
			indices[i] = firstCheck
			while (indepTime[i] > frame$time[firstCheck]) {
				potentialList = c(potentialList,frame$high[firstCheck])
				firstCheck = firstCheck + 1
			}
			besthigh[i]=max(potentialList,na.rm=TRUE)
		} else {
			indices[i]=NA
			besthigh[i]=NA
		}
		
	}
	return(besthigh)
}

getFutureWorstLow = function(futures,frame,lookback) {
	indepTime = futures$time
	worstlow = rep(NA,length(indepTime))
	indices = rep(NA,length(indepTime))
	
	for (i in 1:length(indepTime)) {
		firstCheck = getLagIndex(frame,indepTime[i],lookback,max(2,indices,na.rm=TRUE)-1)
		potentialList = c()
		
		if (!is.na(firstCheck)) {
			indices[i] = firstCheck
			while (indepTime[i] > frame$time[firstCheck]) {
				potentialList = c(potentialList,frame$low[firstCheck])
				firstCheck = firstCheck + 1
			}
			worstlow[i]=min(potentialList,na.rm=TRUE)
		} else {
			indices[i]=NA
			worstlow[i]=NA
		}
		
	}
	return(worstlow)
}

getFutureVolLagged = function(futures,frame,lag) {
	indepTime = futures$time
	fulag1 = rep(NA,length(indepTime))
	indices = rep(NA,length(indepTime))
	for (i in 1:length(indepTime)) {
		newInd = getLagIndex(frame,indepTime[i],lag,max(2,indices,na.rm=TRUE)-1)
		if (is.na(newInd)) {
			indices[i] = NA
			fulag1[i] = NA
		} else {
			indices[i] = newInd
			fulag1[i] = frame$volume[newInd]
		}
	}
	
	return(fulag1)
	
}

# Type YEAR gives fraction of the year (0 to 355/365), type MONTH gives week of the month (i.e. 0, 1, 2, 3, or 4), any other type returns fraction of day (0 to 23/24).
getTimeMarker = function(frame,type) {	
	if (type=="year") {
		return(as.numeric(strftime(frame$time,format="%j")) / 365)
	} else if (type=="month") {
		return(as.numeric(strftime(frame$time,format="%W")) %% 4)
	} else {
		return(as.numeric(strftime(frame$time,format="%H")) / 24)
	}	
}

# Where family is either linear or binomial
# Returns either average missclassification or 


glm.cv(evars3, 123, "response ~ .", "binomial", "binomial", 10)

lm.cv = function(data, seed, formula, response, k) {
  args = c(formula=formula)
  res = cv(data=data, seed=seed, k=k, response=response, func="lm", args=args)
  return(res)
}

glm.cv = function(data, seed, formula, response, family, k) {
  args = c(formula=formula, family=family)
  res = cv(data=data, seed=seed, k=k, response=response, func="glm", args=args)
  return(res)
}

library("randomForest")
rf.cv = function(data, seed, formula, response, k) {
  args = c(formula=formula, ntree=500, mtry=8, nodesize=5)
  res = cv(data=data, seed=seed, k=k, response=response, func="randomForest", args=args)
}

cv = function(data, seed, k, response, func, args) {
  data <- data[complete.cases(data),]
  n= nrow(data); p=ncol(data)
  set.seed(seed)
  id <- sample(1:n);data1=data[id,]
  group <- rep(1:k, n/k+1)[1:n]
  
  test.error <- rep(0, k)
  for(i in 1:k) {
    test <- data1[group==i,]
    train <- data1[group!=i,]
    if(func=="randomForest") {
      m <- randomForest(response~.,data=train, ntree=as.numeric(args["ntree"]), mtry=as.numeric(args["mtry"]), nodesize=as.numeric(args["nodesize"]), keep.forest=TRUE)
    }
    else {
      argsWithData <- list(args, data=train)
      m <- do.call(func, argsWithData)
    }
    m.yhat <- predict(m, test)
    nas <- as.numeric(na.action(na.omit(m.yhat)))
    nas.2 <- na.action(na.omit(test$response))
    m.yhat <- m.yhat[-unique(nas,nas.2)]
    test <- test[-unique(nas,nas.2),]
      if(response == "linear") {
        test.error[i] <- sqrt(sum((m.yhat-test$response)^2)/nrow(test)) 
      }
      if(response == "binomial") {
        m.yhat <- as.numeric(m.yhat>.5)
        tab <- table(test$response, m.yhat)
        test.error[i] <- 1-sum(diag(tab))/sum(tab)
      }
  }
  return(mean(test.error))
}


######################################
######## READ IN DATA ################
######################################

# Read data from csv files.  Note that Excel was converted to csv separately.
setwd("/Users/AKumar/Documents/Yale Year Two/Yale Spring 2013/STAT 365/Final Project/Raw Data/")
setwd("C:/Development/Sites/365final/Raw Data/")
for (i in 1:300) {
	eval(parse(text=paste("stock",i," = read.csv(\'",as.character(i),"\',as.is=TRUE)",sep="")))
	eval(parse(text=paste("names(stock",i,") = stock",i,"[2,]",sep="")))
	eval(parse(text=paste("stock",i," = stock",i,"[-(1:2),1:4]",sep="")))
	eval(parse(text=paste("stock",i,"$close = as.numeric(stock",i,"$close)",sep="")))
	eval(parse(text=paste("stock",i,"$volume = as.numeric(stock",i,"$volume)",sep="")))
	eval(parse(text=paste("stock",i,"$amt = as.numeric(stock",i,"$amt)",sep="")))
	eval(parse(text=paste("stock",i,"$time = as.POSIXct(strptime(stock",i,"$time,\'%Y-%m-%d %H:%M\'))",sep="")))
}

future10 = read.csv("future_10min.csv",colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric"),col.names=c('d','t','open','high','low','close','volume'))
future10 = cbind(future10,time=paste(future10$d,future10$t))
future10$time = as.character(future10$time)
future10$time = as.POSIXct(strptime(future10$time,'%m/%d/%Y %I:%M:%S %p'))
future5 = read.csv("future_5min.csv",colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric"),col.names=c('d','t','open','high','low','close','volume','volume2'))
future5 = cbind(future5,time=paste(future5$d,future5$t))
future5$time = as.character(future5$time)
future5$time = as.POSIXct(strptime(future5$time,'%m/%d/%Y %I:%M:%S %p'))

######################################
######### PROCESSING #################
######################################

## Create master data-frames   ##

# Select subset of data to be used
ids <- sample(1:nrow(future5),1000)
future5.sel <- future5[1:1000,]

# Lagged closes
close <- future5.sel$close
lagClose <- getFutureLagged(future5.sel, future5.sel, as.difftime(5, unit="mins"), "close")
lagClose2 <- getFutureLagged(future5.sel, future5.sel, as.difftime(10, unit="mins"), "close")
lagClose3 <- getFutureLagged(future5.sel, future5.sel, as.difftime(15, unit="mins"), "close")
lagClose4 <- getFutureLagged(future5.sel, future5.sel, as.difftime(20, unit="mins"), "close")
lagClose5 <- getFutureLagged(future5.sel, future5.sel, as.difftime(25, unit="mins"), "close")

# First dataframe
ev1 <- data.frame(response=(close-lagClose)>0, predictor=(lagClose-lagClose2)>0)
ev1 <- ev1[complete.cases(ev1),]
ev1$response <- factor(ev1$response)
ev1$predictor <- factor(ev1$predictor)

# Set up test/train
n= nrow(ev1); p=ncol(ev1)
set.seed(123)
id <- sample(1:n);ev1.s=ev1[id,]
group <- rep(1:k, n/k+1)[1:n]
test <- ev1.s[group==i,]; train <- ev1.s[group!=i,]

# Analyze with GLM
t.2 <-train[which(train$predictor==TRUE),]
t.3 <-train[which(train$predictor==FALSE),]
train[train$predictor==TRUE,]

m <- glm(response~predictor, data=train, family=binomial)
m.yhat <- predict(m, test, type="response")
tab <- table(test$response, m.yhat)
1-sum(diag(tab))/sum(tab)
# .4845361 error rate, slightly better than random

# Add more lags
# First dataframe
ev <- data.frame(response=(close-lagClose)>0, (lagClose-lagClose2)>0, (lagClose2-lagClose3)>0, (lagClose3-lagClose4)>0, (lagClose4-lagClose5)>0)
#ev.p <- data.frame((lagClose-lagClose2), (lagClose2-lagClose3))
#y <- (close-lagClose)>0
#y <- y[complete.cases(y)]
#ev.p <- ev.p[complete.cases(ev.p),]
#windows()
#plot(ev.p, col=(y+1))
ev <- ev[complete.cases(ev),]
ev$response <- factor(ev$response)

# Set up test/train
n= nrow(ev); p=ncol(ev)
set.seed(123)
id <- sample(1:n);ev.s=ev[id,]
group <- rep(1:k, n/k+1)[1:n]
test <- ev.s[group==i,]; train <- ev.s[group!=i,]


# Analyze with GLM
m <- glm(response~., data=train, family=binomial)
m.yhat <- predict(m, test, type="response")
tab <- table(test$response, m.yhat)
1-sum(diag(tab))/sum(tab)
train.class <- as.numeric(m$fitted>1/2)
tab <- table(train$response, train.class)
1-sum(diag(tab))/sum(tab)
m.yhat <- predict(m, test)
  tab <- table(test$response, m.yhat)
# .4845361 error rate, slightly better than random



  # 1. eVars for Ft:
  # Ft-1, Fhigh - Fclose at t-1, Flow - Fclose at t-1, max/min high - low over last 30 minutes, max of Fhigh over last 30 minutes - Fclose at t-1, Ft-1 - Ft-2, Ft-1 - Ft-day, Ft-1 - Ft-week, Ft-1 - Ft-2 * vol t-1

future5.sel <- future5[1:1000,]

# Lagged closes
close <- future5.sel$close
lagClose <- getFutureLagged(future5.sel, future5.sel, as.difftime(5, unit="mins"), "close")
lagClose2 <- getFutureLagged(future5.sel, future5.sel, as.difftime(10, unit="mins"), "close")
  response <- close
  fHighSubFClose.tSub1 <- getFutureLagged(future5.sel, future5.sel, as.difftime(10, unit="mins"), "high") - lagClose
  fLowSubFClose.tsub1 <- getFutureLagged(future5.sel, future5.sel, as.difftime(10, unit="mins"), "low") - lagClose
  highSubLow.last30 <- getFutureBestHigh(future5.sel, future5.sel, as.difftime(30, unit="mins")) - getFutureWorstLow(future5.sel, future5.sel, as.difftime(30, unit="mins"))
  maxFHigh.last30.subFClose.tsub1 <- getFutureBestHigh(future5.sel, future5.sel, as.difftime(30, unit="mins")) - lagClose
    f.tsub1.subF.tsub2 <- lagClose - getFutureLagged(future5.sel, future5.sel, as.difftime(15, unit="mins"), "close")
  f.tsub1.subF.tsubDay <- lagClose - getFutureLagged(future5.sel, future5.sel, as.difftime(1440, unit="mins"), "close") # throws error
  f.tsub1.subF.tsubWeek <- lagClose - getFutureLagged(future5.sel, future5.sel, as.difftime(7, unit="days"), "close")
  f.tsub1.f.tsub2.multVol.tsub1 <- (lagClose - getFutureLagged(future5.sel, future5.sel, as.difftime(15, unit="mins"), "close")) * getFutureVolLagged(future5.sel, future5.sel, as.difftime(15, unit="mins"))
    
  evars1 <- data.frame(response, time=future5.sel$time, lagClose, fHighSubFClose.tSub1, fLowSubFClose.tsub1, highSubLow.last30, maxFHigh.last30.subFClose.tsub1, f.tsub1.subF.tsub2, f.tsub1.subF.tsubDay, f.tsub1.subF.tsubWeek, f.tsub1.f.tsub2.multVol.tsub1)
 

  # 2. eVars for Ft - Ft-1:
  # Ft-1, Fhigh - Fclose at t-1, Flow - Fclose at t-1, max/min high - low over last 30 minutes, max of Fhigh over last 30 minutes - Fclose at t-1, Ft-1 - Ft-2, Ft-1 - Ft-day, Ft-1 - Ft-week, Ft-1 - Ft-2 * vol t-1
  
  response <- close - lagClose
  evars2 <- cbind(response = response, evars1[,-1])

  
  # 3. eVars for Ft - Ft-1 > 0:
  # Ft-1, Fhigh - Fclose at t-1, Flow - Fclose at t-1, max/min high - low over last 30 minutes, max of Fhigh over last 30 minutes - Fclose at t-1, Ft-1 - Ft-2, Ft-1 - Ft-day, Ft-1 - Ft-week, Ft-1 - Ft-2 * vol t-1

  evars3 <- cbind(response = (response>0), evars1[,-1])
  evars3 <- evars3[complete.cases(evars3),]

  # 4. eVars for Ft - Ft-1 > 0:
  # All bool >0: Fhigh - Fclose at t-1, Flow - Fclose at t-1, max/min high - low over last 30 minutes, max of Fhigh over last 30 minutes - Fclose at t-1, Ft-1 - Ft-2, Ft-1 - Ft-day, Ft-1 - Ft-week, Ft-1 - Ft-2 * vol t-1

  evars4 <- data.frame(response = (response>0), future5.sel$time, (fHighSubFClose.tSub1>0), (fLowSubFClose.tsub1>0), (highSubLow.last30>0), (maxFHigh.last30.subFClose.tsub1>0), (f.tsub1.subF.tsub2>0), (f.tsub1.subF.tsubDay>0), (f.tsub1.subF.tsubWeek>0), (f.tsub1.f.tsub2.multVol.tsub1>0))
  # Clean variables
  rm(response, lagClose, fHighSubFClose.tSub1, fLowSubFClose.tsub1, highSubLow.last30, maxFHigh.last30.subFClose.tsub1, f.tsub1.subF.tsub2, f.tsub1.subF.tsubDay, f.tsub1.subF.tsubWeek, f.tsub1.f.tsub2.multVol.tsub1)




######################################
######### ANALYSIS ###################
######################################

# EDA
windows()
hist(evars1$response, breaks=30)

# Generate results table
methods.raw <- c("LM", "randomForest", "knn", "lasso", "stepwise")
frames.raw <- c("evars1 (RMSE)", "evars2 (RMSE)")
methods.class <- c("GLM (binomial), CV", "GLM, 50/50", "randomForest, CV", "randomFOrest, 50/50", "LDA, 50/50", "QDA, 50/50", "kNN, CV", "kNN, 50/50", "")
frames.class <- c("evars3 (missclassification)", "evars4 (missclassification)")
res.raw <- data.frame(matrix(data=NA, nrow=length(methods.raw), ncol=length(frames.raw)))
res.class <- data.frame(matrix(data=NA, nrow=length(methods.class), ncol=length(frames.class)))
row.names(res.raw) <- methods.raw; colnames(res.raw) <- frames.raw
row.names(res.class) <- methods.class; colnames(res.class) <- frames.class


# Baseline LM, evar1
res.raw[1,1] <- lm.cv(data=evars1, seed=321, formula="response ~ .", response="linear", k=10)

# Baseline LM, evar2
res.raw[1,2] <- lm.cv(data=evars2, seed=321, formula="response ~ .", response="linear", k=10)

# Baseline GLM, evar3
res.class[1,1] <- glm.cv(data=evars3, 321, formula="response ~ .", family="binomial", response="binomial", k=10)
data <- evars3
n= nrow(data); p=ncol(data)
set.seed(123)
id <- sample(1:n);data1=data[id,]
group <- rep(1:k, n/k+1)[1:n]
data1$response <- factor(data1$response)
test.error <- rep(0, 10)
for(i in 1:k) {
  test <- data1[group==i,]; train <- data1[group!=i,]
  m <- glm(response~.,data=train,family=binomial)
  m.yhat <- predict(m, test)
  m.yhat <- as.numeric(m.yhat>.5)
  tab <- table(test$response, m.yhat)
  test.error[i] <- 1-sum(diag(tab))/sum(tab)
}
res.class[1,1] <- mean(test.error)
res.class

n <- nrow(evars3)
ids <- sample(1:n, n/2)
train <- evars3[ids,]; test <- evars3[-ids,]
m <- glm(response~.,train,family=binomial)
m.yhat <- predict(m, test)
m.yhat <- as.numeric(m.yhat>.5)
tab <- table(test$response, m.yhat)
res.class[2,1] <- 1-sum(diag(tab))/sum(tab)

# Baseline GLM, evar4
res.class[1,2] <- glm.cv(data=evars4, 321, formula="response ~ .", family="binomial",response="binomial", k=10)

# randomForest, evar3
data <- evars3[complete.cases(evars3),]
n= nrow(data); p=ncol(data)
set.seed(seed)
id <- sample(1:n);data1=data[id,]
group <- rep(1:k, n/k+1)[1:n]
data1$response <- factor(data1$response)
test.error <- rep(0, 10)
for(i in 1:k) {
  test <- data1[group==i,]; train <- data1[group!=i,]
  m <- randomForest(response~.,data=train, ntree=500, mtry=10, nodesize=10)
  m.yhat <- predict(m, test)
  tab <- table(test$response, m.yhat)
  test.error[i] <- 1-sum(diag(tab))/sum(tab)
  rm(m,m.yhat,tab,test,train)
  print(i)
}
res.class[3,1] <- mean(test.error)

# randomForest, evar4)
data <- evars4[complete.cases(evars4),]
n= nrow(data); p=ncol(data)
set.seed(seed)
id <- sample(1:n);data1=data[id,]
group <- rep(1:k, n/k+1)[1:n]
data1$response <- factor(data1$response)
test.error <- rep(0, 10)
for(i in 1:k) {
  test <- data1[group==i,]; train <- data1[group!=i,]
  m <- randomForest(response~.,data=train, ntree=500, mtry=10, nodesize=10)
  m.yhat <- predict(m, test)
  tab <- table(test$response, m.yhat)
  test.error[i] <- 1-sum(diag(tab))/sum(tab)
  rm(m,m.yhat,tab,test,train)
  print(i)
}
res.class[3,2] <- mean(test.error)


# evars3, LDA & QDA
data <- evars3[complete.cases(evars3),]
n= nrow(data); p=ncol(data)
set.seed(seed)
id <- sample(1:n);data1=data[id,];data1$response =as.numeric(data1$response)
group <- rep(1:k, n/k+1)[1:n]
nsim <- 100
test.error.lda <- rep(0, nsim)
test.error.qda <- rep(0, nsim)
for(i in 1:nsim) {
  ids <- sample(1:nrow(data), 500)
  test <- data[ids,]; train <- data[-ids,]
  m.lda <- lda(response ~ ., train)
  yhat <- predict(m.lda,test)$class
  test.error.lda[i]=1-sum(diag(table(yhat,test[,1])))/nrow(test)
  m.qda <- qda(response ~ ., train)
  yhat <- predict(m.qda,test)$class
  test.error.qda[i]=1-sum(diag(table(yhat,test[,1])))/nrow(test)
}
res.class[5,1] <- mean(test.error.lda)
res.class[6,1] <- mean(test.error.qda)

# knn, evar3
data <- evars3[complete.cases(evars3),]
n= nrow(data); p=ncol(data)
set.seed(seed)
id <- sample(1:n);data1=data[id,];data1$response =as.numeric(data1$response)
group <- rep(1:k, n/k+1)[1:n]
k.v <- c(1,3,5,7,11,15,21,25,31,35, 45,69,91, 101,125, 151)
test.error <- matrix(0, nrow=10, ncol=length(k.v))
library("class")
for(i in 1:k) {
  test <- data1[group==i,]; train <- data1[group!=i,]
  for(j in 1:length(k.v)) {
    knn.ghat <- knn(train=train[,3:11], test=test[,3:11],cl=as.factor(train[,1]),k=k.v[j])
    test.error[i,j] <- sum(as.factor(test[,1])!= knn.ghat)/nrow(test)
  }
}
err <- colMeans(test.error)
which.min(err)
res.class[7,1] <- min(err)

n <- nrow(evars3)
ids <- sample(1:n, n/2)
test.error <- rep(0, length(k.v))
train <- evars3[ids,]; test <- evars3[-ids,]
for(j in 1:length(k.v)) {
  knn.ghat <- knn(train=train[,3:11], test=test[,3:11],cl=as.factor(train[,1]),k=k.v[j])
  test.error[j] <- sum(as.factor(test[,1])!= knn.ghat)/nrow(test)
}

res.class[8,1] <- min(test.error)
# The goal here is to create a data frame of variables that would theoretically be accessible at a given time point and regress these explanatory variables against the futures price at t.

# After creating this ultimate data frame, we will partition it into a test and training set.  Each row of the data frame represents all the potential predicting variables -- these may include time-dependent things such as slope, raw number, periods since last gain for both futures price movement and stock price movement as well as non-time dependent things.  Note that because this is backward looking, we may have to abandon some early values of futures price for which stock price information in the past is unavailable.

# Regardless, we ought to believe that we can predict the futures price at t=X with all the state variable data available at t=X, that is to say, all information that can be extracted from the historical data from t < X.

# We will fit various regression models on the training set and then attempt to predict the futures price on the test set.  Instead of attempting to build a true trading simulator, we will see if we can adequately predict the futures price at a given time using only the information that would be available at that time -- for these purposes, every data point is functionally independent, because all relevant state information is captured in the explanatory variables at that time-point.  In a sense, we are asking: suppose I train a model on past information.  Can I take this model into the real world, calculate a few relevant quantities using available historical financials right now, and then predict the futures price right now?

# Goal Specifications:

# eVars for Ft - Ft-1:
# Ft-1, Fhigh - Fclose at t-1, Flow - Fclose at t-1, sum of high - low over last 30 minutes, max of Fhigh over last 30 minutes - Fclose at t-1, Ft-1 - Ft-2, Ft-1 - Ft-24, Ft-1 - Ft-week, Ft-1 - Ft-2 * vol t-1

# Another regression with the prior eVars plus:
# weighted stock up-ness: Si_t-1 / sum,i S_t-1 * {did S go up in last interval t-2 to t-1}
# Same as above for longer time windows

# eVars for Ft:
# fraction of year / month / day it currently is,
# Ft-1, Ft-2, Ft-hour, Ft-24h, Ft-wk, Ft-1*vol, (Ft-1 - Ft-2)*vol, Ft-1 - Ft-2



##########################################
######## DESCRIPTIVE STATISTICS ##########
##########################################

pdf("future_price.pdf",width=10,height=7)
print(ggplot() + geom_line(data=future5,aes(x=time,y=close)) + labs(title="Future Price over Time",x="Time",y="Close Price") + theme_classic() + theme(text=element_text(vjust=.25)))
dev.off()

# This code took all night and didn't produce anything.
# pdf("stock_price.pdf",width=10,height=7)
# p = ggplot()
# k=300
# this = "print(p <- p"
# for(i in 1:k) {
	# this = paste(this," + geom_line(data=stock",i,",aes(x=time,y=close,color=rgb(.3,.3,",(i-1)/k,")),size=.4)",sep="")
# }
# this = paste(this," + labs(title='Stock Prices over Time',x='Time',y='Close Price') + theme_classic() + theme(text=element_text(vjust=.25),legend.position='none'))",sep="")
# eval(parse(text=this))
# dev.off()


desiredRange = c(range(stock1$time)[1],range(future5$time)[2])
s = which(future5$time==desiredRange[1])
e = which(future5$time==desiredRange[2])
stocks2 = data.frame(time=future5$time[s:e],close=numeric(length=length(future5$time[s:e])),count=numeric(length = length(future5$time[s:e])))
k=300
for (j in 1:k) {
	this = eval(parse(text=paste("stock",j,sep="")))
	print(j)
	for (i in 1:dim(stocks2)[1]) {
		if (!is.na(stocks2[i,2])) {
			if (length(this$close[which(this$time==stocks2$time[i])]) != 0) {
				stocks2[i,2] = stocks2[i,2] + this$close[which(this$time==stocks2$time[i])][1]
				stocks2$count[i] = stocks2$count[i] + 1
			}
		}
	}
}
stocks2 = data.frame(stocks2,avg = stocks2$close / stocks2$count)
stocks3 = stocks2
stocks3$avg = stocks3$avg*100
noNaN = stocks3[-which(is.na(stocks3$avg)),]

pdf("stock_and_future_4.pdf",width=10,height=7)
print(ggplot() + geom_line(data=future5,aes(x=time,y=close)) + geom_line(data=noNaN,aes(x=time,y=avg),color="red") + labs(title="Future and Stock Prices over Time",x="Time",y="Close Price (Average of Stocks*100)") + xlim(desiredRange) + theme_classic() + theme(text=element_text(vjust=.25))) + 
dev.off()


cors = rep(NA,301)
covs = rep(NA,301)

for (j in 1:300) {
	print(j)
	this = eval(parse(text=paste("stock",j,sep="")))
	thisStock = data.frame(time=future5$time[s:e],closef = future5$close[s:e],closes = numeric(length=length(future5$time[s:e])))
	for (i in 1:dim(thisStock)[1]) {
		if (length(this$close[which(this$time==thisStock$time[i])]) != 0) {
			thisStock[i,3] = this$close[which(this$time==thisStock$time[i])][1]
		} else {
			thisStock[i,3] = NA
		}
	}
	cors[j] = cor(thisStock$closef,thisStock$closes,use="complete.obs")
	covs[j] = var(thisStock$closef,thisStock$closes,na.rm=TRUE)
}
cors[301] = cor(future5$close[s:e],stocks3$avg,use="complete.obs")
covs[301] = var(future5$close[s:e],stocks3$avg,na.rm=TRUE)
corframe = data.frame(index = as.factor(1:300),cors=cors[1:300],covs=covs[1:300])

sortedcov = order(x = abs(covs[1:300]),decreasing=TRUE)
sortedcor = order(x=abs(cors[1:300]),decreasing=TRUE)
both = data.frame(Covariances = sortedcov, Correlations = sortedcor)

pdf("top correlates.pdf")
textplot(both[1:15,],show.rownames=FALSE,mar=c(0.5,0.5,0.5,0.5))
dev.off()

pdf("correlations_stock_future.pdf")
print(ggplot() + geom_bar(data=corframe,aes(x=index,y=cors)) + labs(x='Stock Number',y='Correlation with Future Price',title='Correlations between Futures and Stocks') + geom_hline(aes(yintercept=cors[301],color="red")) + theme_classic() + theme(legend.position='none',text=element_text(vjust=.25),axis.text.x=element_text(size=0),axis.ticks.x = element_line(size=0)) + ylim(-1,1))
dev.off()

pdf("covariances_stock_future.pdf")
print(ggplot() + geom_bar(data=corframe,aes(x=index,y=covs)) + labs(x='Stock Number',y='Covariance with Future Price',title='Covariance between Futures and Stocks') + geom_hline(aes(yintercept=var(future5$close[s:e],stocks2$avg,na.rm=TRUE),color="red")) + theme_classic() + theme(legend.position='none',text=element_text(vjust=.25),axis.text.x=element_text(size=0),axis.ticks.x = element_line(size=0)))
dev.off()





# Independent variables
# Current close price
# Change in close price
# Positive change in close price

# Dependent variables - futures
# F(t-1) = last close of F not equal
# Upside5 = Fhigh-Fclose at t-1
# Downside5 = Flow - Fclose at t-1
# Relative upside = sum of 30: Fhigh - Flow
# Downside/upside T = max(lastT hi/low) - Fclose(t-1)
# Ft-1 - Ft-2 = slope of future
# ft-1 - Ft-24hr = "medium" term trend
# Ft-1 = Ft-week = "long" term trend
# (Ft-1 - Ft-2)*Vol(Ft-1)

# Dependent varialbes - stocks
# All F factors
# Average S(t-1) - S(t-2) > 0
# Weighted by S(t-1)/sum(S(t-1))
# Different change

price = 0
for(i in 1:299) {
  sname <- paste("stock", i, sep="")
  price <- price + eval(parse(text=sname))[1000,"close"]
  print(price)
  print(sname)
}

# Stocks without NAs
stocks = 0
for(i in 1:299) {
  sname <- paste("stock", i, sep="")
  if(sum(is.na(eval(parse(text=sname))[,"close"])) == 0) {
    stocks = stocks + 1
  }
  print(sname)
  print(stocks)
}

# function getLagIndex(frame, time, lag(seconds), start)
  # Look in frame$time at start index
  # find the index # s.t.
    # frameTime[i-1] - time < lagSec
    # frameTime[i] - >= lagSec
    # frameTime[i] - time < 0
  # else NA

# fucntion F[getLagIndex(F, 1, 5min, 0)]
  # Next, F[getLagIndex(..., start=lastLagIndex)]

# functions forEachExplanVar()
  # create a T length vector
    # Call get LagIndexFrom here
    # where T = number of F times
  # return that vector

# Actual runtime
  # Call all explanvar functions
  # Insert results into master frame