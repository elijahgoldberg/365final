# Akshay Kumar and Elijah Goldberg
# STAT 365

######################################
######## DECLARATIONS ################
######################################

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
getFutureLagged = function(futures,frame,lag) {

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
			indices[i] = max(1,indices,na.rm=TRUE)
			fulag1[i] = NA
		
		# If we found a lagged index, let fulag[i] be the close price at that index, and let it be known that the next solution will be found near here.
		} else {
			indices[i] = newInd
			fulag1[i] = frame$close[newInd]
		}
	}
	
	return(fulag1)
	
}

# Seems to work.

getStockLagged = function(futures,stocknumber,lag) {
	frame = eval(parse(text=paste("stock",stocknumber,sep="")))
	laggedStock = getFutureLagged(futures,frame,lag)
	return(laggedStock)
}

######################################
######## READ IN DATA ################
######################################

# Read data from csv files.  Note that Excel was converted to csv separately.
setwd("/Users/AKumar/Documents/Yale Year Two/Yale Spring 2013/STAT 365/Final Project/Raw Data/")
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