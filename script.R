# Akshay Kumar and Elijah Goldberg
# STAT 365

######################################
######## DECLARATIONS ################
######################################


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


# Some extraneous code.
# name = "stock1"
# binder = "stock1"
# for (i in 2:300) {
	# name = c(name,paste("stock",i,sep=""))
	# binder = paste(binder,",","stock",i,sep="")
# }


######################################
######### PROCESSING #################
######################################

# The goal here is to create a data frame of variables that would theoretically be accessible at a given time point and regress these explanatory variables against the futures price at t.

# After creating this ultimate data frame, we will partition it into a test and training set.  Each row of the data frame represents all the potential predicting variables -- these may include time-dependent things such as slope, raw number, periods since last gain for both futures price movement and stock price movement as well as non-time dependent things.  Note that because this is backward looking, we may have to abandon some early values of futures price for which stock price information in the past is unavailable.

# Regardless, we ought to believe that we can predict the futures price at t=X with all the state variable data available at t=X, that is to say, all information that can be extracted from the historical data from t < X.

# We will fit various regression models on the training set and then attempt to predict the futures price on the test set.  Instead of attempting to build a true trading simulator, we will see if we can adequately predict the futures price at a given time using only the information that would be available at that time -- for these purposes, every data point is functionally independent, because all relevant state information is captured in the explanatory variables at that time-point.  In a sense, we are asking: suppose I train a model on past information.  Can I take this model into the real world, calculate a few relevant quantities using available historical financials right now, and then predict the futures price right now?