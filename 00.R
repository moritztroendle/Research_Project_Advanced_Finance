#Hallo Nora wie geht es dir?
  
ts.plot(df$W_v1)
ts.plot(df$W_v2)
ts.plot(df$W_v3)
ts.plot(df$S) #
ts.plot(df$U)
ts.plot(df$P)

#Stationarity Tests and First Differences
adf.test(InputData$W_v3)
W_v6df<-diff(InputData$W_v3,1)
ts.plot(W_v3df)
adf.test(W_v3df)

adf.test(InputData$S)
Sdf<-diff(InputData$S,1)
ts.plot(Sdf)
adf.test(Sdf)

adf.test(na.omit(InputData$U)) #na have to be omitted for the Dicky-Fuller Test
Udf<-diff(InputData$U,1)
ts.plot(Udf)
adf.test(na.omit(Udf))

adf.test(InputData$P)
Pdf<-diff(InputData$P,1)
ts.plot(Pdf)
adf.test(Pdf)


#Logarithmus von den BTC Preisen nehmen
n <- length(InputData$P)
lrpice <- log(InputData$P[-1]/InputData$P[-n])
print(lrpice)

#Decompose Logarithmus Differences Preise
ts3<-ts(data=lrpice, start=c(2013,1,1), end=c(2018,3,31), frequency=365)
ts3
objekt3<-decompose(ts3)
plot(objekt3)

adf.test(lrpice)
pacf(lrpice, lag.max=150)

#noch mal DIfferenz nehmen
lrpice1 <- diff(lrpice)
print(lrpice1)
ts4<-ts(data=lrpice1, start=c(2013,1,1), end=c(2018,3,31), frequency=365)
ts4
objekt4<-decompose(ts4)
plot(objekt4)
acf(lrpice1, lag.max=150)

m3=VARorder(lrpice, Sdf, maxp=200, output=T)
m3


ar1<-ar(na.omit(Pdf, Sdf, W_v3df, Udf), aic=T, order.max = 1, method = "ols", dmean=T, intercept=F)
ar1

##ACF & PACF
acf1<-acf(Pdf, Sdf, W_v3df, Udf, lag.max = NULL, type = c("correlation"), plot= TRUE,
    na.action = na.fail)
pacf1<-pacf(na.omit(Pdf, Sdf, W_v3df, Udf), lag.max = NULL, plot = TRUE, na.action = na.fail)
# PACF darf keine NA's haben


#MARSS
install.packages("MARSS")
library(MARSS)
library("mvtnorm")
install.packages("MTS")
library(MTS)
sig=diag(2) # create the 2-by-2 identity matrix
x=rmvnorm(300,rep(0,2),sig) # generate random draws
par(mfrow=c(5,5))
MTSplot(x) # Obtain time series plots (output not shown)
ccm(x)

y=as.matrix(InputData-InputData$Date-InputData$W_v1)
y=cbind(Pdf,U_v1df,Sdf,Wdf)
MTSplot(y)
ccm(y)
mq(y, lag=1900) #komisch, warum nimmt der Einfluss nicht ab????



lalaBack <- function(X, min, max) {
  X_std = (X - min(X)) / (max(X) - min(X))
  x_scaled = X_std * (max-min)+min
  -  x_scaled <- round(x_scaled, 3)
  + # x_scaled <- round(x_scaled, 3)
    return(x_scaled)
}

Test <-lalaBack(Pdf, min(Pdf), max(Pdf))


library(vars)
var=VARselect(y, lag.max=100)
var
m2=VARorder(y, maxp=400, output=T)
m2
df<-data.frame(matrix(unlist(m2),nrow=100,byrow=T))

df<-do.call(cbind.data.frame, m2)
plot(m2$aic)

acf(P_v1df, lag.max = 300)
jarque.bera.test(Pdf)
jarque.bera.test(Sdf)
jarque.bera.test(Udf)
jarque.bera.test(W_v3df)






